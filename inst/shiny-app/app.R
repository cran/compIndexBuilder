# ================================================================
# Composite Index Builder & Analytics Platform - Version 2.0.0
# Features:
#   * CSV + Excel input
#   * Multi-sheet Excel loading with workbook-wide active-sheet selector
#   * Refresh workbook sheet list + load/refresh active sheet
#   * Individual download for every loaded sheet (+ ZIP all sheets)
#   * Column role detection and mapping
#   * Missing-data handling and normalization
#   * Mixed indicator directions
#   * Equal/custom weighting
#   * Entity-level ranking and weight-impact analysis
#   * Time series, forecasting, comparison
#   * Pillar/sub-index construction with Equal/Custom/Correlation/PCA weights
#   * Diagnostics: Cronbach alpha, CV, PCA, sensitivity, correlation heatmap, Sankey
# ================================================================

# ---- Required packages -------------------------------------------------------
required_packages <- c(
  "shiny", "shinydashboard", "DT", "plotly", "ggplot2", "dplyr",
  "readxl", "forecast", "tidyr", "networkD3", "psych", "corrplot",
  "missForest", "zoo", "jsonlite"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running the app: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(readxl)
library(forecast)
library(tidyr)
library(networkD3)
library(psych)
library(corrplot)
library(missForest)
library(zoo)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- General helpers ---------------------------------------------------------
safe_id <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  ifelse(grepl("^[0-9]", x), paste0("x_", x), x)
}

safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]", "_", as.character(x))
  gsub("_+", "_", x)
}

normalize_weights <- function(w) {
  w <- as.numeric(w)
  w[!is.finite(w) | w < 0] <- 0
  if (length(w) == 0 || sum(w) <= 0) {
    return(rep(1 / max(length(w), 1), length(w)))
  }
  w / sum(w)
}

minmax_scale <- function(x) {
  x <- as.numeric(x)
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    out <- rep(0, length(x))
    out[is.na(x)] <- NA_real_
    return(out)
  }
  (x - rng[1]) / diff(rng)
}

zscore_scale <- function(x) {
  x <- as.numeric(x)
  s <- sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) {
    out <- rep(0, length(x))
    out[is.na(x)] <- NA_real_
    return(out)
  }
  (x - m) / s
}

# Missing-aware weighted aggregation. Missing indicators do NOT become zero;
# the remaining available weights are re-normalized within each row.
calculate_composite_index_universal <- function(data, weights, selected_indicators) {
  if (length(weights) != length(selected_indicators)) {
    stop("Number of weights must match number of selected indicators.")
  }
  if (length(selected_indicators) == 0) {
    stop("At least one indicator must be selected.")
  }
  missing_cols <- setdiff(selected_indicators, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing indicator columns: ", paste(missing_cols, collapse = ", "))
  }

  w <- normalize_weights(weights)
  X <- as.matrix(data[, selected_indicators, drop = FALSE])
  storage.mode(X) <- "double"

  weighted <- sweep(X, 2, w, `*`)
  numerator <- rowSums(weighted, na.rm = TRUE)
  available <- !is.na(X)
  denominator <- as.numeric(available %*% w)

  score <- numerator / denominator
  score[denominator <= 0] <- NA_real_
  score
}

# ---- File import -------------------------------------------------------------
clean_imported_data <- function(data, first_col_names = FALSE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)

  # Drop fully empty rows/columns.
  if (nrow(data) > 0 && ncol(data) > 0) {
    row_all_na <- apply(data, 1, function(r) all(is.na(r) | trimws(as.character(r)) == ""))
    if (any(row_all_na)) data <- data[!row_all_na, , drop = FALSE]

    col_all_na <- vapply(data, function(x) all(is.na(x) | trimws(as.character(x)) == ""), logical(1))
    if (any(col_all_na)) data <- data[, !col_all_na, drop = FALSE]
  }

  # Make names syntactically safe and unique.
  names(data) <- make.names(names(data), unique = TRUE)

  # Convert mostly-numeric character columns to numeric.
  for (nm in names(data)) {
    if (is.character(data[[nm]])) {
      raw_nonmissing <- !is.na(data[[nm]]) & trimws(data[[nm]]) != ""
      if (sum(raw_nonmissing) > 0) {
        numeric_test <- suppressWarnings(as.numeric(gsub(",", "", data[[nm]])))
        numeric_ratio <- sum(!is.na(numeric_test) & raw_nonmissing) / sum(raw_nonmissing)
        if (is.finite(numeric_ratio) && numeric_ratio >= 0.8) {
          data[[nm]] <- numeric_test
        }
      }
    }
  }

  if (first_col_names && ncol(data) >= 1) {
    first_col <- as.character(data[[1]])
    valid_ids <- !is.na(first_col) & trimws(first_col) != ""
    if (all(valid_ids) && length(unique(first_col)) == nrow(data)) {
      # Keep the identifier column in the data so it can still be mapped as
      # Entity/Identifier; also expose it as row names for convenience.
      rownames(data) <- first_col
    }
  }

  data
}

read_data_file <- function(file_path, extension, sheet_name = NULL,
                           has_header = TRUE, first_col_names = FALSE) {
  extension <- tolower(extension)
  if (extension %in% c("xlsx", "xls")) {
    raw <- readxl::read_excel(
      file_path,
      sheet = sheet_name %||% 1,
      col_names = has_header
    )
  } else if (extension == "csv") {
    raw <- read.csv(
      file_path,
      header = has_header,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    stop("Unsupported file format. Please use CSV, XLS, or XLSX.")
  }

  if (!has_header && ncol(raw) > 0) {
    names(raw) <- paste0("Column_", seq_len(ncol(raw)))
  }

  clean_imported_data(raw, first_col_names = first_col_names)
}

# ---- Column role detection ---------------------------------------------------
detect_column_roles <- function(data) {
  info <- list()

  for (nm in names(data)) {
    x <- data[[nm]]
    clean <- x[!is.na(x)]

    if (length(clean) == 0) {
      info[[nm]] <- list(
        type = "empty", unique_count = 0, unique_ratio = 0,
        suggested_role = "skip", sample_values = "All NA"
      )
      next
    }

    unique_count <- length(unique(clean))
    unique_ratio <- unique_count / length(clean) * 100

    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
      type <- "time"
      role <- "time"
    } else if (is.numeric(x)) {
      year_like <- all(clean >= 1900 & clean <= 2100, na.rm = TRUE) && unique_count <= 100
      if (year_like) {
        role <- "time"
      } else if (unique_ratio > 90 && unique_count > 10) {
        role <- "identifier"
      } else {
        role <- "indicator"
      }
      type <- "numeric"
    } else if (is.character(x) || is.factor(x)) {
      role <- if (unique_ratio > 30 || unique_count > 20) "entity" else "category"
      type <- "categorical"
    } else {
      role <- "unknown"
      type <- class(x)[1]
    }

    samples <- head(unique(clean), 3)
    info[[nm]] <- list(
      type = type,
      unique_count = unique_count,
      unique_ratio = round(unique_ratio, 1),
      suggested_role = role,
      sample_values = paste(samples, collapse = ", ")
    )
  }

  info
}

# ---- Missing data ------------------------------------------------------------
impute_selected_indicators <- function(data, indicators, method, entity_col = NULL, time_col = NULL) {
  out <- data

  if (method == "remove") {
    return(out[complete.cases(out[, indicators, drop = FALSE]), , drop = FALSE])
  }

  if (method == "keep") return(out)

  if (method == "median") {
    for (nm in indicators) {
      med <- median(out[[nm]], na.rm = TRUE)
      if (is.finite(med)) out[[nm]][is.na(out[[nm]])] <- med
    }
    return(out)
  }

  if (method == "interpolation") {
    # Interpolate within each entity when an entity column exists, and use
    # time ordering when a time column is available. This prevents values
    # from one entity leaking into another entity's interpolation.
    groups <- if (!is.null(entity_col) && entity_col %in% names(out)) {
      split(seq_len(nrow(out)), as.character(out[[entity_col]]), drop = TRUE)
    } else {
      list(All = seq_len(nrow(out)))
    }

    for (idx in groups) {
      if (length(idx) == 0) next
      ord_idx <- idx
      if (!is.null(time_col) && time_col %in% names(out)) {
        ord_idx <- idx[order(out[[time_col]][idx], na.last = TRUE)]
      }
      for (nm in indicators) {
        x <- as.numeric(out[[nm]][ord_idx])
        if (sum(!is.na(x)) >= 2) {
          x <- zoo::na.approx(x, na.rm = FALSE)
          x <- zoo::na.locf(x, na.rm = FALSE)
          x <- zoo::na.locf(x, fromLast = TRUE, na.rm = FALSE)
          out[[nm]][ord_idx] <- x
        }
      }
    }
    return(out)
  }

  if (method == "missforest") {
    numeric_block <- out[, indicators, drop = FALSE]
    if (ncol(numeric_block) == 1) {
      med <- median(numeric_block[[1]], na.rm = TRUE)
      if (is.finite(med)) numeric_block[[1]][is.na(numeric_block[[1]])] <- med
    } else if (anyNA(numeric_block)) {
      mf <- missForest::missForest(numeric_block, verbose = FALSE)
      numeric_block <- mf$ximp
    }
    out[, indicators] <- numeric_block
    return(out)
  }

  stop("Unknown missing-data method.")
}

# ---- Ranking helpers ---------------------------------------------------------
entity_score_table <- function(data, entity_col, time_col = NULL,
                               score_col = "composite_index",
                               mode = "mean") {
  if (is.null(entity_col) || !entity_col %in% names(data)) return(NULL)
  if (!score_col %in% names(data)) return(NULL)

  d <- data %>% filter(!is.na(.data[[entity_col]]), !is.na(.data[[score_col]]))
  if (nrow(d) == 0) return(NULL)

  if (mode == "latest" && !is.null(time_col) && time_col %in% names(d)) {
    d <- d %>%
      group_by(.data[[entity_col]]) %>%
      arrange(.data[[time_col]], .by_group = TRUE) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      transmute(Entity = as.character(.data[[entity_col]]), Score = .data[[score_col]])
  } else {
    d <- d %>%
      group_by(.data[[entity_col]]) %>%
      summarise(Score = mean(.data[[score_col]], na.rm = TRUE), .groups = "drop")
    names(d)[names(d) == entity_col] <- "Entity"
    d$Entity <- as.character(d$Entity)
  }

  d %>% arrange(desc(Score)) %>% mutate(Rank = row_number())
}

# ---- Forecast helpers --------------------------------------------------------
forecast_series <- function(y, h = 5, frequency = 1) {
  y <- as.numeric(y)
  y <- y[is.finite(y)]
  h <- max(1L, as.integer(h))
  frequency <- max(1L, as.integer(frequency))

  if (length(y) == 0) {
    return(list(forecast = rep(NA_real_, h), lower = rep(NA_real_, h),
                upper = rep(NA_real_, h), method = "No valid observations"))
  }

  if (length(y) < 3) {
    m <- mean(y)
    s <- if (length(y) > 1) sd(y) else 0
    margin <- if (is.finite(s)) 1.96 * s else 0
    return(list(
      forecast = rep(m, h),
      lower = rep(m - margin, h),
      upper = rep(m + margin, h),
      method = "Simple mean"
    ))
  }

  tryCatch({
    y_ts <- ts(y, frequency = frequency)
    fit <- forecast::auto.arima(y_ts, seasonal = frequency > 1)
    fc <- forecast::forecast(fit, h = h, level = 95)
    list(
      forecast = as.numeric(fc$mean),
      lower = as.numeric(fc$lower[, 1]),
      upper = as.numeric(fc$upper[, 1]),
      method = paste0("ARIMA", if (frequency > 1) " (seasonal allowed)" else "")
    )
  }, error = function(e) {
    idx <- seq_along(y)
    fit <- lm(y ~ idx)
    future_idx <- seq(length(y) + 1, length(y) + h)
    pred <- predict(fit, newdata = data.frame(idx = future_idx), interval = "prediction", level = 0.95)
    list(
      forecast = as.numeric(pred[, "fit"]),
      lower = as.numeric(pred[, "lwr"]),
      upper = as.numeric(pred[, "upr"]),
      method = "Linear trend fallback"
    )
  })
}

future_time_values <- function(time_values, h) {
  x <- time_values[!is.na(time_values)]
  if (length(x) == 0) return(seq_len(h))

  if (inherits(x, "Date")) {
    ux <- sort(unique(x))
    step <- if (length(ux) >= 2) as.numeric(median(diff(ux))) else 1
    if (!is.finite(step) || step <= 0) step <- 1
    return(max(ux) + step * seq_len(h))
  }

  if (inherits(x, c("POSIXct", "POSIXt"))) {
    ux <- sort(unique(as.numeric(x)))
    step <- if (length(ux) >= 2) median(diff(ux)) else 86400
    vals <- max(ux) + step * seq_len(h)
    return(as.POSIXct(vals, origin = "1970-01-01", tz = attr(x, "tzone") %||% "UTC"))
  }

  xn <- suppressWarnings(as.numeric(as.character(x)))
  if (sum(is.finite(xn)) >= 1) {
    ux <- sort(unique(xn[is.finite(xn)]))
    step <- if (length(ux) >= 2) median(diff(ux)) else 1
    if (!is.finite(step) || step <= 0) step <- 1
    return(max(ux) + step * seq_len(h))
  }

  paste0("t+", seq_len(h))
}

# ---- Pillar helpers ----------------------------------------------------------
within_group_weights <- function(data, indicators, method = "equal", custom = NULL) {
  indicators <- indicators[indicators %in% names(data)]
  if (length(indicators) == 0) return(numeric(0))
  if (length(indicators) == 1) return(1)

  if (method == "equal") return(rep(1 / length(indicators), length(indicators)))

  if (method == "custom") {
    if (is.null(custom) || length(custom) != length(indicators)) {
      return(rep(1 / length(indicators), length(indicators)))
    }
    return(normalize_weights(custom))
  }

  X <- data[, indicators, drop = FALSE]

  if (method == "correlation") {
    cor_matrix <- suppressWarnings(cor(X, use = "pairwise.complete.obs"))
    if (!is.matrix(cor_matrix) || any(dim(cor_matrix) != length(indicators))) {
      return(rep(1 / length(indicators), length(indicators)))
    }
    diag(cor_matrix) <- NA_real_
    avg_abs <- rowMeans(abs(cor_matrix), na.rm = TRUE)
    avg_abs[!is.finite(avg_abs)] <- 0
    scores <- pmax(1 - avg_abs, 0)
    if (sum(scores) <= .Machine$double.eps) {
      return(rep(1 / length(indicators), length(indicators)))
    }
    return(normalize_weights(scores))
  }

  if (method == "pca") {
    # Remove constant columns and use complete rows for a stable PCA.
    sds <- vapply(X, sd, numeric(1), na.rm = TRUE)
    usable <- is.finite(sds) & sds > 0
    if (sum(usable) < 2) return(rep(1 / length(indicators), length(indicators)))

    X2 <- X[, usable, drop = FALSE]
    X2 <- X2[complete.cases(X2), , drop = FALSE]
    if (nrow(X2) < 3) return(rep(1 / length(indicators), length(indicators)))

    pca <- tryCatch(prcomp(X2, scale. = TRUE, center = TRUE), error = function(e) NULL)
    if (is.null(pca)) return(rep(1 / length(indicators), length(indicators)))

    loadings <- abs(pca$rotation[, 1])
    w <- rep(0, length(indicators))
    names(w) <- indicators
    w[names(loadings)] <- loadings
    if (sum(w) <= 0) return(rep(1 / length(indicators), length(indicators)))
    return(normalize_weights(w))
  }

  rep(1 / length(indicators), length(indicators))
}

calculate_group_based_index <- function(data, group_definitions,
                                        within_group_method = "equal",
                                        custom_weight_provider = NULL) {
  if (length(group_definitions) == 0) return(NULL)

  valid_groups <- list()
  for (nm in names(group_definitions)) {
    g <- group_definitions[[nm]]
    indicators <- intersect(g$indicators, names(data))
    if (length(indicators) > 0) {
      g$indicators <- indicators
      valid_groups[[nm]] <- g
    }
  }
  if (length(valid_groups) == 0) return(NULL)

  group_weights <- normalize_weights(vapply(valid_groups, function(g) g$weight, numeric(1)))
  names(group_weights) <- names(valid_groups)

  group_indices <- list()
  for (nm in names(valid_groups)) {
    g <- valid_groups[[nm]]
    custom <- NULL
    if (within_group_method == "custom" && !is.null(custom_weight_provider)) {
      custom <- custom_weight_provider(nm, g$indicators)
    }

    ind_w <- within_group_weights(data, g$indicators, within_group_method, custom)
    idx <- calculate_composite_index_universal(data, ind_w, g$indicators)

    group_indices[[nm]] <- list(
      index = idx,
      weight = group_weights[[nm]],
      indicators = g$indicators,
      indicator_weights = ind_w
    )
  }

  group_matrix <- do.call(cbind, lapply(group_indices, `[[`, "index"))
  colnames(group_matrix) <- names(group_indices)
  group_df <- as.data.frame(group_matrix)
  overall <- calculate_composite_index_universal(group_df, group_weights, names(group_indices))

  list(
    overall_index = overall,
    group_indices = group_indices,
    group_weights = group_weights
  )
}

create_group_statistics <- function(group_result) {
  if (is.null(group_result) || length(group_result$group_indices) == 0) return(NULL)
  do.call(rbind, lapply(names(group_result$group_indices), function(nm) {
    g <- group_result$group_indices[[nm]]
    data.frame(
      Pillar = nm,
      Indicators_Count = length(g$indicators),
      Pillar_Weight = round(g$weight, 4),
      Mean_Index = round(mean(g$index, na.rm = TRUE), 4),
      SD_Index = round(sd(g$index, na.rm = TRUE), 4),
      Min_Index = round(min(g$index, na.rm = TRUE), 4),
      Max_Index = round(max(g$index, na.rm = TRUE), 4),
      Indicators = paste(g$indicators, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
}

# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Composite Index Platform"),

  dashboardSidebar(
    sidebarMenu(
      id = "main_tabs",
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Column Mapping", tabName = "mapping", icon = icon("table")),
      menuItem("Data Processing", tabName = "processing", icon = icon("cogs")),
      menuItem("Custom Weighting", tabName = "weighting", icon = icon("balance-scale")),
      menuItem("Analysis Results", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("stethoscope")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("line-chart")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-line")),
      menuItem("Comparison", tabName = "comparison", icon = icon("exchange-alt")),
      menuItem("Sub-index / Pillar", tabName = "groupindex", icon = icon("layer-group"))
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("\n      .weight-box { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); }\n      .weight-input { margin-bottom: 8px; padding: 6px; background: rgba(255,255,255,0.10); border-radius: 5px; }\n      .content-wrapper, .right-side { background-color: #f4f4f4; }\n      .sheet-download { margin: 4px 8px 4px 0; display: inline-block; }\n      .small-note { font-size: 12px; color: #666; }\n    "))),

    tabItems(
      # ---- Upload ------------------------------------------------------------
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Upload Data", status = "primary", solidHeader = TRUE, width = 7,
            fileInput("data_file", "Choose CSV or Excel File", accept = c(".csv", ".xlsx", ".xls")),
            checkboxInput("has_header", "First row contains column names", TRUE),
            checkboxInput("first_col_names", "First column contains row identifiers (column will be retained)", FALSE),

            conditionalPanel(
              condition = "output.is_excel_upload",
              fluidRow(
                column(8,
                  selectizeInput(
                    "excel_sheets", "Sheets to load / download:",
                    choices = NULL, multiple = TRUE,
                    options = list(placeholder = "Choose one or more sheets...")
                  )
                ),
                column(4,
                  br(),
                  actionButton("refresh_sheet_list", "Refresh Sheet List",
                               class = "btn-warning", icon = icon("refresh"), width = "100%")
                )
              ),
              actionButton("load_selected_sheets", "Load Selected Sheets",
                           class = "btn-info", icon = icon("folder-open"))
            ),

            conditionalPanel(
              condition = "output.sheet_selector_ready",
              hr(),
              selectInput("active_sheet", "Active sheet for analysis:", choices = NULL),
              fluidRow(
                column(6,
                  actionButton("load_active_sheet", "Load / Refresh Active Sheet",
                               class = "btn-primary", icon = icon("refresh"), width = "100%")
                ),
                column(6,
                  actionButton("reload_active", "Reload with Header Options",
                               class = "btn-default", icon = icon("repeat"), width = "100%")
                )
              ),
              p("For Excel files, the Active Sheet list always contains every sheet in the workbook. Select another sheet and click Load / Refresh Active Sheet.",
                class = "small-note"),
              br(),
              verbatimTextOutput("data_summary")
            )
          ),

          box(
            title = "Sheet Downloads", status = "success", solidHeader = TRUE, width = 5,
            p("Every Excel sheet can be downloaded independently as CSV, even if it is not the active sheet."),
            uiOutput("sheet_download_buttons"),
            conditionalPanel(
              condition = "output.multiple_sheets_loaded",
              br(),
              downloadButton("download_all_sheets", "Download all workbook sheets (ZIP)", class = "btn-success")
            )
          )
        ),

        fluidRow(
          box(
            title = "Loaded Sheets", status = "info", solidHeader = TRUE, width = 12,
            DTOutput("loaded_sheets_table")
          )
        ),

        fluidRow(
          box(
            title = "Active Sheet - Column Summary", status = "success", solidHeader = TRUE, width = 12,
            DTOutput("upload_data_summary")
          )
        ),

        fluidRow(
          box(
            title = "Active Sheet - Preview", status = "success", solidHeader = TRUE, width = 12,
            DTOutput("uploaded_data_preview")
          )
        )
      ),

      # ---- Mapping -----------------------------------------------------------
      tabItem(
        tabName = "mapping",
        fluidRow(
          box(
            title = "Automatic Column Analysis", status = "info", solidHeader = TRUE, width = 12,
            DTOutput("column_analysis_table")
          )
        ),
        fluidRow(
          box(
            title = "Column Mapping", status = "warning", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4, selectInput("entity_column", "Entity column (required):", choices = NULL)),
              column(4, selectInput("time_column", "Time column (optional):", choices = NULL)),
              column(4, selectInput("identifier_column", "Additional identifier (optional):", choices = NULL))
            ),
            actionButton("confirm_mapping", "Confirm Mapping", class = "btn-success", icon = icon("check"))
          )
        )
      ),

      # ---- Processing --------------------------------------------------------
      tabItem(
        tabName = "processing",
        fluidRow(
          box(
            title = "Indicators", status = "primary", solidHeader = TRUE, width = 6,
            checkboxGroupInput("indicators_select", "Select numeric indicators:", choices = NULL),
            fluidRow(
              column(6, actionButton("select_all_indicators", "Select All", class = "btn-info btn-sm")),
              column(6, actionButton("clear_all_indicators", "Clear All", class = "btn-warning btn-sm"))
            )
          ),
          box(
            title = "Processing Options", status = "warning", solidHeader = TRUE, width = 6,
            selectInput(
              "normalization_method", "Normalization:",
              choices = c("None" = "none", "Min-Max (0-1)" = "minmax", "Z-score" = "zscore"),
              selected = "minmax"
            ),
            selectInput(
              "missing_method", "Missing data:",
              choices = c(
                "Remove incomplete rows" = "remove",
                "Keep missing; re-normalize available weights" = "keep",
                "Median imputation" = "median",
                "Linear interpolation" = "interpolation",
                "MissForest" = "missforest"
              ),
              selected = "remove"
            ),
            selectInput(
              "direction_option", "Indicator direction:",
              choices = c("Higher is better" = "higher", "Lower is better" = "lower", "Mixed" = "mixed"),
              selected = "higher"
            ),
            conditionalPanel(
              condition = "input.direction_option == 'mixed'",
              uiOutput("indicator_directions_ui")
            ),
            numericInput("min_data_points", "Minimum rows per entity:", value = 1, min = 1, step = 1),
            actionButton("process_data", "Process Data", class = "btn-primary btn-lg", icon = icon("play"))
          )
        ),
        fluidRow(
          box(title = "Processed Data Preview", status = "success", solidHeader = TRUE, width = 12,
              DTOutput("processed_data_table"))
        ),
        fluidRow(
          box(title = "Processed Data Summary", status = "info", solidHeader = TRUE, width = 12,
              DTOutput("data_summary_table"))
        )
      ),

      # ---- Weighting ---------------------------------------------------------
      tabItem(
        tabName = "weighting",
        fluidRow(
          box(
            title = "Custom Indicator Weights", status = "primary", solidHeader = TRUE, width = 6,
            class = "weight-box",
            uiOutput("weight_inputs"),
            fluidRow(
              column(6, actionButton("set_equal_weights", "Equal Weights", class = "btn-info")),
              column(6, actionButton("apply_weights", "Apply Weights", class = "btn-success"))
            )
          ),
          box(title = "Weights Summary", status = "info", solidHeader = TRUE, width = 6,
              DTOutput("weights_summary"))
        ),
        fluidRow(
          box(title = "Weight Impact on Entity Ranking", status = "success", solidHeader = TRUE, width = 6,
              plotlyOutput("weight_impact_plot")),
          box(title = "Largest Rank Changes", status = "warning", solidHeader = TRUE, width = 6,
              DTOutput("weight_comparison_table"))
        )
      ),

      # ---- Analysis ----------------------------------------------------------
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "Ranking Configuration", status = "warning", solidHeader = TRUE, width = 4,
            selectInput(
              "ranking_mode", "How to rank entities:",
              choices = c("Mean across available rows" = "mean", "Latest observation" = "latest"),
              selected = "latest"
            ),
            downloadButton("download_processed", "Download processed active sheet", class = "btn-success")
          ),
          box(title = "Composite Index Rankings", status = "primary", solidHeader = TRUE, width = 8,
              plotlyOutput("ranking_plot"))
        ),
        fluidRow(
          box(title = "Indicator Correlations", status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("correlation_plot")),
          box(title = "Entity Ranking Table", status = "success", solidHeader = TRUE, width = 6,
              DTOutput("entity_ranking_table"))
        ),
        fluidRow(
          box(title = "Detailed Row-Level Results", status = "success", solidHeader = TRUE, width = 12,
              DTOutput("composite_results_table"))
        )
      ),

      # ---- Diagnostics -------------------------------------------------------
      tabItem(
        tabName = "diagnostics",
        fluidRow(
          box(title = "Cronbach Alpha", status = "primary", solidHeader = TRUE, width = 4,
              verbatimTextOutput("cronbach")),
          box(title = "Coefficient of Variation", status = "info", solidHeader = TRUE, width = 8,
              DTOutput("cv_table"))
        ),
        fluidRow(
          box(title = "PCA Summary", status = "warning", solidHeader = TRUE, width = 6,
              verbatimTextOutput("pca_summary")),
          box(title = "PCA Biplot", status = "warning", solidHeader = TRUE, width = 6,
              plotOutput("pca_plot", height = 420))
        ),
        fluidRow(
          box(title = "Weight Sensitivity (+5%)", status = "success", solidHeader = TRUE, width = 6,
              DTOutput("sensitivity_table")),
          box(title = "Correlation Heatmap", status = "success", solidHeader = TRUE, width = 6,
              plotOutput("sensitivity_heatmap", height = 430))
        ),
        fluidRow(
          box(title = "Weight Structure (Sankey)", status = "info", solidHeader = TRUE, width = 12,
              sankeyNetworkOutput("sankey", height = "360px"))
        )
      ),

      # ---- Time series -------------------------------------------------------
      tabItem(
        tabName = "timeseries",
        fluidRow(
          box(
            title = "Single Entity Time Series", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("ts_entity", "Entity:", choices = NULL),
            selectInput("ts_metric", "Metric:", choices = NULL),
            plotlyOutput("timeseries_plot")
          ),
          box(
            title = "Multi-Entity Time Series", status = "info", solidHeader = TRUE, width = 6,
            selectizeInput("multi_ts_entities", "Entities (max 10):", choices = NULL, multiple = TRUE),
            selectInput("multi_ts_metric", "Metric:", choices = NULL),
            plotlyOutput("multi_timeseries_plot")
          )
        )
      ),

      # ---- Forecast ----------------------------------------------------------
      tabItem(
        tabName = "forecasting",
        fluidRow(
          box(
            title = "Forecast Settings", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("forecast_entity", "Entity:", choices = NULL),
            selectInput("forecast_metric", "Metric:", choices = NULL),
            numericInput("forecast_periods", "Forecast horizon:", value = 5, min = 1, max = 50),
            numericInput("forecast_frequency", "Time-series frequency (1=annual/non-seasonal, 4=quarterly, 12=monthly):",
                         value = 1, min = 1, max = 365),
            actionButton("generate_forecast", "Generate Forecast", class = "btn-success", icon = icon("chart-line"))
          ),
          box(title = "Forecast Plot", status = "success", solidHeader = TRUE, width = 8,
              plotlyOutput("forecast_plot"))
        ),
        fluidRow(
          box(title = "Forecast Data", status = "info", solidHeader = TRUE, width = 6,
              DTOutput("forecast_table")),
          box(
            title = "Multi-Entity Forecast", status = "warning", solidHeader = TRUE, width = 6,
            selectizeInput("multi_forecast_entities", "Entities:", choices = NULL, multiple = TRUE),
            actionButton("generate_multi_forecast", "Generate Multi Forecast", class = "btn-warning"),
            plotlyOutput("multi_forecast_plot")
          )
        )
      ),

      # ---- Comparison --------------------------------------------------------
      tabItem(
        tabName = "comparison",
        fluidRow(
          box(
            title = "Entity Comparison", status = "primary", solidHeader = TRUE, width = 4,
            selectizeInput("compare_entities", "Entities:", choices = NULL, multiple = TRUE),
            selectInput("compare_metric", "Metric:", choices = NULL),
            conditionalPanel(
              condition = "output.has_time_column",
              selectInput("compare_time_period", "Time period:", choices = NULL)
            ),
            actionButton("generate_comparison", "Generate Comparison", class = "btn-info", icon = icon("exchange-alt")),
            br(), br(),
            verbatimTextOutput("comparison_summary")
          ),
          box(title = "Comparison Chart", status = "success", solidHeader = TRUE, width = 8,
              plotlyOutput("comparison_plot"))
        ),
        fluidRow(
          box(title = "Comparison Details", status = "info", solidHeader = TRUE, width = 12,
              DTOutput("comparison_table"))
        )
      ),

      # ---- Pillars -----------------------------------------------------------
      tabItem(
        tabName = "groupindex",
        fluidRow(
          box(
            title = "Pillar-based Composite Index", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(
                6,
                h4("Step 1: Define Pillars"),
                textInput("group_name", "Pillar name:", placeholder = "e.g., Economic"),
                numericInput("group_weight", "Pillar weight:", value = 1, min = 0, step = 0.1),
                selectizeInput("group_indicators", "Indicators in this pillar:", choices = NULL, multiple = TRUE),
                fluidRow(
                  column(6, actionButton("add_pillar", "Add / Update Pillar", class = "btn-success")),
                  column(6, actionButton("clear_pillars", "Clear All", class = "btn-warning"))
                ),
                hr(),
                h4("Step 2: Within-Pillar Weighting"),
                radioButtons(
                  "within_pillar_method", "Method:",
                  choices = c(
                    "Equal weights" = "equal",
                    "Custom weights" = "custom",
                    "Correlation-based" = "correlation",
                    "PCA-based" = "pca"
                  ),
                  selected = "equal"
                ),
                conditionalPanel(
                  condition = "input.within_pillar_method == 'custom'",
                  uiOutput("custom_weights_tables")
                ),
                actionButton("calculate_pillar_index", "Calculate Pillar Index", class = "btn-primary btn-lg")
              ),
              column(
                6,
                h4("Current Pillars"),
                uiOutput("pillars_display"),
                h4("Pillar Weights"),
                plotlyOutput("pillar_weights_plot", height = "300px")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Pillar Results", status = "success", solidHeader = TRUE, width = 12,
            tabsetPanel(
              tabPanel("Index Values", DTOutput("pillar_index_table")),
              tabPanel(
                "Contributions",
                fluidRow(
                  column(6, plotlyOutput("pillar_contribution_plot")),
                  column(6, plotlyOutput("pillar_performance_plot"))
                )
              ),
              tabPanel("Statistics", DTOutput("pillar_stats_table")),
              tabPanel("Pillar Correlations", plotlyOutput("pillar_correlation_plot"))
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  values <- reactiveValues(
    file_path = NULL,
    file_ext = NULL,
    sheets = list(),
    available_sheets = character(0),
    raw_data = NULL,
    active_sheet = NULL,
    column_info = NULL,
    mapping_confirmed = FALSE,
    entity_column = NULL,
    time_column = NULL,
    identifier_column = NULL,
    selected_indicators = NULL,
    processed_data = NULL,
    processed_equal = NULL,
    custom_weights = NULL,
    weights_applied = FALSE,
    forecast_results = NULL,
    multi_forecast_data = NULL
  )

  group_definitions <- reactiveVal(list())
  group_index_result <- reactiveVal(NULL)

  reset_analysis_state <- function(clear_mapping = TRUE) {
    values$selected_indicators <- NULL
    values$processed_data <- NULL
    values$processed_equal <- NULL
    values$custom_weights <- NULL
    values$weights_applied <- FALSE
    values$forecast_results <- NULL
    values$multi_forecast_data <- NULL
    group_definitions(list())
    group_index_result(NULL)

    if (clear_mapping) {
      values$mapping_confirmed <- FALSE
      values$entity_column <- NULL
      values$time_column <- NULL
      values$identifier_column <- NULL
    }
  }

  set_active_sheet <- function(sheet_name) {
    req(sheet_name, sheet_name %in% names(values$sheets))
    values$active_sheet <- sheet_name
    values$raw_data <- values$sheets[[sheet_name]]
    values$column_info <- detect_column_roles(values$raw_data)
    reset_analysis_state(clear_mapping = TRUE)
    update_column_mapping_choices()
  }

  update_column_mapping_choices <- function() {
    if (is.null(values$raw_data) || is.null(values$column_info)) return()

    all_cols <- names(values$raw_data)
    entity_suggestions <- names(values$column_info)[vapply(values$column_info, function(x) x$suggested_role == "entity", logical(1))]
    time_suggestions <- names(values$column_info)[vapply(values$column_info, function(x) x$suggested_role == "time", logical(1))]

    entity_default <- if (length(entity_suggestions) > 0) entity_suggestions[1] else if (length(all_cols) > 0) all_cols[1] else ""
    time_default <- if (length(time_suggestions) > 0) time_suggestions[1] else ""

    updateSelectInput(session, "entity_column", choices = all_cols, selected = entity_default)
    updateSelectInput(session, "time_column", choices = c("None" = "", all_cols), selected = time_default)
    updateSelectInput(session, "identifier_column", choices = c("None" = "", all_cols), selected = "")
  }

  update_analysis_choices <- function() {
    if (is.null(values$processed_data) || is.null(values$entity_column)) return()

    entities <- sort(unique(as.character(values$processed_data[[values$entity_column]])))
    metrics <- unique(c("composite_index", values$selected_indicators))

    updateSelectInput(session, "ts_entity", choices = entities)
    updateSelectizeInput(session, "multi_ts_entities", choices = entities, server = TRUE)
    updateSelectInput(session, "forecast_entity", choices = entities)
    updateSelectizeInput(session, "multi_forecast_entities", choices = entities, server = TRUE)
    updateSelectizeInput(session, "compare_entities", choices = entities, server = TRUE)

    updateSelectInput(session, "ts_metric", choices = metrics, selected = "composite_index")
    updateSelectInput(session, "multi_ts_metric", choices = metrics, selected = "composite_index")
    updateSelectInput(session, "forecast_metric", choices = metrics, selected = "composite_index")
    updateSelectInput(session, "compare_metric", choices = metrics, selected = "composite_index")

    if (!is.null(values$time_column) && values$time_column %in% names(values$processed_data)) {
      tvals <- sort(unique(values$processed_data[[values$time_column]]))
      choices <- c("All periods" = "", setNames(as.character(tvals), as.character(tvals)))
      updateSelectInput(session, "compare_time_period", choices = choices, selected = "")
    }

    updateSelectizeInput(session, "group_indicators", choices = values$selected_indicators, server = TRUE)
  }

  # ---- Upload handlers -------------------------------------------------------
  output$is_excel_upload <- reactive({
    !is.null(values$file_ext) && values$file_ext %in% c("xlsx", "xls")
  })
  outputOptions(output, "is_excel_upload", suspendWhenHidden = FALSE)

  output$data_uploaded <- reactive(!is.null(values$raw_data))
  outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)

  # The sheet selector must become visible as soon as an Excel workbook is
  # recognised, even before multiple sheets have been loaded into values$sheets.
  output$sheet_selector_ready <- reactive({
    (!is.null(values$file_ext) && values$file_ext %in% c("xlsx", "xls") &&
       length(values$available_sheets) > 0) || !is.null(values$raw_data)
  })
  outputOptions(output, "sheet_selector_ready", suspendWhenHidden = FALSE)

  output$multiple_sheets_loaded <- reactive(length(values$available_sheets) > 1)
  outputOptions(output, "multiple_sheets_loaded", suspendWhenHidden = FALSE)

  output$has_time_column <- reactive(!is.null(values$time_column) && nzchar(values$time_column))
  outputOptions(output, "has_time_column", suspendWhenHidden = FALSE)

  # Re-read the workbook's sheet names and synchronise both selectors.
  refresh_workbook_sheet_choices <- function(preserve_selection = TRUE) {
    req(values$file_path, values$file_ext %in% c("xlsx", "xls"))

    sheet_names <- tryCatch(
      readxl::excel_sheets(values$file_path),
      error = function(e) {
        showNotification(paste("Could not read workbook sheet names:", e$message),
                         type = "error", duration = 7)
        character(0)
      }
    )

    if (length(sheet_names) == 0) return(FALSE)

    old_multi <- if (preserve_selection) isolate(input$excel_sheets) else character(0)
    old_active <- if (preserve_selection) isolate(input$active_sheet) else NULL

    values$available_sheets <- sheet_names

    selected_multi <- intersect(old_multi %||% character(0), sheet_names)
    if (length(selected_multi) == 0) selected_multi <- sheet_names[1]

    selected_active <- if (!is.null(old_active) && old_active %in% sheet_names) {
      old_active
    } else if (!is.null(values$active_sheet) && values$active_sheet %in% sheet_names) {
      values$active_sheet
    } else {
      sheet_names[1]
    }

    # server = FALSE is deliberate here. Workbook sheet lists are normally
    # small, and client-side updating is more reliable while conditionalPanel
    # visibility is changing immediately after upload.
    updateSelectizeInput(
      session, "excel_sheets",
      choices = sheet_names,
      selected = selected_multi,
      server = FALSE
    )
    updateSelectInput(
      session, "active_sheet",
      choices = setNames(sheet_names, sheet_names),
      selected = selected_active
    )

    TRUE
  }

  # Load one workbook sheet by name. The sheet does not need to have been part
  # of the multi-sheet selection; choosing it in Active Sheet is sufficient.
  load_excel_sheet_by_name <- function(sheet_name, notify = TRUE) {
    req(values$file_path, values$file_ext %in% c("xlsx", "xls"))

    if (is.null(sheet_name) || !nzchar(sheet_name) ||
        !sheet_name %in% values$available_sheets) {
      if (notify) showNotification("Please choose a valid workbook sheet.", type = "warning")
      return(FALSE)
    }

    d <- tryCatch(
      read_data_file(
        values$file_path,
        values$file_ext,
        sheet_name = sheet_name,
        has_header = input$has_header,
        first_col_names = input$first_col_names
      ),
      error = function(e) {
        showNotification(
          paste0("Sheet '", sheet_name, "' could not be loaded: ", e$message),
          type = "error", duration = 7
        )
        NULL
      }
    )

    if (is.null(d)) return(FALSE)

    values$sheets[[sheet_name]] <- d
    set_active_sheet(sheet_name)

    # Keep the selector explicitly synchronised after a server-side load.
    updateSelectInput(
      session, "active_sheet",
      choices = setNames(values$available_sheets, values$available_sheets),
      selected = sheet_name
    )

    if (notify) {
      showNotification(paste("Active sheet loaded:", sheet_name),
                       type = "message", duration = 4)
    }
    TRUE
  }

  observeEvent(input$data_file, {
    req(input$data_file)

    values$file_path <- input$data_file$datapath
    values$file_ext <- tolower(tools::file_ext(input$data_file$name))
    values$sheets <- list()
    values$available_sheets <- character(0)
    values$raw_data <- NULL
    values$active_sheet <- NULL
    values$column_info <- NULL
    reset_analysis_state(TRUE)

    # Clear stale browser-side selections from a previously uploaded file.
    updateSelectInput(session, "active_sheet", choices = character(0), selected = character(0))
    updateSelectizeInput(session, "excel_sheets", choices = character(0),
                         selected = character(0), server = FALSE)

    if (values$file_ext %in% c("xlsx", "xls")) {
      ok <- refresh_workbook_sheet_choices(preserve_selection = FALSE)
      if (!isTRUE(ok)) {
        showNotification("No readable Excel sheets were found.", type = "error")
        return()
      }

      # Automatically load the first sheet so the workbook is immediately
      # usable, while the Active Sheet selector already contains ALL sheets.
      first_sheet <- values$available_sheets[1]
      load_excel_sheet_by_name(first_sheet, notify = FALSE)

      showNotification(
        paste0(
          "Excel workbook loaded. ", length(values$available_sheets),
          " sheet(s) found. Choose any sheet under 'Active sheet for analysis'."
        ),
        type = "message", duration = 6
      )

    } else if (values$file_ext == "csv") {
      d <- tryCatch(
        read_data_file(values$file_path, "csv",
                       has_header = input$has_header,
                       first_col_names = input$first_col_names),
        error = function(e) {
          showNotification(paste("CSV load error:", e$message), type = "error", duration = 7)
          NULL
        }
      )

      if (!is.null(d)) {
        values$available_sheets <- "CSV_Data"
        values$sheets <- list(CSV_Data = d)
        updateSelectInput(session, "active_sheet",
                          choices = c("CSV_Data" = "CSV_Data"),
                          selected = "CSV_Data")
        set_active_sheet("CSV_Data")
        showNotification("CSV loaded successfully.", type = "message")
      }

    } else {
      showNotification("Unsupported file format. Use CSV, XLS, or XLSX.", type = "error")
    }
  }, ignoreInit = TRUE)

  # Re-read workbook sheet names. This is useful if the workbook was replaced
  # or modified and the user wants the selector rebuilt without restarting app.
  observeEvent(input$refresh_sheet_list, {
    req(values$file_ext %in% c("xlsx", "xls"))
    if (isTRUE(refresh_workbook_sheet_choices(preserve_selection = TRUE))) {
      showNotification(
        paste("Sheet list refreshed:", length(values$available_sheets), "sheet(s) found."),
        type = "message", duration = 4
      )
    }
  })

  # Load a user-selected collection for independent download / inspection.
  observeEvent(input$load_selected_sheets, {
    req(values$file_path, values$file_ext %in% c("xlsx", "xls"))

    selected <- input$excel_sheets %||% character(0)
    selected <- intersect(selected, values$available_sheets)

    if (length(selected) == 0) {
      showNotification("Select at least one sheet.", type = "warning")
      return()
    }

    loaded <- list()
    withProgress(message = "Loading selected sheets...", value = 0, {
      for (i in seq_along(selected)) {
        nm <- selected[i]
        incProgress(1 / length(selected), detail = nm)
        d <- tryCatch(
          read_data_file(
            values$file_path, values$file_ext, sheet_name = nm,
            has_header = input$has_header,
            first_col_names = input$first_col_names
          ),
          error = function(e) {
            showNotification(
              paste0("Sheet '", nm, "' could not be loaded: ", e$message),
              type = "error", duration = 7
            )
            NULL
          }
        )
        if (!is.null(d)) loaded[[nm]] <- d
      }
    })

    if (length(loaded) == 0) return()
    values$sheets <- loaded

    # Prefer the currently selected active sheet if it was among the loaded
    # sheets; otherwise use the first loaded sheet. The selector itself still
    # keeps ALL workbook sheet names, not only loaded ones.
    candidate <- input$active_sheet
    if (is.null(candidate) || !candidate %in% names(values$sheets)) {
      candidate <- names(values$sheets)[1]
    }

    set_active_sheet(candidate)
    updateSelectInput(
      session, "active_sheet",
      choices = setNames(values$available_sheets, values$available_sheets),
      selected = candidate
    )

    showNotification(
      paste(length(values$sheets), "sheet(s) loaded for download/analysis."),
      type = "message", duration = 4
    )
  })

  # Selecting a different Active Sheet immediately loads it if needed. This
  # makes switching sheets a one-step action; the button below can always be
  # used to force a fresh read from the workbook.
  observeEvent(input$active_sheet, {
    nm <- input$active_sheet
    if (is.null(nm) || !nzchar(nm)) return()

    if (values$file_ext %in% c("xlsx", "xls")) {
      if (!nm %in% values$available_sheets) return()

      if (nm %in% names(values$sheets)) {
        if (!identical(nm, values$active_sheet)) {
          set_active_sheet(nm)
          showNotification(paste("Active sheet:", nm), type = "message", duration = 3)
        }
      } else {
        load_excel_sheet_by_name(nm, notify = TRUE)
      }

    } else if (values$file_ext == "csv" && nm == "CSV_Data" &&
               nm %in% names(values$sheets) && !identical(nm, values$active_sheet)) {
      set_active_sheet(nm)
    }
  }, ignoreInit = TRUE)

  # Explicitly force a fresh read of the currently selected workbook sheet.
  observeEvent(input$load_active_sheet, {
    req(input$active_sheet)

    if (values$file_ext %in% c("xlsx", "xls")) {
      load_excel_sheet_by_name(input$active_sheet, notify = TRUE)
    } else if (values$file_ext == "csv") {
      d <- tryCatch(
        read_data_file(values$file_path, "csv",
                       has_header = input$has_header,
                       first_col_names = input$first_col_names),
        error = function(e) {
          showNotification(paste("CSV reload error:", e$message), type = "error", duration = 7)
          NULL
        }
      )
      if (!is.null(d)) {
        values$sheets[["CSV_Data"]] <- d
        set_active_sheet("CSV_Data")
        showNotification("CSV refreshed.", type = "message")
      }
    }
  })

  # Reload uses the current header / row-identifier options. It is retained as
  # a separate button because changing those options changes data structure.
  observeEvent(input$reload_active, {
    req(values$file_path, input$active_sheet)

    if (values$file_ext == "csv") {
      d <- tryCatch(
        read_data_file(values$file_path, "csv",
                       has_header = input$has_header,
                       first_col_names = input$first_col_names),
        error = function(e) {
          showNotification(paste("Reload error:", e$message), type = "error", duration = 7)
          NULL
        }
      )
      if (is.null(d)) return()
      values$sheets[["CSV_Data"]] <- d
      set_active_sheet("CSV_Data")

    } else {
      nm <- input$active_sheet
      if (!nm %in% values$available_sheets) {
        showNotification("The selected sheet is no longer available in the workbook.", type = "error")
        return()
      }
      d <- tryCatch(
        read_data_file(values$file_path, values$file_ext, sheet_name = nm,
                       has_header = input$has_header,
                       first_col_names = input$first_col_names),
        error = function(e) {
          showNotification(paste("Reload error:", e$message), type = "error", duration = 7)
          NULL
        }
      )
      if (is.null(d)) return()
      values$sheets[[nm]] <- d
      set_active_sheet(nm)
    }

    showNotification("Active sheet reloaded with the current header settings.",
                     type = "message", duration = 4)
  })

  # Dynamic individual download handlers for every workbook sheet.
  # Excel sheets are read on demand, so a sheet can be downloaded even when it
  # has never been loaded as the active analysis sheet.
  observe({
    sheet_names <- values$available_sheets
    if (length(sheet_names) == 0) return()

    lapply(seq_along(sheet_names), function(i) {
      local({
        sheet_nm <- sheet_names[[i]]
        out_id <- paste0("download_sheet_", i)

        output[[out_id]] <- downloadHandler(
          filename = function() paste0(safe_filename(sheet_nm), ".csv"),
          content = function(file) {
            if (values$file_ext %in% c("xlsx", "xls")) {
              d <- read_data_file(
                values$file_path, values$file_ext, sheet_name = sheet_nm,
                has_header = input$has_header,
                first_col_names = input$first_col_names
              )
            } else {
              d <- values$sheets[[sheet_nm]]
            }
            write.csv(d, file, row.names = FALSE, na = "")
          }
        )
      })
    })
  })

  output$sheet_download_buttons <- renderUI({
    nms <- values$available_sheets
    if (length(nms) == 0) return(helpText("Upload a CSV or Excel workbook first."))

    tagList(lapply(seq_along(nms), function(i) {
      nm <- nms[[i]]
      div(
        class = "sheet-download",
        downloadButton(
          paste0("download_sheet_", i),
          paste0("Download ", nm),
          class = "btn-sm btn-success"
        )
      )
    }))
  })

  output$download_all_sheets <- downloadHandler(
    filename = function() {
      if (values$file_ext %in% c("xlsx", "xls")) {
        paste0("workbook_sheets_", Sys.Date(), ".zip")
      } else {
        paste0("data_sheets_", Sys.Date(), ".zip")
      }
    },
    content = function(file) {
      req(length(values$available_sheets) > 0)

      td <- tempfile("sheet_export_")
      dir.create(td)
      on.exit(unlink(td, recursive = TRUE), add = TRUE)

      csv_files <- vapply(values$available_sheets, function(nm) {
        if (values$file_ext %in% c("xlsx", "xls")) {
          d <- read_data_file(
            values$file_path, values$file_ext, sheet_name = nm,
            has_header = input$has_header,
            first_col_names = input$first_col_names
          )
        } else {
          d <- values$sheets[[nm]]
        }

        path <- file.path(td, paste0(safe_filename(nm), ".csv"))
        write.csv(d, path, row.names = FALSE, na = "")
        path
      }, character(1))

      old <- setwd(td)
      on.exit(setwd(old), add = TRUE)
      utils::zip(file, files = basename(csv_files))
    }
  )

  # ---- Upload displays -------------------------------------------------------
  output$data_summary <- renderText({
    req(values$raw_data)
    paste0(
      "Active sheet: ", values$active_sheet,
      " | Rows: ", nrow(values$raw_data),
      " | Columns: ", ncol(values$raw_data),
      " | Loaded sheets: ", length(values$sheets),
      if (values$file_ext %in% c("xlsx", "xls")) {
        paste0(" | Workbook sheets: ", length(values$available_sheets))
      } else {
        ""
      }
    )
  })

  output$loaded_sheets_table <- renderDT({
    req(length(values$sheets) > 0)
    df <- data.frame(
      Sheet = names(values$sheets),
      Rows = vapply(values$sheets, nrow, integer(1)),
      Columns = vapply(values$sheets, ncol, integer(1)),
      Active = names(values$sheets) == values$active_sheet,
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })

  output$upload_data_summary <- renderDT({
    req(values$raw_data)
    rows <- lapply(names(values$raw_data), function(nm) {
      x <- values$raw_data[[nm]]
      nonmissing <- x[!is.na(x)]
      samples <- paste(head(unique(nonmissing), 3), collapse = ", ")
      if (nchar(samples) > 70) samples <- paste0(substr(samples, 1, 67), "...")
      data.frame(
        Column = nm,
        Type = class(x)[1],
        Count = length(x),
        Missing = sum(is.na(x)),
        Missing_Percent = round(mean(is.na(x)) * 100, 1),
        Unique = length(unique(nonmissing)),
        Sample_Values = samples,
        stringsAsFactors = FALSE
      )
    })
    datatable(do.call(rbind, rows), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15)) %>%
      formatStyle("Missing_Percent", backgroundColor = styleInterval(c(10, 30), c("lightgreen", "khaki", "lightcoral")))
  })

  output$uploaded_data_preview <- renderDT({
    req(values$raw_data)
    datatable(head(values$raw_data, 200), options = list(scrollX = TRUE, pageLength = 10))
  })

  # ---- Mapping ---------------------------------------------------------------
  output$column_analysis_table <- renderDT({
    req(values$column_info)
    df <- data.frame(
      Column = names(values$column_info),
      Type = vapply(values$column_info, `[[`, character(1), "type"),
      Unique_Count = vapply(values$column_info, `[[`, numeric(1), "unique_count"),
      Unique_Ratio_Percent = paste0(vapply(values$column_info, `[[`, numeric(1), "unique_ratio"), "%"),
      Suggested_Role = vapply(values$column_info, `[[`, character(1), "suggested_role"),
      Sample_Values = vapply(values$column_info, `[[`, character(1), "sample_values"),
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
  })

  observeEvent(input$confirm_mapping, {
    req(values$raw_data, input$entity_column)
    if (!input$entity_column %in% names(values$raw_data)) {
      showNotification("Please select a valid entity column.", type = "error")
      return()
    }

    values$entity_column <- input$entity_column
    values$time_column <- if (!is.null(input$time_column) && nzchar(input$time_column)) input$time_column else NULL
    values$identifier_column <- if (!is.null(input$identifier_column) && nzchar(input$identifier_column)) input$identifier_column else NULL
    values$mapping_confirmed <- TRUE

    mapped <- na.omit(c(values$entity_column, values$time_column, values$identifier_column))
    numeric_cols <- names(values$raw_data)[vapply(values$raw_data, is.numeric, logical(1))]
    indicators <- setdiff(numeric_cols, mapped)
    updateCheckboxGroupInput(session, "indicators_select", choices = indicators, selected = indicators)

    showNotification("Column mapping confirmed.", type = "message")
  })

  observeEvent(input$select_all_indicators, {
    req(values$raw_data)
    mapped <- na.omit(c(values$entity_column, values$time_column, values$identifier_column))
    numeric_cols <- names(values$raw_data)[vapply(values$raw_data, is.numeric, logical(1))]
    updateCheckboxGroupInput(session, "indicators_select", selected = setdiff(numeric_cols, mapped))
  })

  observeEvent(input$clear_all_indicators, {
    updateCheckboxGroupInput(session, "indicators_select", selected = character(0))
  })

  # ---- Processing ------------------------------------------------------------
  output$indicator_directions_ui <- renderUI({
    inds <- input$indicators_select
    if (is.null(inds) || length(inds) == 0) return(helpText("Select indicators first."))

    tagList(lapply(inds, function(ind) {
      div(
        style = "padding: 6px; border: 1px solid #ddd; border-radius: 4px; margin-bottom: 5px;",
        fluidRow(
          column(6, strong(ind)),
          column(6, selectInput(
            paste0("dir_", safe_id(ind)), NULL,
            choices = c("Higher is better" = "higher", "Lower is better" = "lower"),
            selected = "higher", width = "100%"
          ))
        )
      )
    }))
  })

  observeEvent(input$process_data, {
    req(values$raw_data, values$mapping_confirmed, input$indicators_select)
    inds <- input$indicators_select
    if (length(inds) == 0) {
      showNotification("Select at least one numeric indicator.", type = "warning")
      return()
    }

    d <- values$raw_data

    # Minimum rows per entity.
    if (!is.null(values$entity_column) && input$min_data_points > 1) {
      keep_entities <- d %>%
        group_by(.data[[values$entity_column]]) %>%
        summarise(n_rows = n(), .groups = "drop") %>%
        filter(n_rows >= input$min_data_points) %>%
        pull(.data[[values$entity_column]])
      d <- d %>% filter(.data[[values$entity_column]] %in% keep_entities)
    }

    # Missing values.
    d <- tryCatch(
      impute_selected_indicators(d, inds, input$missing_method, values$entity_column, values$time_column),
      error = function(e) {
        showNotification(paste("Missing-data processing failed:", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(d) || nrow(d) == 0) {
      showNotification("No rows remain after processing.", type = "error")
      return()
    }

    # Direction.
    if (input$direction_option == "lower") {
      for (ind in inds) d[[ind]] <- -as.numeric(d[[ind]])
    } else if (input$direction_option == "mixed") {
      for (ind in inds) {
        dir_val <- input[[paste0("dir_", safe_id(ind))]] %||% "higher"
        if (dir_val == "lower") d[[ind]] <- -as.numeric(d[[ind]])
      }
    }

    # Normalization.
    for (ind in inds) {
      if (input$normalization_method == "minmax") d[[ind]] <- minmax_scale(d[[ind]])
      if (input$normalization_method == "zscore") d[[ind]] <- zscore_scale(d[[ind]])
    }

    eq_w <- rep(1, length(inds))
    names(eq_w) <- inds
    d$composite_index <- calculate_composite_index_universal(d, eq_w, inds)

    values$selected_indicators <- inds
    values$processed_data <- d
    values$processed_equal <- d
    values$custom_weights <- eq_w
    values$weights_applied <- FALSE
    group_definitions(list())
    group_index_result(NULL)

    update_analysis_choices()
    showNotification("Data processed successfully.", type = "message")
  })

  output$processed_data_table <- renderDT({
    req(values$processed_data)
    datatable(head(values$processed_data, 200), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$data_summary_table <- renderDT({
    req(values$processed_data)
    rows <- lapply(names(values$processed_data), function(nm) {
      x <- values$processed_data[[nm]]
      if (is.numeric(x)) {
        data.frame(
          Column = nm, Type = "Numeric", Count = length(x), Missing = sum(is.na(x)),
          Unique = length(unique(x[!is.na(x)])), Mean = round(mean(x, na.rm = TRUE), 4),
          Median = round(median(x, na.rm = TRUE), 4), Min = round(min(x, na.rm = TRUE), 4),
          Max = round(max(x, na.rm = TRUE), 4), SD = round(sd(x, na.rm = TRUE), 4),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          Column = nm, Type = class(x)[1], Count = length(x), Missing = sum(is.na(x)),
          Unique = length(unique(x[!is.na(x)])), Mean = NA, Median = NA, Min = NA, Max = NA, SD = NA,
          stringsAsFactors = FALSE
        )
      }
    })
    datatable(do.call(rbind, rows), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
  })

  # ---- Custom weighting ------------------------------------------------------
  output$weight_inputs <- renderUI({
    inds <- values$selected_indicators
    if (is.null(inds) || length(inds) == 0) return(h4("Process data first.", style = "color:white;"))

    tagList(lapply(inds, function(ind) {
      current <- if (!is.null(values$custom_weights) && ind %in% names(values$custom_weights)) values$custom_weights[[ind]] else 1
      div(
        class = "weight-input",
        fluidRow(
          column(7, tags$label(ind, style = "color:white;font-weight:bold;")),
          column(5, numericInput(paste0("main_weight_", safe_id(ind)), NULL, value = current, min = 0, step = 0.1))
        )
      )
    }))
  })

  observeEvent(input$set_equal_weights, {
    req(values$selected_indicators)
    for (ind in values$selected_indicators) {
      updateNumericInput(session, paste0("main_weight_", safe_id(ind)), value = 1)
    }
  })

  observeEvent(input$apply_weights, {
    req(values$processed_equal, values$selected_indicators)
    inds <- values$selected_indicators
    new_w <- vapply(inds, function(ind) input[[paste0("main_weight_", safe_id(ind))]] %||% 1, numeric(1))
    if (all(new_w <= 0) || any(!is.finite(new_w))) {
      showNotification("Weights must be finite and at least one must be positive.", type = "error")
      return()
    }
    names(new_w) <- inds

    d <- values$processed_equal
    d$composite_index <- calculate_composite_index_universal(d, new_w, inds)
    values$processed_data <- d
    values$custom_weights <- new_w
    values$weights_applied <- TRUE
    update_analysis_choices()

    showNotification("Custom weights applied.", type = "message")
  })

  output$weights_summary <- renderDT({
    req(values$custom_weights)
    w <- values$custom_weights
    df <- data.frame(
      Indicator = names(w),
      Raw_Weight = as.numeric(w),
      Normalized_Weight = normalize_weights(w),
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 20)) %>%
      formatRound(c("Raw_Weight", "Normalized_Weight"), 4)
  })

  rank_comparison_data <- reactive({
    req(values$processed_data, values$processed_equal, values$entity_column)
    eq <- entity_score_table(values$processed_equal, values$entity_column, values$time_column, "composite_index", input$ranking_mode)
    cu <- entity_score_table(values$processed_data, values$entity_column, values$time_column, "composite_index", input$ranking_mode)
    if (is.null(eq) || is.null(cu)) return(NULL)

    eq <- eq %>% rename(Equal_Score = Score, Equal_Rank = Rank)
    cu <- cu %>% rename(Custom_Score = Score, Custom_Rank = Rank)
    inner_join(eq, cu, by = "Entity") %>%
      mutate(Rank_Change = Equal_Rank - Custom_Rank, Score_Change = Custom_Score - Equal_Score) %>%
      arrange(desc(abs(Rank_Change)))
  })

  output$weight_impact_plot <- renderPlotly({
    req(values$weights_applied)
    d <- rank_comparison_data()
    if (is.null(d) || nrow(d) == 0) return(plotly_empty() %>% layout(title = "No ranking comparison available"))
    d <- head(d, 20)
    p <- ggplot(d, aes(x = reorder(Entity, abs(Rank_Change)), y = Rank_Change, fill = Rank_Change > 0)) +
      geom_col() + coord_flip() + theme_minimal() +
      labs(title = "Largest ranking changes", x = "Entity", y = "Equal rank - Custom rank", fill = "Improved")
    ggplotly(p)
  })

  output$weight_comparison_table <- renderDT({
    req(values$weights_applied)
    d <- rank_comparison_data()
    if (is.null(d)) return(NULL)
    datatable(head(d, 50), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15)) %>%
      formatRound(c("Equal_Score", "Custom_Score", "Score_Change"), 4)
  })

  # ---- Main analysis ---------------------------------------------------------
  current_entity_ranking <- reactive({
    req(values$processed_data, values$entity_column)
    entity_score_table(values$processed_data, values$entity_column, values$time_column,
                       "composite_index", input$ranking_mode)
  })

  output$ranking_plot <- renderPlotly({
    d <- current_entity_ranking()
    if (is.null(d) || nrow(d) == 0) return(plotly_empty() %>% layout(title = "No ranking data"))
    d <- head(d, 20)
    p <- ggplot(d, aes(x = reorder(Entity, Score), y = Score)) +
      geom_col(fill = "steelblue", alpha = 0.85) + coord_flip() + theme_minimal() +
      labs(title = "Top 20 entities by composite index", x = "Entity", y = "Composite Index")
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$entity_ranking_table <- renderDT({
    d <- current_entity_ranking()
    if (is.null(d)) return(NULL)
    datatable(d %>% select(Rank, Entity, Score), rownames = FALSE, options = list(pageLength = 15)) %>%
      formatRound("Score", 4)
  })

  output$correlation_plot <- renderPlotly({
    req(values$processed_data, values$selected_indicators)
    cols <- unique(c("composite_index", values$selected_indicators))
    X <- values$processed_data[, cols, drop = FALSE]
    if (ncol(X) < 2) return(plotly_empty() %>% layout(title = "Need at least two numeric variables"))
    cm <- suppressWarnings(cor(X, use = "pairwise.complete.obs"))
    long <- expand.grid(Var1 = rownames(cm), Var2 = colnames(cm), stringsAsFactors = FALSE)
    long$value <- as.vector(cm)
    p <- ggplot(long, aes(Var1, Var2, fill = value, text = paste0("Correlation: ", round(value, 3)))) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-1, 1)) +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = NULL, y = NULL, fill = "r", title = "Correlation matrix")
    ggplotly(p, tooltip = "text")
  })

  output$composite_results_table <- renderDT({
    req(values$processed_data)
    d <- values$processed_data
    show_cols <- unique(c(values$entity_column, values$time_column, values$identifier_column,
                          "composite_index", values$selected_indicators))
    show_cols <- show_cols[!is.na(show_cols) & nzchar(show_cols) & show_cols %in% names(d)]
    d <- d[, show_cols, drop = FALSE]
    d <- d[order(d$composite_index, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    d$Row_Rank <- seq_len(nrow(d))
    d <- d[, c("Row_Rank", setdiff(names(d), "Row_Rank")), drop = FALSE]
    datatable(d, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 25))
  })

  output$download_processed <- downloadHandler(
    filename = function() paste0(safe_filename(values$active_sheet %||% "processed"), "_processed_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$processed_data)
      write.csv(values$processed_data, file, row.names = FALSE, na = "")
    }
  )

  # ---- Diagnostics -----------------------------------------------------------
  diagnostic_matrix <- reactive({
    req(values$processed_data, values$selected_indicators)
    X <- values$processed_data[, values$selected_indicators, drop = FALSE]
    X[, vapply(X, is.numeric, logical(1)), drop = FALSE]
  })

  output$cronbach <- renderPrint({
    X <- diagnostic_matrix()
    if (ncol(X) < 2) {
      cat("Cronbach alpha requires at least two indicators.")
      return()
    }
    Xc <- X[complete.cases(X), , drop = FALSE]
    if (nrow(Xc) < 3) {
      cat("Insufficient complete rows for Cronbach alpha.")
      return()
    }
    result <- tryCatch(psych::alpha(Xc, warnings = FALSE, check.keys = FALSE), error = function(e) NULL)
    if (is.null(result)) {
      cat("Cronbach alpha could not be calculated.")
      return()
    }
    a <- result$total$std.alpha
    interpretation <- if (a >= .9) "Excellent" else if (a >= .8) "Good" else if (a >= .7) "Acceptable" else if (a >= .6) "Questionable" else "Poor"
    cat("Standardized Cronbach alpha:", round(a, 4), "\nInterpretation:", interpretation,
        "\n\nUse this only when the indicators are intended to measure a common latent construct.")
  })

  output$cv_table <- renderDT({
    X <- diagnostic_matrix()
    df <- data.frame(
      Indicator = names(X),
      Mean = vapply(X, mean, numeric(1), na.rm = TRUE),
      SD = vapply(X, sd, numeric(1), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    df$CV <- ifelse(abs(df$Mean) > .Machine$double.eps, df$SD / abs(df$Mean), NA_real_)
    datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 20)) %>% formatRound(c("Mean", "SD", "CV"), 4)
  })

  pca_object <- reactive({
    X <- diagnostic_matrix()
    sds <- vapply(X, sd, numeric(1), na.rm = TRUE)
    X <- X[, is.finite(sds) & sds > 0, drop = FALSE]
    X <- X[complete.cases(X), , drop = FALSE]
    if (ncol(X) < 2 || nrow(X) < 3) return(NULL)
    tryCatch(prcomp(X, center = TRUE, scale. = TRUE), error = function(e) NULL)
  })

  output$pca_summary <- renderPrint({
    p <- pca_object()
    if (is.null(p)) cat("PCA requires at least two non-constant indicators and three complete observations.") else print(summary(p))
  })

  output$pca_plot <- renderPlot({
    p <- pca_object()
    if (is.null(p)) {
      plot.new(); title("PCA unavailable")
    } else {
      biplot(p, main = "PCA Biplot")
    }
  })

  output$sensitivity_table <- renderDT({
    req(values$processed_data, values$selected_indicators, values$custom_weights)
    inds <- values$selected_indicators
    base_w <- values$custom_weights
    base_idx <- calculate_composite_index_universal(values$processed_data, base_w, inds)

    rows <- lapply(seq_along(inds), function(i) {
      w2 <- base_w
      w2[i] <- w2[i] * 1.05
      idx2 <- calculate_composite_index_universal(values$processed_data, w2, inds)
      data.frame(
        Indicator = inds[i],
        Pearson_Score_Correlation = suppressWarnings(cor(base_idx, idx2, use = "complete.obs", method = "pearson")),
        Spearman_Rank_Correlation = suppressWarnings(cor(base_idx, idx2, use = "complete.obs", method = "spearman")),
        stringsAsFactors = FALSE
      )
    })
    datatable(do.call(rbind, rows), rownames = FALSE, options = list(pageLength = 20)) %>%
      formatRound(c("Pearson_Score_Correlation", "Spearman_Rank_Correlation"), 5)
  })

  output$sensitivity_heatmap <- renderPlot({
    req(values$processed_data)
    numeric_data <- values$processed_data[, vapply(values$processed_data, is.numeric, logical(1)), drop = FALSE]
    if (ncol(numeric_data) < 2) {
      plot.new(); title("Need at least two numeric columns")
      return()
    }
    cm <- suppressWarnings(cor(numeric_data, use = "pairwise.complete.obs"))
    corrplot::corrplot(cm, method = "color", type = "upper", order = "hclust", addCoef.col = "black",
                       tl.col = "black", tl.srt = 45, number.cex = 0.7,
                       title = "Correlation Matrix", mar = c(0, 0, 2, 0))
  })

  output$sankey <- renderSankeyNetwork({
    req(values$custom_weights)
    w <- normalize_weights(values$custom_weights)
    links <- data.frame(source = rep(0, length(w)), target = seq_along(w), value = as.numeric(w))
    nodes <- data.frame(name = c("Composite Index", paste0(names(w), " (", round(w, 3), ")")))
    networkD3::sankeyNetwork(
      Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "value", NodeID = "name",
      fontSize = 12, nodeWidth = 28
    )
  })

  # ---- Time series -----------------------------------------------------------
  output$timeseries_plot <- renderPlotly({
    req(values$processed_data, values$time_column, input$ts_entity, input$ts_metric)
    d <- values$processed_data %>%
      filter(as.character(.data[[values$entity_column]]) == as.character(input$ts_entity)) %>%
      arrange(.data[[values$time_column]])
    if (nrow(d) == 0) return(plotly_empty() %>% layout(title = "No data"))
    p <- ggplot(d, aes(x = .data[[values$time_column]], y = .data[[input$ts_metric]])) +
      geom_line(color = "steelblue", linewidth = 1) + geom_point(color = "navy", size = 2) +
      theme_minimal() + labs(title = paste("Time series:", input$ts_entity), x = values$time_column, y = input$ts_metric)
    ggplotly(p)
  })

  output$multi_timeseries_plot <- renderPlotly({
    req(values$processed_data, values$time_column, input$multi_ts_entities, input$multi_ts_metric)
    ents <- input$multi_ts_entities
    if (length(ents) > 10) {
      showNotification("Please select at most 10 entities.", type = "warning")
      ents <- ents[1:10]
    }
    d <- values$processed_data %>%
      filter(as.character(.data[[values$entity_column]]) %in% as.character(ents)) %>%
      arrange(.data[[values$time_column]])
    if (nrow(d) == 0) return(plotly_empty() %>% layout(title = "No data"))
    p <- ggplot(d, aes(x = .data[[values$time_column]], y = .data[[input$multi_ts_metric]], color = .data[[values$entity_column]])) +
      geom_line(linewidth = 1) + geom_point(size = 1.5) + theme_minimal() +
      labs(title = paste("Multi-entity time series:", input$multi_ts_metric), color = "Entity")
    ggplotly(p)
  })

  # ---- Forecasting -----------------------------------------------------------
  observeEvent(input$generate_forecast, {
    req(values$processed_data, values$time_column, input$forecast_entity, input$forecast_metric)
    d <- values$processed_data %>%
      filter(as.character(.data[[values$entity_column]]) == as.character(input$forecast_entity)) %>%
      arrange(.data[[values$time_column]])

    if (nrow(d) == 0) {
      showNotification("No data for the selected entity.", type = "warning")
      return()
    }

    valid <- is.finite(as.numeric(d[[input$forecast_metric]]))
    d_valid <- d[valid, , drop = FALSE]
    fc <- forecast_series(d_valid[[input$forecast_metric]], input$forecast_periods, input$forecast_frequency)
    future_t <- future_time_values(d_valid[[values$time_column]], length(fc$forecast))

    values$forecast_results <- list(
      entity = input$forecast_entity,
      metric = input$forecast_metric,
      historical = d_valid,
      future_time = future_t,
      forecast = fc
    )
    showNotification(paste("Forecast generated using", fc$method), type = "message")
  })

  output$forecast_plot <- renderPlotly({
    req(values$forecast_results)
    r <- values$forecast_results
    hist <- r$historical
    fc <- r$forecast

    # Use a continuous index for robust plotting while keeping period labels in hover/table.
    hist_n <- nrow(hist)
    plot_df <- data.frame(
      Index = c(seq_len(hist_n), hist_n + seq_along(fc$forecast)),
      Value = c(hist[[r$metric]], fc$forecast),
      Type = c(rep("Historical", hist_n), rep("Forecast", length(fc$forecast))),
      Lower = c(rep(NA_real_, hist_n), fc$lower),
      Upper = c(rep(NA_real_, hist_n), fc$upper),
      Period = c(as.character(hist[[values$time_column]]), as.character(r$future_time)),
      stringsAsFactors = FALSE
    )

    p <- ggplot(plot_df, aes(Index, Value, color = Type, text = paste0("Period: ", Period, "<br>Value: ", round(Value, 4)))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      geom_ribbon(
        data = subset(plot_df, Type == "Forecast"),
        mapping = aes(x = Index, ymin = Lower, ymax = Upper),
        inherit.aes = FALSE, alpha = 0.15
      ) +
      theme_minimal() + labs(
        title = paste("Forecast for", r$entity),
        subtitle = paste("Metric:", r$metric, "| Method:", fc$method),
        x = "Time order", y = r$metric
      )
    ggplotly(p, tooltip = "text")
  })

  output$forecast_table <- renderDT({
    req(values$forecast_results)
    r <- values$forecast_results
    df <- data.frame(
      Period = as.character(r$future_time),
      Forecast = r$forecast$forecast,
      Lower_95 = r$forecast$lower,
      Upper_95 = r$forecast$upper,
      Method = r$forecast$method,
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE, options = list(pageLength = 15)) %>%
      formatRound(c("Forecast", "Lower_95", "Upper_95"), 4)
  })

  observeEvent(input$generate_multi_forecast, {
    req(values$processed_data, values$time_column, input$multi_forecast_entities, input$forecast_metric)
    ents <- input$multi_forecast_entities
    if (length(ents) == 0) return()
    if (length(ents) > 12) {
      showNotification("Multi-forecast is limited to 12 entities for readability.", type = "warning")
      ents <- ents[1:12]
    }

    all_df <- list()
    for (ent in ents) {
      d <- values$processed_data %>%
        filter(as.character(.data[[values$entity_column]]) == as.character(ent)) %>%
        arrange(.data[[values$time_column]])
      valid <- is.finite(as.numeric(d[[input$forecast_metric]]))
      d <- d[valid, , drop = FALSE]
      if (nrow(d) == 0) next

      fc <- forecast_series(d[[input$forecast_metric]], input$forecast_periods, input$forecast_frequency)
      future_t <- future_time_values(d[[values$time_column]], length(fc$forecast))

      all_df[[paste0(ent, "_hist")]] <- data.frame(
        Entity = as.character(ent), Sequence = seq_len(nrow(d)), Period = as.character(d[[values$time_column]]),
        Value = d[[input$forecast_metric]], Type = "Historical", stringsAsFactors = FALSE
      )
      all_df[[paste0(ent, "_fc")]] <- data.frame(
        Entity = as.character(ent), Sequence = nrow(d) + seq_along(fc$forecast), Period = as.character(future_t),
        Value = fc$forecast, Type = "Forecast", stringsAsFactors = FALSE
      )
    }

    values$multi_forecast_data <- if (length(all_df) > 0) do.call(rbind, all_df) else NULL
  })

  output$multi_forecast_plot <- renderPlotly({
    req(values$multi_forecast_data)
    d <- values$multi_forecast_data
    p <- ggplot(d, aes(Sequence, Value, color = Entity, linetype = Type,
                       text = paste0("Entity: ", Entity, "<br>Period: ", Period, "<br>Value: ", round(Value, 4)))) +
      geom_line(linewidth = 1) + geom_point(size = 1.4) + theme_minimal() +
      labs(title = paste("Multi-entity forecast:", input$forecast_metric), x = "Time order", y = input$forecast_metric)
    ggplotly(p, tooltip = "text")
  })

  # ---- Comparison ------------------------------------------------------------
  comparison_data <- eventReactive(input$generate_comparison, {
    req(values$processed_data, input$compare_entities, input$compare_metric)
    d <- values$processed_data %>%
      filter(as.character(.data[[values$entity_column]]) %in% as.character(input$compare_entities))

    if (!is.null(values$time_column) && !is.null(input$compare_time_period) && nzchar(input$compare_time_period)) {
      d <- d %>% filter(as.character(.data[[values$time_column]]) == as.character(input$compare_time_period))
    }
    d
  }, ignoreNULL = FALSE)

  output$comparison_plot <- renderPlotly({
    d <- comparison_data()
    if (is.null(d) || nrow(d) == 0) return(plotly_empty() %>% layout(title = "No comparison data"))

    # Aggregate if multiple rows remain for the same entity.
    agg <- d %>%
      group_by(.data[[values$entity_column]]) %>%
      summarise(Value = mean(.data[[input$compare_metric]], na.rm = TRUE), .groups = "drop")
    names(agg)[names(agg) == values$entity_column] <- "Entity"
    agg <- agg %>% arrange(desc(Value))

    p <- ggplot(agg, aes(x = reorder(as.character(Entity), Value), y = Value)) +
      geom_col(fill = "steelblue", alpha = 0.85) + coord_flip() + theme_minimal() +
      geom_text(aes(label = round(Value, 3)), hjust = -0.1, size = 3) +
      labs(title = paste("Entity comparison:", input$compare_metric), x = "Entity", y = input$compare_metric)
    ggplotly(p)
  })

  output$comparison_table <- renderDT({
    d <- comparison_data()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    cols <- unique(c(values$entity_column, values$time_column, "composite_index", values$selected_indicators))
    cols <- cols[!is.na(cols) & nzchar(cols) & cols %in% names(d)]
    d <- d[, cols, drop = FALSE]
    d <- d[order(d[[input$compare_metric]], decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    datatable(d, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
  })

  output$comparison_summary <- renderText({
    d <- comparison_data()
    if (is.null(d) || nrow(d) == 0) return("No data available for the selected comparison.")
    agg <- d %>%
      group_by(.data[[values$entity_column]]) %>%
      summarise(Value = mean(.data[[input$compare_metric]], na.rm = TRUE), .groups = "drop")
    names(agg)[names(agg) == values$entity_column] <- "Entity"
    agg <- agg[is.finite(agg$Value), , drop = FALSE]
    if (nrow(agg) == 0) return("No valid metric values.")
    best_i <- which.max(agg$Value)
    worst_i <- which.min(agg$Value)
    paste0(
      "Best: ", agg$Entity[best_i], " (", round(agg$Value[best_i], 4), ")\n",
      "Worst: ", agg$Entity[worst_i], " (", round(agg$Value[worst_i], 4), ")\n",
      "Average across entities: ", round(mean(agg$Value, na.rm = TRUE), 4), "\n",
      "Range: ", round(diff(range(agg$Value, na.rm = TRUE)), 4)
    )
  })

  # ---- Pillars ---------------------------------------------------------------
  observeEvent(input$add_pillar, {
    req(values$processed_data, input$group_name, input$group_indicators)
    nm <- trimws(input$group_name)
    if (!nzchar(nm)) {
      showNotification("Enter a pillar name.", type = "warning")
      return()
    }
    if (length(input$group_indicators) == 0) {
      showNotification("Select at least one indicator for the pillar.", type = "warning")
      return()
    }

    groups <- group_definitions()
    groups[[nm]] <- list(name = nm, indicators = input$group_indicators, weight = input$group_weight)
    group_definitions(groups)
    group_index_result(NULL)

    updateTextInput(session, "group_name", value = "")
    updateSelectizeInput(session, "group_indicators", selected = character(0))
    updateNumericInput(session, "group_weight", value = 1)
    showNotification(paste("Pillar", nm, "saved."), type = "message")
  })

  observeEvent(input$clear_pillars, {
    group_definitions(list())
    group_index_result(NULL)
  })

  observeEvent(input$remove_pillar, {
    groups <- group_definitions()
    groups[[input$remove_pillar]] <- NULL
    group_definitions(groups)
    group_index_result(NULL)
  })

  output$pillars_display <- renderUI({
    groups <- group_definitions()
    if (length(groups) == 0) return(helpText("No pillars defined yet."))

    tagList(lapply(names(groups), function(nm) {
      g <- groups[[nm]]
      div(
        class = "panel panel-default",
        div(class = "panel-heading", strong(nm)),
        div(
          class = "panel-body",
          p(strong("Weight: "), g$weight),
          p(strong("Indicators: "), paste(g$indicators, collapse = ", ")),
          actionButton(
            paste0("remove_btn_", safe_id(nm)), "Remove", class = "btn-danger btn-xs",
            onclick = sprintf("Shiny.setInputValue('remove_pillar', %s, {priority: 'event'});", jsonlite::toJSON(nm, auto_unbox = TRUE))
          )
        )
      )
    }))
  })

  output$custom_weights_tables <- renderUI({
    groups <- group_definitions()
    if (length(groups) == 0) return(helpText("Define pillars first."))

    tagList(lapply(names(groups), function(gname) {
      g <- groups[[gname]]
      div(
        style = "border:1px solid #ddd;padding:10px;margin-bottom:10px;border-radius:5px;",
        h5(gname),
        tagList(lapply(g$indicators, function(ind) {
          fluidRow(
            column(8, p(ind, style = "margin-top:7px;")),
            column(4, numericInput(
              paste0("pillar_w_", safe_id(gname), "_", safe_id(ind)), NULL,
              value = 1, min = 0, step = 0.1, width = "100%"
            ))
          )
        }))
      )
    }))
  })

  output$pillar_weights_plot <- renderPlotly({
    groups <- group_definitions()
    if (length(groups) == 0) return(plotly_empty() %>% layout(title = "No pillars"))
    df <- data.frame(Pillar = names(groups), Weight = vapply(groups, function(g) g$weight, numeric(1)))
    p <- ggplot(df, aes(Pillar, Weight)) + geom_col(fill = "steelblue") + theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Raw pillar weights")
    ggplotly(p)
  })

  observeEvent(input$calculate_pillar_index, {
    req(values$processed_data)
    groups <- group_definitions()
    if (length(groups) == 0) {
      showNotification("Define at least one pillar.", type = "warning")
      return()
    }

    custom_provider <- function(gname, indicators) {
      vapply(indicators, function(ind) {
        input[[paste0("pillar_w_", safe_id(gname), "_", safe_id(ind))]] %||% 1
      }, numeric(1))
    }

    result <- tryCatch(
      calculate_group_based_index(values$processed_data, groups, input$within_pillar_method, custom_provider),
      error = function(e) {
        showNotification(paste("Pillar calculation failed:", e$message), type = "error", duration = 8)
        NULL
      }
    )

    if (!is.null(result)) {
      group_index_result(result)
      showNotification("Pillar-based composite index calculated.", type = "message")
    }
  })

  output$pillar_index_table <- renderDT({
    r <- group_index_result()
    req(r)
    base_cols <- unique(c(values$entity_column, values$time_column, values$identifier_column))
    base_cols <- base_cols[!is.na(base_cols) & nzchar(base_cols) & base_cols %in% names(values$processed_data)]
    df <- values$processed_data[, base_cols, drop = FALSE]
    df$Pillar_Based_Index <- r$overall_index
    for (nm in names(r$group_indices)) df[[paste0("Pillar_", safe_id(nm))]] <- r$group_indices[[nm]]$index
    datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 20))
  })

  output$pillar_contribution_plot <- renderPlotly({
    r <- group_index_result(); req(r)
    df <- data.frame(Pillar = names(r$group_weights), Contribution = 100 * as.numeric(r$group_weights))
    plot_ly(df, labels = ~Pillar, values = ~Contribution, type = "pie", textinfo = "label+percent") %>%
      layout(title = "Pillar contribution to overall index")
  })

  output$pillar_performance_plot <- renderPlotly({
    r <- group_index_result(); req(r)
    df <- do.call(rbind, lapply(names(r$group_indices), function(nm) {
      x <- r$group_indices[[nm]]$index
      data.frame(Pillar = nm, Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE))
    }))
    p <- ggplot(df, aes(Pillar, Mean)) + geom_col(fill = "steelblue") +
      geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = .2) +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Pillar performance", y = "Mean sub-index")
    ggplotly(p)
  })

  output$pillar_stats_table <- renderDT({
    r <- group_index_result(); req(r)
    datatable(create_group_statistics(r), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 20))
  })

  output$pillar_correlation_plot <- renderPlotly({
    r <- group_index_result(); req(r)
    if (length(r$group_indices) < 2) return(plotly_empty() %>% layout(title = "At least two pillars are required"))
    df <- as.data.frame(do.call(cbind, lapply(r$group_indices, `[[`, "index")))
    names(df) <- names(r$group_indices)
    df$Overall <- r$overall_index
    cm <- suppressWarnings(cor(df, use = "pairwise.complete.obs"))
    long <- expand.grid(Var1 = rownames(cm), Var2 = colnames(cm), stringsAsFactors = FALSE)
    long$value <- as.vector(cm)
    p <- ggplot(long, aes(Var1, Var2, fill = value, text = paste0("r = ", round(value, 3)))) +
      geom_tile(color = "white") + scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-1, 1)) +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x = NULL, y = NULL, fill = "r")
    ggplotly(p, tooltip = "text")
  })
}

# ---- Launch ------------------------------------------------------------------
shinyApp(ui = ui, server = server)
