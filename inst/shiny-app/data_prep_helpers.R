# Data preparation helpers for compIndexBuilder Shiny app.
# These helpers intentionally use base R only so they are easy to test and
# remain available before the rest of the application is initialised.

parse_missing_tokens <- function(x) {
  if (is.null(x) || length(x) == 0 || !nzchar(trimws(x))) {
    return(character(0))
  }
  tokens <- trimws(unlist(strsplit(x, "[,;|]", perl = TRUE), use.names = FALSE))
  unique(tokens[nzchar(tokens)])
}

standardize_missing_values <- function(data,
                                       missing_tokens = c("#N/A", "N/A", "NA", "..", "...", "NULL", "null"),
                                       zero_is_missing = FALSE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  missing_tokens <- unique(trimws(as.character(missing_tokens)))
  missing_tokens <- missing_tokens[nzchar(missing_tokens)]
  missing_tokens_upper <- toupper(missing_tokens)

  token_replacements <- 0L
  zero_replacements <- 0L

  for (nm in names(data)) {
    x <- data[[nm]]

    if (is.factor(x)) x <- as.character(x)

    if (is.character(x)) {
      trimmed <- trimws(x)
      is_blank <- !is.na(trimmed) & trimmed == ""
      is_token <- !is.na(trimmed) & toupper(trimmed) %in% missing_tokens_upper
      replace_idx <- is_blank | is_token
      token_replacements <- token_replacements + sum(replace_idx, na.rm = TRUE)
      x[replace_idx] <- NA_character_
      data[[nm]] <- x
    }
  }

  # Numeric conversion is done after missing codes are standardised. This
  # prevents placeholders such as '..' from forcing an otherwise numeric
  # indicator column to remain character.
  for (nm in names(data)) {
    if (is.character(data[[nm]])) {
      x <- data[[nm]]
      raw_nonmissing <- !is.na(x) & trimws(x) != ""
      if (sum(raw_nonmissing) > 0) {
        numeric_test <- suppressWarnings(as.numeric(gsub(",", "", x, fixed = TRUE)))
        numeric_ratio <- sum(!is.na(numeric_test) & raw_nonmissing) / sum(raw_nonmissing)
        if (is.finite(numeric_ratio) && numeric_ratio >= 0.8) {
          data[[nm]] <- numeric_test
        }
      }
    }
  }

  if (isTRUE(zero_is_missing)) {
    for (nm in names(data)) {
      if (is.numeric(data[[nm]])) {
        idx <- !is.na(data[[nm]]) & data[[nm]] == 0
        zero_replacements <- zero_replacements + sum(idx)
        data[[nm]][idx] <- NA_real_
      }
    }
  }

  attr(data, "missing_replacements") <- token_replacements
  attr(data, "zero_replacements") <- zero_replacements
  data
}

parse_indicator_year_name <- function(x) {
  x <- trimws(as.character(x))

  # Examples accepted: IN1-2019, IN1_2019, IN1.2019, IN1 2019,
  # IN1 (2019), and IN12019. A four-digit year must be at the end.
  m <- regexec("^(.*?)[[:space:]_.-]*\\(?((?:19|20)[0-9]{2})\\)?$", x, perl = TRUE)
  hit <- regmatches(x, m)[[1]]
  if (length(hit) != 3) return(NULL)

  indicator <- trimws(gsub("[[:space:]_.-]+$", "", hit[2], perl = TRUE))
  year <- suppressWarnings(as.integer(hit[3]))

  # Avoid treating a bare year or a purely numeric prefix as an indicator.
  if (!nzchar(indicator) || !grepl("[A-Za-z]", indicator) || !is.finite(year)) return(NULL)

  list(indicator = indicator, year = year)
}

indicator_year_column_map <- function(data) {
  nms <- names(data)
  rows <- lapply(seq_along(nms), function(i) {
    parsed <- parse_indicator_year_name(nms[i])
    if (is.null(parsed)) return(NULL)
    data.frame(
      Column = nms[i],
      Indicator = parsed$indicator,
      Year = parsed$year,
      Position = i,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(
      Column = character(0), Indicator = character(0), Year = integer(0),
      Position = integer(0), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

indicator_year_layout_info <- function(data) {
  map <- indicator_year_column_map(data)
  if (nrow(map) == 0) {
    return(list(detected = FALSE, map = map, indicators = character(0), years = integer(0),
                duplicate_pairs = character(0)))
  }

  indicators <- unique(map$Indicator)
  years <- sort(unique(map$Year))
  pair_key <- paste(map$Indicator, map$Year, sep = "::")
  duplicate_pairs <- unique(pair_key[duplicated(pair_key)])

  # Auto-detection is intentionally conservative: at least two indicators,
  # at least two years, at least four matching columns, and no duplicate
  # indicator-year pairs.
  detected <- length(indicators) >= 2 && length(years) >= 2 &&
    nrow(map) >= 4 && length(duplicate_pairs) == 0

  list(
    detected = detected,
    map = map,
    indicators = indicators,
    years = years,
    duplicate_pairs = duplicate_pairs
  )
}

reshape_indicator_year_wide <- function(data, force = FALSE) {
  info <- indicator_year_layout_info(data)
  map <- info$map

  if (nrow(map) == 0) {
    if (force) stop("No indicator-year column names were found. Expected names such as IN1-2019 or IN2_2020.")
    return(data)
  }

  if (length(info$duplicate_pairs) > 0) {
    stop(
      "Duplicate indicator-year pairs were found: ",
      paste(info$duplicate_pairs, collapse = ", "),
      ". Rename those columns so each indicator-year pair is unique."
    )
  }

  if (!force && !isTRUE(info$detected)) return(data)

  id_cols <- setdiff(names(data), map$Column)
  year_col <- if ("Year" %in% id_cols) "Indicator_Year" else "Year"
  indicators <- info$indicators
  years <- info$years

  original_row <- seq_len(nrow(data))
  pieces <- lapply(years, function(yr) {
    out <- data[, id_cols, drop = FALSE]
    out[[year_col]] <- yr
    out$.compIndex_original_row <- original_row

    for (ind in indicators) {
      hit <- map$Column[map$Indicator == ind & map$Year == yr]
      if (length(hit) == 1) {
        out[[ind]] <- data[[hit]]
      } else {
        out[[ind]] <- NA_real_
      }
    }
    out
  })

  out <- do.call(rbind, pieces)
  out <- out[order(out$.compIndex_original_row, out[[year_col]]), , drop = FALSE]
  out$.compIndex_original_row <- NULL
  rownames(out) <- NULL

  attr(out, "indicator_year_map") <- map
  attr(out, "indicator_year_reshaped") <- TRUE
  attr(out, "indicator_year_column") <- year_col
  out
}

prepare_imported_data <- function(data,
                                  layout = c("auto", "standard", "indicator_year"),
                                  missing_tokens = c("#N/A", "N/A", "NA", "..", "...", "NULL", "null"),
                                  zero_is_missing = FALSE,
                                  first_col_names = FALSE) {
  layout <- match.arg(layout)
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)

  # Drop fully empty rows/columns before other transformations.
  if (nrow(data) > 0 && ncol(data) > 0) {
    row_all_na <- apply(data, 1, function(r) all(is.na(r) | trimws(as.character(r)) == ""))
    if (any(row_all_na)) data <- data[!row_all_na, , drop = FALSE]

    col_all_na <- vapply(data, function(x) all(is.na(x) | trimws(as.character(x)) == ""), logical(1))
    if (any(col_all_na)) data <- data[, !col_all_na, drop = FALSE]
  }

  # Preserve meaningful headers such as IN1-2019. Only blank and duplicate
  # names are repaired; Shiny input IDs are sanitised separately where needed.
  nms <- trimws(as.character(names(data)))
  blank <- !nzchar(nms)
  if (any(blank)) nms[blank] <- paste0("Column_", which(blank))
  names(data) <- make.unique(nms, sep = "_dup")

  data <- standardize_missing_values(
    data,
    missing_tokens = missing_tokens,
    zero_is_missing = zero_is_missing
  )
  token_replacements <- attr(data, "missing_replacements")
  if (is.null(token_replacements)) token_replacements <- 0L
  zero_replacements <- attr(data, "zero_replacements")
  if (is.null(zero_replacements)) zero_replacements <- 0L

  layout_info <- indicator_year_layout_info(data)
  do_reshape <- identical(layout, "indicator_year") ||
    (identical(layout, "auto") && isTRUE(layout_info$detected))

  if (do_reshape) {
    data <- reshape_indicator_year_wide(data, force = identical(layout, "indicator_year"))
  }

  if (first_col_names && ncol(data) >= 1) {
    first_col <- as.character(data[[1]])
    valid_ids <- !is.na(first_col) & trimws(first_col) != ""
    if (all(valid_ids) && length(unique(first_col)) == nrow(data)) {
      rownames(data) <- first_col
    }
  }

  reshaped <- isTRUE(attr(data, "indicator_year_reshaped"))
  final_map <- if (reshaped) attr(data, "indicator_year_map") else layout_info$map
  year_col <- if (reshaped) attr(data, "indicator_year_column") else NULL

  attr(data, "compIndex_import_report") <- list(
    layout_requested = layout,
    indicator_year_detected = isTRUE(layout_info$detected),
    reshaped = reshaped,
    indicator_count = length(layout_info$indicators),
    year_count = length(layout_info$years),
    years = layout_info$years,
    matching_columns = nrow(layout_info$map),
    year_column = year_col,
    missing_token_replacements = as.integer(token_replacements),
    zero_replacements = as.integer(zero_replacements),
    zero_is_missing = isTRUE(zero_is_missing)
  )
  attr(data, "compIndex_indicator_year_map") <- final_map
  data
}
