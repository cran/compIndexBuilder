# Composite Index Builder & Analytics Platform
compIndexBuilder <- function(Edata){
# Helper function to convert named vectors to lists for JSON compatibility
safe_choices <- function(choices_vector) {
  if(is.null(names(choices_vector))) {
    return(choices_vector)
  } else {
    return(as.list(choices_vector))
  }
}

# Group-based composite index calculation function
calculate_group_based_index <- function(data, group_definitions, within_group_method = "equal", input = NULL) {
  if (length(group_definitions) == 0) {
    return(NULL)
  }
  
  group_indices <- list()
  group_weights <- sapply(group_definitions, function(g) g$weight)
  group_weights <- group_weights / sum(group_weights)  # Normalize group weights
  
  # Calculate index for each group
  for (i in seq_along(group_definitions)) {
    group <- group_definitions[[i]]
    group_name <- group$name
    group_indicators <- group$indicators
    
    # Filter indicators that exist in data
    available_indicators <- group_indicators[group_indicators %in% names(data)]
    
    if (length(available_indicators) == 0) {
      next
    }
    
    # Get within-group weights
    if (within_group_method == "equal") {
      indicator_weights <- rep(1/length(available_indicators), length(available_indicators))
    } else if (within_group_method == "custom") {
      # Read custom weights from UI inputs
      if (!is.null(input)) {
        # Get weights from UI inputs for this group
        custom_weights <- numeric(length(available_indicators))
        for (j in seq_along(available_indicators)) {
          indicator <- available_indicators[j]
          input_id <- paste0("weight_", group_name, "_", gsub("[^A-Za-z0-9]", "_", indicator))
          weight_value <- input[[input_id]]
          custom_weights[j] <- if(is.null(weight_value)) 1 else weight_value
        }
        indicator_weights <- custom_weights
      } else {
        # Fallback to stored custom weights or equal weights
        indicator_weights <- group$custom_weights
      }
      if (is.null(indicator_weights) || length(indicator_weights) != length(available_indicators)) {
        indicator_weights <- rep(1/length(available_indicators), length(available_indicators))
      }
      indicator_weights <- indicator_weights / sum(indicator_weights)
    } else if (within_group_method == "correlation") {
      group_data <- data[available_indicators]
      cor_matrix <- cor(group_data, use = "complete.obs")
      avg_correlations <- rowMeans(abs(cor_matrix), na.rm = TRUE)
      indicator_weights <- (1 - avg_correlations) / sum(1 - avg_correlations)
    } else if (within_group_method == "pca") {
      group_data <- data[available_indicators]
      pca_result <- prcomp(group_data, scale. = TRUE, center = TRUE)
      indicator_weights <- abs(pca_result$rotation[, 1])
      indicator_weights <- indicator_weights / sum(indicator_weights)
    }
    
    # Calculate group index
    group_index <- calculate_composite_index_universal(data, indicator_weights, available_indicators)
    group_indices[[group_name]] <- list(
      index = group_index,
      weight = group_weights[i],
      indicators = available_indicators,
      indicator_weights = indicator_weights
    )
  }
  
  # Calculate overall composite index
  if (length(group_indices) > 0) {
    overall_index <- rep(0, nrow(data))
    for (group_name in names(group_indices)) {
      group_info <- group_indices[[group_name]]
      overall_index <- overall_index + (group_info$index * group_info$weight)
    }
    
    return(list(
      overall_index = overall_index,
      group_indices = group_indices,
      group_weights = group_weights
    ))
  }
  
  return(NULL)
}
# Function to create group statistics
create_group_statistics <- function(data, group_result) {
  if (is.null(group_result)) return(NULL)
  
  stats_list <- list()
  
  for (group_name in names(group_result$group_indices)) {
    group_info <- group_result$group_indices[[group_name]]
    group_data <- data[group_info$indicators]
    
    stats_list[[group_name]] <- data.frame(
      Group = group_name,
      Indicators_Count = length(group_info$indicators),
      Group_Weight = round(group_info$weight, 3),
      Mean_Index = round(mean(group_info$index, na.rm = TRUE), 3),
      SD_Index = round(sd(group_info$index, na.rm = TRUE), 3),
      Min_Index = round(min(group_info$index, na.rm = TRUE), 3),
      Max_Index = round(max(group_info$index, na.rm = TRUE), 3),
      Indicators = paste(group_info$indicators, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, stats_list)
}
# Universal Core Functions
calculate_composite_index_universal <- function(data, weights, selected_indicators) {
  if (length(weights) != length(selected_indicators)) {
    stop("Number of weights must match number of selected indicators")
  }
  
  # Normalize weights
  normalized_weights <- weights / sum(weights)
  
  # Calculate weighted sum
  composite_scores <- rep(0, nrow(data))
  
  for (i in seq_along(selected_indicators)) {
    indicator <- selected_indicators[i]
    weight <- normalized_weights[i]
    
    if (indicator %in% names(data)) {
      # Handle missing values
      indicator_values <- data[[indicator]]
      indicator_values[is.na(indicator_values)] <- 0
      
      composite_scores <- composite_scores + (indicator_values * weight)
    }
  }
  
  return(composite_scores)
}
read_data_file_universal <- function(file_path, file_extension, sheet_name = NULL, has_header = TRUE, first_col_names = FALSE) {
  if (file_extension %in% c(".xlsx", ".xls")) {
    if (is.null(sheet_name)) {
      data <- read_excel(file_path, col_names = has_header)
    } else {
      data <- read_excel(file_path, sheet = sheet_name, col_names = has_header)
    }
    
    # Convert tibble to data.frame to avoid tibble warnings
    data <- as.data.frame(data)
    
    # If no headers, create generic column names
    if (!has_header) {
      colnames(data) <- paste0("Column_", 1:ncol(data))
    }
    
    # Handle first column as row names if specified
    if (first_col_names && ncol(data) > 1) {
      # Check if first column has unique values and no NAs
      first_col <- as.character(data[,1])
      if (length(unique(first_col)) == nrow(data) && !any(is.na(first_col))) {
        rownames(data) <- first_col
        data <- data[,-1]  # Remove first column
      } else {
        warning("First column contains duplicate or missing values, keeping as regular column")
      }
    }
    
    return(data)
  } else if (file_extension == ".csv") {
    # Always read as data.frame first
    data <- read.csv(file_path, stringsAsFactors = FALSE, header = has_header)
    
    # Handle first column as row names if specified
    if (first_col_names && ncol(data) > 1) {
      first_col <- as.character(data[,1])
      # Check if first column has unique values and no NAs
      if (length(unique(first_col)) == nrow(data) && !any(is.na(first_col))) {
        rownames(data) <- first_col
        data <- data[,-1]  # Remove first column
      } else {
        warning("First column contains duplicate or missing values, keeping as regular column")
      }
    }
    
    # If no headers, create generic column names
    if (!has_header) {
      colnames(data) <- paste0("Column_", 1:ncol(data))
    }
    
    return(data)
  } else {
    stop("Unsupported file format")
  }
}
detect_column_roles <- function(data) {
  column_info <- list()
  
  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    
    # Remove NA values for analysis
    clean_data <- col_data[!is.na(col_data)]
    
    # Skip if all values are NA
    if (length(clean_data) == 0) {
      column_info[[col_name]] <- list(
        type = "empty",
        unique_count = 0,
        unique_ratio = 0,
        suggested_role = "skip",
        sample_values = "All NA"
      )
      next
    }
    
    # Basic statistics
    unique_count <- length(unique(clean_data))
    total_count <- length(clean_data)
    unique_ratio <- if (total_count > 0) (unique_count / total_count) * 100 else 0
    
    # Determine type and role
    if (is.numeric(col_data)) {
      # Check for year column
      if (all(clean_data >= 1900 & clean_data <= 2100, na.rm = TRUE) && 
          length(unique(clean_data)) <= 50) {
        suggested_role <- "time"
      } else if (unique_ratio > 80) {
        suggested_role <- "identifier"
      } else {
        suggested_role <- "indicator"
      }
      col_type <- "numeric"
    } else if (is.character(col_data) || is.factor(col_data)) {
      if (unique_ratio > 50) {
        suggested_role <- "entity"
      } else {
        suggested_role <- "category"
      }
      col_type <- "categorical"
    } else {
      col_type <- "other"
      suggested_role <- "unknown"
    }
    
    # Sample values (safely)
    sample_vals <- head(unique(clean_data), 3)
    sample_text <- if (length(sample_vals) > 0) {
      paste(sample_vals, collapse = ", ")
    } else {
      "No valid values"
    }
    
    column_info[[col_name]] <- list(
      type = col_type,
      unique_count = unique_count,
      unique_ratio = round(unique_ratio, 1),
      suggested_role = suggested_role,
      sample_values = sample_text
    )
  }
  
  return(column_info)
}
forecast_time_series_universal <- function(data, entity_col, time_col, metric_col, periods) {
  # Prepare time series data
  ts_data <- data %>%
    arrange(!!sym(time_col)) %>%
    pull(!!sym(metric_col))
  
  # Remove NA values
  ts_data <- ts_data[!is.na(ts_data)]
  
  if (length(ts_data) < 3) {
    return(list(
      forecast = rep(mean(ts_data, na.rm = TRUE), periods),
      lower = rep(mean(ts_data, na.rm = TRUE) * 0.9, periods),
      upper = rep(mean(ts_data, na.rm = TRUE) * 1.1, periods),
      method = "Simple Mean"
    ))
  }
  
  # Try different forecasting methods
  tryCatch({
    # Try ARIMA
    fit <- auto.arima(ts_data, seasonal = FALSE)
    forecast_result <- forecast(fit, h = periods)
    
    return(list(
      forecast = as.numeric(forecast_result$mean),
      lower = as.numeric(forecast_result$lower[,2]),
      upper = as.numeric(forecast_result$upper[,2]),
      method = "ARIMA"
    ))
  }, error = function(e) {
    # Fallback to linear trend
    time_index <- seq_along(ts_data)
    lm_fit <- lm(ts_data ~ time_index)
    
    future_times <- seq(length(ts_data) + 1, length(ts_data) + periods)
    predictions <- predict(lm_fit, newdata = data.frame(time_index = future_times))
    
    return(list(
      forecast = as.numeric(predictions),
      lower = as.numeric(predictions * 0.9),
      upper = as.numeric(predictions * 1.1),
      method = "Linear Trend"
    ))
  })
}
# Universal UI
ui <- dashboardPage(
  dashboardHeader(title = "Composite Index Builder & Analytics Platform"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Column Mapping", tabName = "mapping", icon = icon("table")),
      menuItem("Data Processing", tabName = "processing", icon = icon("cogs")),
      menuItem("Custom Weighting", tabName = "weighting", icon = icon("balance-scale")),
      menuItem("Analysis Results", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("line-chart")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-line")),
      menuItem("Comparison", tabName = "comparison", icon = icon("exchange-alt")),
      menuItem("Sun Index / Pillar", tabName = "groupindex", icon = icon("layer-group"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .weight-box {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }
        .weight-input {
          margin-bottom: 10px;
          padding: 5px;
          background: rgba(255,255,255,0.1);
          border-radius: 5px;
        }
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "upload",
        fluidRow(
          box(
            title = "Upload Your Data", status = "primary", solidHeader = TRUE, width = 8,
            fileInput("data_file", "Choose CSV or Excel File",
                     accept = c(".csv", ".xlsx", ".xls")),
            
            # Header option
            checkboxInput("has_header", "First row contains column names (headers)", value = TRUE),
            # First column option
            checkboxInput("first_col_names", "First column contains row names/identifiers", value = FALSE),
            
            
            
            conditionalPanel(
              condition = "output.is_excel_file",
              selectInput("excel_sheet", "Select Excel Sheet:", choices = NULL),
              actionButton("load_excel_sheet", "Load Selected Sheet", class = "btn-info")
            ),
            
            conditionalPanel(
              condition = "output.data_uploaded",
              h4("Data Successfully Uploaded!"),
              verbatimTextOutput("data_summary")
            )
          ),
          
          box(
            title = "Instructions", status = "info", solidHeader = TRUE, width = 4,
            h4("How to Use:"),
            tags$ol(
              tags$li("Upload your CSV or Excel file"),
              tags$li("Map your columns (entity, time, indicators)"),
              tags$li("Process data and select indicators"),
              tags$li("Customize weights if needed"),
              tags$li("Analyze results and generate insights")
            ),
            br(),
            h4("Supported Data:"),
            tags$ul(
              tags$li("Any tabular data with numeric indicators"),
              tags$li("Entity column (countries, companies, etc.)"),
              tags$li("Optional time column for time series"),
              tags$li("Multiple numeric indicators for analysis")
            )
          )
        ),
        # Data Summary Section
        fluidRow(
          conditionalPanel(
            condition = "output.data_uploaded",
            box(
              title = "Data Overview", status = "success", solidHeader = TRUE, width = 12,
              h4("Column Summary:"),
              dataTableOutput("upload_data_summary")
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.data_uploaded",
            box(
              title = "Data Preview", status = "success", solidHeader = TRUE, width = 12,
              dataTableOutput("uploaded_data_preview")
            )
          )
        )
      ),
      
      # Column Mapping Tab
      tabItem(tabName = "mapping",
        fluidRow(
          box(
            title = "Column Analysis", status = "info", solidHeader = TRUE, width = 12,
            conditionalPanel(
              condition = "output.data_uploaded",
              p("Our system has analyzed your data and suggests column roles. Review and adjust as needed:"),
              dataTableOutput("column_analysis_table")
            ),
            conditionalPanel(
              condition = "!output.data_uploaded",
              h4("Please upload data first.")
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.data_uploaded",
            box(
              title = "Column Mapping Configuration", status = "warning", solidHeader = TRUE, width = 12,
              fluidRow(
                column(4,
                  h4("Required Mapping:"),
                  selectInput("entity_column", "Entity Column (Required):", choices = NULL),
                  p("Select the column that identifies your entities (e.g., countries, companies, products)")
                ),
                column(4,
                  h4("Optional Mapping:"),
                  selectInput("time_column", "Time Column (Optional):", choices = NULL),
                  p("Select if your data has time dimension (years, periods, etc.)")
                ),
                column(4,
                  h4("Additional Mapping:"),
                  selectInput("identifier_column", "Additional Identifier (Optional):", choices = NULL),
                  p("Select any additional grouping column if needed")
                )
              ),
              br(),
              div(style = "text-align: center;",
                actionButton("confirm_mapping", "Confirm Column Mapping", 
                           class = "btn-success btn-lg", icon = icon("check"))
              )
            )
          )
        )
      ),
      # Data Processing Tab  
      tabItem(tabName = "processing",
        fluidRow(
          box(
            title = "Data Processing Configuration", status = "primary", solidHeader = TRUE, width = 6,
            conditionalPanel(
              condition = "output.mapping_confirmed",
              h4("Select Indicators for Analysis:"),
              checkboxGroupInput("indicators_select", "Available Indicators:", choices = NULL),
              br(),
              fluidRow(
                column(6, actionButton("select_all_indicators", "Select All", class = "btn-info btn-sm")),
                column(6, actionButton("clear_all_indicators", "Clear All", class = "btn-warning btn-sm"))
              )
            ),
            conditionalPanel(
              condition = "!output.mapping_confirmed",
              h4("Please complete column mapping first.")
            )
          ),
          
          box(
            title = "Processing Options", status = "warning", solidHeader = TRUE, width = 6,
            conditionalPanel(
              condition = "output.mapping_confirmed",
              radioButtons("normalization_method", "Data Normalization:",
                          choices = list("None" = "none",
                                       "Min-Max Normalization" = "minmax"),
                          selected = "none"),
              br(),
              # helpText("Z-Score: Centers data around mean=0, sd=1. May fail if variance=0."),
              helpText("Min-Max: Scales data to 0-1 range. May fail if all values are identical."),
              checkboxInput("remove_missing", "Remove Missing Values", value = TRUE),
              selectInput("direction_option", "Direction:", 
                         choices = list("Higher is Better" = "higher", 
                                      "Lower is Better" = "lower", 
                                      "Mixed" = "mixed"), 
                         selected = "higher"),
              # Conditional panel for mixed direction
              conditionalPanel(
                condition = "input.direction_option == 'mixed'",
                h5("Specify Direction for Each Indicator:"),
                p("Select whether higher or lower values are better for each indicator:", style = "font-size: 12px; color: #666;"),
                uiOutput("indicator_directions_ui")
              ),
              numericInput("min_data_points", "Minimum Data Points per Entity:", 
                          value = 1, min = 1, max = 10),
              br(),
              div(style = "text-align: center;",
                actionButton("process_data", "Process Data", 
                           class = "btn-primary btn-lg", icon = icon("play"))
              )
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.data_processed",
            box(
              title = "Processed Data Preview", status = "success", solidHeader = TRUE, width = 12,
              dataTableOutput("processed_data_table")
            )
          ),
          # Data Summary Section
          conditionalPanel(
            condition = "output.data_processed",
            box(
              title = "Data Summary", status = "info", solidHeader = TRUE, width = 12,
              dataTableOutput("data_summary_table")
            )
          )
        )
      ),
      
      # Custom Weighting Tab
      tabItem(tabName = "weighting",
        fluidRow(
          box(
            title = "Custom Indicator Weights", status = "primary", solidHeader = TRUE, width = 6,
            class = "weight-box",
            conditionalPanel(
              condition = "output.indicators_available",
              h4("Adjust Indicator Importance:", style = "color: white;"),
              p("Higher weights = more influence on composite index", style = "color: lightgray;"),
              br(),
              uiOutput("weight_inputs"),
              br(),
              fluidRow(
                column(6, actionButton("set_equal_weights", "Equal Weights", class = "btn-info")),
                column(6, actionButton("apply_weights", "Apply Weights", class = "btn-success"))
              )
            ),
            conditionalPanel(
              condition = "!output.indicators_available",
              h4("Please process data and select indicators first.", style = "color: white;")
            )
          ),
          
          box(
            title = "Weights Summary", status = "info", solidHeader = TRUE, width = 6,
            conditionalPanel(
              condition = "output.indicators_available",
              dataTableOutput("weights_summary")
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.weights_applied",
            box(
              title = "Weight Impact Analysis", status = "success", solidHeader = TRUE, width = 6,
              plotlyOutput("weight_impact_plot")
            ),
            box(
              title = "Top Changes from Custom Weights", status = "warning", solidHeader = TRUE, width = 6,
              dataTableOutput("weight_comparison_table")
            )
          )
        )
      ),
      
      # Analysis Results Tab
      tabItem(tabName = "analysis",
        fluidRow(
          conditionalPanel(
            condition = "output.data_processed",
            box(
              title = "Composite Index Rankings", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("ranking_plot")
            ),
            box(
              title = "Indicators Correlation", status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("correlation_plot")
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.data_processed",
            box(
              title = "Detailed Results", status = "success", solidHeader = TRUE, width = 12,
              dataTableOutput("composite_results_table")
            )
          ),
          conditionalPanel(
            condition = "!output.data_processed",
            box(
              title = "No Data", status = "warning", solidHeader = TRUE, width = 12,
              h4("Please upload and process your data first.")
            )
          )
        )
      ),
      # Time Series Tab
      tabItem(tabName = "timeseries",
        fluidRow(
          box(
            title = "Single Entity Time Series", status = "primary", solidHeader = TRUE, width = 6,
            conditionalPanel(
              condition = "output.has_time_column && output.data_processed",
              selectInput("ts_entity", "Select Entity:", choices = NULL),
              selectInput("ts_metric", "Select Metric:", choices = NULL),
              plotlyOutput("timeseries_plot")
            ),
            conditionalPanel(
              condition = "!output.has_time_column || !output.data_processed",
              h4("Time series requires time column and processed data.")
            )
          ),
          
          box(
            title = "Multi-Entity Comparison", status = "info", solidHeader = TRUE, width = 6,
            conditionalPanel(
              condition = "output.has_time_column && output.data_processed",
              selectInput("multi_ts_entities", "Select Entities (max 10):", 
                         choices = NULL, multiple = TRUE),
              selectInput("multi_ts_metric", "Select Metric:", choices = NULL),
              plotlyOutput("multi_timeseries_plot")
            )
          )
        )
      ),
      
      # Forecasting Tab
      tabItem(tabName = "forecasting",
        fluidRow(
          box(
            title = "Generate Forecast", status = "primary", solidHeader = TRUE, width = 4,
            conditionalPanel(
              condition = "output.has_time_column && output.data_processed",
              selectInput("forecast_entity", "Select Entity:", choices = NULL),
              selectInput("forecast_metric", "Select Metric:", choices = NULL),
              numericInput("forecast_periods", "Forecast Periods:", value = 5, min = 1, max = 20),
              br(),
              actionButton("generate_forecast", "Generate Forecast", 
                          class = "btn-success", icon = icon("chart-line"))
            ),
            conditionalPanel(
              condition = "!output.has_time_column || !output.data_processed",
              h4("Forecasting requires time column and processed data.")
            )
          ),
          
          box(
            title = "Forecast Results", status = "success", solidHeader = TRUE, width = 8,
            conditionalPanel(
              condition = "output.forecast_generated",
              plotlyOutput("forecast_plot")
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.forecast_generated",
            box(
              title = "Forecast Data", status = "info", solidHeader = TRUE, width = 6,
              dataTableOutput("forecast_table")
            ),
            box(
              title = "Multi-Entity Forecast", status = "warning", solidHeader = TRUE, width = 6,
              selectInput("multi_forecast_entities", "Select Entities:", 
                         choices = NULL, multiple = TRUE),
              plotlyOutput("multi_forecast_plot")
            )
          )
        )
      ),
      
      # Comparison Tab
      tabItem(tabName = "comparison",
        fluidRow(
          box(
            title = "Entity Comparison Setup", status = "primary", solidHeader = TRUE, width = 4,
            conditionalPanel(
              condition = "output.data_processed",
              selectInput("compare_entities", "Select Entities to Compare:", 
                         choices = NULL, multiple = TRUE),
              selectInput("compare_metric", "Select Metric:", choices = NULL),
              conditionalPanel(
                condition = "output.has_time_column",
                selectInput("compare_time_period", "Select Time Period:", choices = NULL)
              ),
              br(),
              actionButton("generate_comparison", "Generate Comparison", 
                          class = "btn-info", icon = icon("exchange-alt"))
            ),
            conditionalPanel(
              condition = "!output.data_processed",
              h4("Please process data first.")
            )
          ),
          
          box(
            title = "Comparison Chart", status = "success", solidHeader = TRUE, width = 8,
            conditionalPanel(
              condition = "output.comparison_generated",
              plotlyOutput("comparison_plot")
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.comparison_generated",
            box(
              title = "Comparison Details", status = "info", solidHeader = TRUE, width = 12,
              dataTableOutput("comparison_table")
            )
          )
        )
      ),
      # Group-based Composite Index Tab
      tabItem(tabName = "groupindex",
        fluidRow(
          box(
            title = "Pillar-based Composite Index Calculator", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            fluidRow(
              column(6,
                h4("Step 1: Define Indicator Pillars"),
                helpText("Create pillars of related indicators and assign weights to each pillar."),
                
                # Pillar definition interface
                div(id = "pillar_definitions",
                  fluidRow(
                    column(8,
                      textInput("group_name", "Pillar Name:", placeholder = "e.g., Economic Indicators")
                    ),
                    column(4,
                      numericInput("group_weight", "Pillar Weight:", value = 1, min = 0, step = 0.1)
                    )
                  ),
                  fluidRow(
                    column(12,
                      selectizeInput("group_indicators", 
                                   "Select Indicators for this Pillar:",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = list(placeholder = "Choose indicators..."))
                    )
                  ),
                  fluidRow(
                    column(6,
                      actionButton("add_pillar", "Add Pillar", class = "btn-success", icon = icon("plus"))
                    ),
                    column(6,
                      actionButton("clear_pillars", "Clear All Pillars", class = "btn-warning", icon = icon("trash"))
                    )
                  )
                ),
                
                br(),
                h4("Step 2: Within-Pillar Weighting Method"),
                radioButtons("within_pillar_method", 
                           "How to weight indicators within each pillar:",
                           choices = list(
                             "Equal weights" = "equal",
                             "Custom weights" = "custom",
                             "Correlation-based" = "correlation",
                             "PCA-based" = "pca"
                           ),
                           selected = "equal"),
                
                # Custom weights table (shown only when custom method is selected)
                conditionalPanel(
                  condition = "input.within_pillar_method == 'custom'",
                  br(),
                  h5("Custom Weights for Each Pillar:"),
                  helpText("Set custom weights for indicators within each pillar. Weights will be normalized to sum to 1."),
                  uiOutput("custom_weights_tables")
                ),
                
                br(),
                h4("Step 3: Calculate Pillar-based Index"),
                actionButton("calculate_pillar_index", "Calculate Pillar-based Composite Index", 
                           class = "btn-primary btn-lg", icon = icon("calculator"))
              ),
              
              column(6,
                h4("Current Pillars"),
                div(id = "pillars_display",
                  helpText("Add pillars using the form on the left.")
                ),
                
                br(),
                h4("Pillar Weights Visualization"),
                plotlyOutput("pillar_weights_plot", height = "300px")
              )
            )
          )
        ),
        
        # Results section
        fluidRow(
          box(
            title = "Pillar-based Index Results", 
            status = "success", 
            solidHeader = TRUE, 
            width = 12,
            
            tabsetPanel(
              tabPanel("Index Values",
                br(),
                dataTableOutput("pillar_index_table")
              ),
              
              tabPanel("Pillar Contributions",
                br(),
                fluidRow(
                  column(6,
                    h4("Pillar Contribution to Overall Index"),
                    plotlyOutput("pillar_contribution_plot")
                  ),
                  column(6,
                    h4("Pillar Performance Comparison"),
                    plotlyOutput("pillar_performance_plot")
                  )
                )
              ),
              
              tabPanel("Detailed Analysis",
                br(),
                fluidRow(
                  #column(6,
                    #h4("Correlation Matrix by Pillars"),
                   # plotlyOutput("pillar_correlation_plot")
                 # ),
                  column(6,
                    h4("Pillar Statistics"),
                    dataTableOutput("pillar_stats_table")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
# Server Logic
server <- function(input, output, session) {
  # Reactive values for universal data storage
  values <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    processed_data_equal_weights = NULL,
    selected_indicators = NULL,
    entity_column = NULL,
    time_column = NULL,
    identifier_column = NULL,
    column_info = NULL,
    custom_weights = NULL,
    weights_applied = FALSE
  )
  # Group-based index reactive values
  group_definitions <- reactiveVal(list())
  group_index_result <- reactiveVal(NULL)
  
  
  # Generate custom weights tables for each group
  output$custom_weights_tables <- renderUI({
    groups <- group_definitions()
    if (length(groups) == 0) {
      return(helpText("Please define groups first before setting custom weights."))
    }
    
    # Create a table for each group
    group_tables <- lapply(names(groups), function(group_name) {
      group <- groups[[group_name]]
      indicators <- group$indicators
      
      if (length(indicators) == 0) return(NULL)
      
      # Create weight inputs for each indicator in this group
      weight_inputs <- lapply(seq_along(indicators), function(i) {
        indicator <- indicators[i]
        input_id <- paste0("weight_", group_name, "_", gsub("[^A-Za-z0-9]", "_", indicator))
        
        fluidRow(
          column(8, 
            p(indicator, style = "margin-top: 7px;")
          ),
          column(4,
            numericInput(input_id, NULL, value = 1, min = 0, step = 0.1, width = "100%")
          )
        )
      })
      
      # Return the group table
      div(
        style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
        h6(paste("Group:", group_name), style = "color: #337ab7; font-weight: bold;"),
        do.call(tagList, weight_inputs),
        actionButton(paste0("normalize_", gsub("[^A-Za-z0-9]", "_", group_name)), 
                    "Normalize Weights", class = "btn-info btn-sm", style = "margin-top: 5px;")
      )
    })
    
    # Remove NULL elements and return
    group_tables <- group_tables[!sapply(group_tables, is.null)]
    if (length(group_tables) == 0) {
      return(helpText("No indicators found in defined groups."))
    }
    
    do.call(tagList, group_tables)
  })
  
  # Add observers for normalize buttons
  observe({
    groups <- group_definitions()
    if (length(groups) == 0) return()
    
    # Create observers for each group's normalize button
    lapply(names(groups), function(group_name) {
      button_id <- paste0("normalize_", gsub("[^A-Za-z0-9]", "_", group_name))
      
      observeEvent(input[[button_id]], {
        group <- groups[[group_name]]
        indicators <- group$indicators
        
        # Get current weights
        current_weights <- numeric(length(indicators))
        for (i in seq_along(indicators)) {
          indicator <- indicators[i]
          input_id <- paste0("weight_", group_name, "_", gsub("[^A-Za-z0-9]", "_", indicator))
          current_weights[i] <- input[[input_id]] %||% 1
        }
        
        # Normalize weights
        if (sum(current_weights) > 0) {
          normalized_weights <- current_weights / sum(current_weights)
          
          # Update the inputs with normalized values
          for (i in seq_along(indicators)) {
            indicator <- indicators[i]
            input_id <- paste0("weight_", group_name, "_", gsub("[^A-Za-z0-9]", "_", indicator))
            updateNumericInput(session, input_id, value = round(normalized_weights[i], 3))
          }
          
          showNotification(paste("Weights normalized for group:", group_name), type = "message")
        }
      }, ignoreInit = TRUE)
    })
  })
  
  # Update indicator choices when data is loaded
  observe({
    if (!is.null(values$processed_data)) {
      numeric_cols <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      updateSelectizeInput(session, "group_indicators", choices = numeric_cols)
    }
  })
  
  # Add group functionality
  observeEvent(input$add_pillar, {
    req(input$group_name, input$group_indicators, input$group_weight)
    
    current_groups <- group_definitions()
    new_group <- list(
      name = input$group_name,
      indicators = input$group_indicators,
      weight = input$group_weight,
      custom_weights = NULL
    )
    
    current_groups[[input$group_name]] <- new_group
    group_definitions(current_groups)
    
    group_name_to_show <- input$group_name  # Store before clearing
    # Clear inputs
    updateTextInput(session, "group_name", value = "")
    updateSelectizeInput(session, "group_indicators", selected = character(0))
    updateNumericInput(session, "group_weight", value = 1)
    
    showNotification(paste("Group", group_name_to_show, "added successfully!"), type = "message")
  })
  
  # Clear all groups
  observeEvent(input$clear_pillars, {
    group_definitions(list())
    group_index_result(NULL)
    showNotification("All groups cleared!", type = "warning")
  })
  
  # Display current groups
  output$pillars_display <- renderUI({
    groups <- group_definitions()
    if (length(groups) == 0) {
      return(helpText("No groups defined yet. Add groups using the form on the left."))
    }
    
    group_cards <- lapply(names(groups), function(group_name) {
      group <- groups[[group_name]]
      div(
        class = "panel panel-default",
        div(
          class = "panel-heading",
          h5(group_name, style = "margin: 0;")
        ),
        div(
          class = "panel-body",
          p(strong("Weight: "), group$weight),
          p(strong("Indicators: "), paste(group$indicators, collapse = ", ")),
          actionButton(paste0("remove_", group_name), "Remove", 
                      class = "btn-danger btn-xs", 
                      onclick = paste0("Shiny.setInputValue('remove_group', '", group_name, "', {priority: 'event'});"))
        )
      )
    })
    
    do.call(tagList, group_cards)
  })
  
  # Remove individual group
  observeEvent(input$remove_group, {
    current_groups <- group_definitions()
    current_groups[[input$remove_group]] <- NULL
    group_definitions(current_groups)
    showNotification(paste("Group", input$remove_group, "removed!"), type = "warning")
  })
  
  # Group weights visualization
  output$pillar_weights_plot <- renderPlotly({
    groups <- group_definitions()
    if (length(groups) == 0) {
      return(plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = 'No data available'))
    }
    
    weights_df <- data.frame(
      Group = names(groups),
      Weight = sapply(groups, function(g) g$weight),
      stringsAsFactors = FALSE
    )
    
    p <- ggplot(weights_df, aes(x = Group, y = Weight, fill = Group)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Group Weights", x = "Groups", y = "Weight")
    
    ggplotly(p, tooltip = c('x', 'y'))
  })
  
  # Calculate group-based index
  # Create reactive trigger for both button click and method change
  calc_trigger <- reactive({
    list(input$calculate_pillar_index, input$within_pillar_method)
  })
  
  # Calculate group-based index when trigger changes
  observeEvent(calc_trigger(), {
    req(values$processed_data, group_definitions())
    
    groups <- group_definitions()
    if (length(groups) == 0) {
      showNotification("Please define at least one group first!", type = "error")
      return()
    }
    
    withProgress(message = "Calculating group-based composite index...", {
      result <- calculate_group_based_index(
        values$processed_data, 
        groups, 
        input$within_pillar_method,
        input
      )
      
      if (!is.null(result)) {
        group_index_result(result)
        showNotification("Group-based composite index calculated successfully!", type = "message")
      } else {
        showNotification("Error calculating group-based index. Please check your data and groups.", type = "error")
      }
    })
  })
  
  # Group index results table
  output$pillar_index_table <- renderDataTable({
    result <- group_index_result()
    if (is.null(result)) return(NULL)
    
    # Create results dataframe
    results_df <- values$processed_data
    results_df$Group_Based_Index <- round(result$overall_index, 4)
    
    # Add individual group indices
    for (group_name in names(result$group_indices)) {
      col_name <- paste0("Group_", gsub("[^A-Za-z0-9]", "_", group_name))
      results_df[[col_name]] <- round(result$group_indices[[group_name]]$index, 4)
    }
    
    DT::datatable(results_df, options = list(scrollX = TRUE, pageLength = 15))
  })
  
  # Group contribution plot
  output$pillar_contribution_plot <- renderPlotly({
    result <- group_index_result()
    if (is.null(result)) return(plot_ly() %>% layout(title = 'No data available'))
    
    contrib_df <- data.frame(
      Group = names(result$group_indices),
      Contribution = sapply(result$group_indices, function(g) g$weight * 100),
      stringsAsFactors = FALSE
    )
    
    # Use native plotly pie chart instead of ggplot + coord_polar
    plot_ly(contrib_df, 
            labels = ~Group, 
            values = ~Contribution, 
            type = 'pie',
            textinfo = 'label+percent',
            hovertemplate = '%{label}: %{value:.1f}%<extra></extra>') %>%
      layout(title = 'Group Contribution to Overall Index (%)',
             showlegend = TRUE)
  })
  
  # Group performance comparison
  output$pillar_performance_plot <- renderPlotly({
    result <- group_index_result()
    if (is.null(result)) return(plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = 'No data available'))
    
    perf_data <- lapply(names(result$group_indices), function(group_name) {
      group_index <- result$group_indices[[group_name]]$index
      data.frame(
        Group = group_name,
        Mean = mean(group_index, na.rm = TRUE),
        SD = sd(group_index, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
    
    perf_df <- do.call(rbind, perf_data)
    
    p <- ggplot(perf_df, aes(x = Group, y = Mean, fill = Group)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Group Performance Comparison", x = "Groups", y = "Mean Index Value")
    
    ggplotly(p, tooltip = c('x', 'y'))
  })
  
  # Group statistics table
  output$pillar_stats_table <- renderDataTable({
    result <- group_index_result()
    if (is.null(result)) return(NULL)
    
    stats_df <- create_group_statistics(values$processed_data, result)
    DT::datatable(stats_df, options = list(scrollX = TRUE))
  })
  
  # Group correlation matrix plot
  # Group correlation matrix plot
  output$pillar_correlation_plot <- renderPlotly({
    req(group_index_result())
    
    result <- group_index_result()
    groups <- group_definitions()
    
    # Comprehensive validation
    if (is.null(result) || length(groups) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    if (is.null(result$group_indices) || length(result$group_indices) == 0) {
      return(plotly_empty() %>% layout(title = "No group indices calculated"))
    }
    
    if (is.null(result$composite_index) || length(result$composite_index) == 0) {
      return(plotly_empty() %>% layout(title = "No composite index calculated"))
    }
    
    # Check if we have enough groups for correlation
    if (length(result$group_indices) < 2) {
      return(plotly_empty() %>% layout(title = "Need at least 2 groups for correlation analysis"))
    }
    
    # Create data frame with proper validation
    tryCatch({
      # Start with just the group indices
      group_data <- data.frame(row.names = seq_along(result$composite_index))
      
      # Add each group's index
      for (group_name in names(result$group_indices)) {
        group_index <- result$group_indices[[group_name]]
        if (!is.null(group_index) && length(group_index) > 0) {
          # Clean group name for column
          clean_name <- gsub("[^A-Za-z0-9_]", "_", group_name)
          group_data[[paste0("Group_", clean_name)]] <- as.numeric(group_index)
        }
      }
      
      # Add overall composite index
      group_data$Overall_Index <- as.numeric(result$composite_index)
      
      # Validate we have data
      if (ncol(group_data) < 2) {
        return(plotly_empty() %>% layout(title = "Insufficient data for correlation"))
      }
      
      if (nrow(group_data) == 0) {
        return(plotly_empty() %>% layout(title = "No observations available"))
      }
      
      # Calculate correlation matrix
      cor_matrix <- cor(group_data, use = "complete.obs")
      
      # Check for valid correlations
      if (any(is.na(cor_matrix)) || nrow(cor_matrix) < 2) {
        return(plotly_empty() %>% layout(title = "Unable to calculate correlations"))
      }
      
      # Convert to long format for plotting
      cor_long <- expand.grid(
        Var1 = factor(rownames(cor_matrix), levels = rownames(cor_matrix)),
        Var2 = factor(colnames(cor_matrix), levels = rev(colnames(cor_matrix)))
      )
      cor_long$value <- as.vector(cor_matrix)
      
      # Create the correlation heatmap
      p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = value, text = paste(
        "Row:", Var1, "<br>",
        "Column:", Var2, "<br>",
        "Correlation:", round(value, 3)
      ))) +
        geom_tile(color = "white", size = 0.5) +
        scale_fill_gradient2(
          low = "#d73027", 
          mid = "white", 
          high = "#1a9850", 
          midpoint = 0,
          limits = c(-1, 1),
          name = "Correlation"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(hjust = 1),
          panel.grid = element_blank(),
          axis.title = element_blank()
        ) +
        labs(title = "Correlation Matrix: Groups and Overall Index") +
        coord_fixed()
      
      # Convert to plotly
      ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(text = "Correlation Matrix: Groups and Overall Index", x = 0.5),
          margin = list(l = 100, r = 50, t = 80, b = 100)
        )
      
    }, error = function(e) {
      # Return error message if something goes wrong
      plotly_empty() %>% layout(title = paste("Error creating correlation plot:", e$message))
    })
  })
  
  
  # File upload handling
  observeEvent(input$data_file, {
    req(input$data_file)
    
    file_ext <- tools::file_ext(input$data_file$datapath)
    
    if (file_ext %in% c("xlsx", "xls")) {
      # Handle Excel files
      sheet_names <- excel_sheets(input$data_file$datapath)
      updateSelectInput(session, "excel_sheet", choices = sheet_names, selected = sheet_names[1])
      values$is_excel <- TRUE
    } else {
      # Handle CSV files
      values$raw_data <- read_data_file_universal(input$data_file$datapath, paste0(".", file_ext), has_header = input$has_header, first_col_names = input$first_col_names)
      values$is_excel <- FALSE
      
      # Analyze columns automatically
      values$column_info <- detect_column_roles(values$raw_data)
      
      # Update column choices
      update_column_choices()
    }
  })
  
  # Excel sheet loading
  observeEvent(input$load_excel_sheet, {
    req(input$data_file, input$excel_sheet)
    
    file_ext <- tools::file_ext(input$data_file$datapath)
    values$raw_data <- read_data_file_universal(input$data_file$datapath, paste0(".", file_ext), input$excel_sheet, has_header = input$has_header, first_col_names = input$first_col_names)
    
    # Analyze columns automatically
    values$column_info <- detect_column_roles(values$raw_data)
    
    # Update column choices
    update_column_choices()
  })
  
  # Function to update column choices based on detected roles
  update_column_choices <- function() {
    if (!is.null(values$raw_data) && !is.null(values$column_info)) {
      all_cols <- names(values$raw_data)
      
      # Suggest entity column (categorical with reasonable unique ratio)
      entity_suggestions <- names(values$column_info)[sapply(values$column_info, function(x) x$suggested_role == "entity")]
      
      # Suggest time column (numeric, could be year/period)
      time_suggestions <- names(values$column_info)[sapply(values$column_info, function(x) x$suggested_role %in% c("time", "time_or_indicator"))]
      
      # Update selectInputs
      updateSelectInput(session, "entity_column", choices = as.list(c("None" = "", all_cols)), 
                       selected = if(length(entity_suggestions) > 0) entity_suggestions[1] else "")
      
      updateSelectInput(session, "time_column", choices = as.list(c("None" = "", all_cols)), 
                       selected = if(length(time_suggestions) > 0) time_suggestions[1] else "")
      
      updateSelectInput(session, "identifier_column", choices = as.list(c("None" = "", all_cols)), selected = "")
    }
  }
  
  # Column mapping confirmation - FIXED showNotification
  observeEvent(input$confirm_mapping, {
    req(values$raw_data, input$entity_column)
    
    values$entity_column <- if(input$entity_column == "") NULL else input$entity_column
    values$time_column <- if(input$time_column == "") NULL else input$time_column
    values$identifier_column <- if(input$identifier_column == "") NULL else input$identifier_column
    values$mapping_confirmed <- TRUE
    
    # Update indicator choices (numeric columns excluding mapped columns)
    mapped_cols <- c(values$entity_column, values$time_column, values$identifier_column)
    mapped_cols <- mapped_cols[!is.null(mapped_cols)]
    
    numeric_cols <- names(values$raw_data)[sapply(values$raw_data, is.numeric)]
    available_indicators <- setdiff(numeric_cols, mapped_cols)
    
    updateCheckboxGroupInput(session, "indicators_select", choices = available_indicators)
    
    showNotification("Column mapping confirmed successfully!", type = "message")
  })
  # Generate UI for individual indicator directions when mixed is selected
  output$indicator_directions_ui <- renderUI({
    req(input$direction_option == "mixed")
    
    # Check if we have indicators available
    if(is.null(values$raw_data) || !values$mapping_confirmed) {
      return(p("Please complete column mapping first.", style = "color: #999;"))
    }
    
    # Get available indicators
    if(!is.null(input$indicators_select) && length(input$indicators_select) > 0) {
      available_indicators <- input$indicators_select
    } else if(!is.null(values$column_mapping)) {
      available_indicators <- values$column_mapping
    } else {
      return(p("No indicators available.", style = "color: #999;"))
    }
    
    if(length(available_indicators) == 0) {
      return(p("Please select indicators first.", style = "color: #999;"))
    }
    
    # Create direction inputs for each indicator
    direction_inputs <- lapply(available_indicators, function(indicator) {
      div(style = "margin-bottom: 10px; padding: 8px; border: 1px solid #ddd; border-radius: 4px;",
        fluidRow(
          column(6, p(strong(indicator), style = "margin-top: 8px; margin-bottom: 0;")),
          column(6, selectInput(
            inputId = paste0("dir_", make.names(indicator)),
            label = NULL,
            choices = list("Higher is Better" = "higher", "Lower is Better" = "lower"),
            selected = "higher",
            width = "100%"
          ))
        )
      )
    })
    
    tagList(
      h6("Configure direction for each indicator:", style = "margin-bottom: 15px; color: #666;"),
      do.call(tagList, direction_inputs)
    )
  })
  
  # Reactive outputs for UI conditions
  output$data_uploaded <- reactive({
    !is.null(values$raw_data)
  })
  outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)
  
  output$is_excel_file <- reactive({
    !is.null(values$is_excel) && values$is_excel
  })
  outputOptions(output, "is_excel_file", suspendWhenHidden = FALSE)
  
  output$mapping_confirmed <- reactive({
    values$mapping_confirmed
  })
  outputOptions(output, "mapping_confirmed", suspendWhenHidden = FALSE)
  
  output$has_time_column <- reactive({
    !is.null(values$time_column)
  })
  outputOptions(output, "has_time_column", suspendWhenHidden = FALSE)
  
  output$data_processed <- reactive({
    !is.null(values$processed_data)
  })
  outputOptions(output, "data_processed", suspendWhenHidden = FALSE)
  
  output$indicators_available <- reactive({
    !is.null(values$selected_indicators) && length(values$selected_indicators) > 0
  })
  outputOptions(output, "indicators_available", suspendWhenHidden = FALSE)
  
  output$weights_applied <- reactive({
    values$weights_applied
  })
  outputOptions(output, "weights_applied", suspendWhenHidden = FALSE)
  
  output$forecast_generated <- reactive({
    !is.null(values$forecast_results)
  })
  outputOptions(output, "forecast_generated", suspendWhenHidden = FALSE)
  
  output$comparison_generated <- reactive({
    !is.null(input$generate_comparison) && input$generate_comparison > 0
  })
  outputOptions(output, "comparison_generated", suspendWhenHidden = FALSE)
  
  # Data processing
  observeEvent(input$process_data, {
    req(values$raw_data, values$mapping_confirmed, input$indicators_select)
    
    if (length(input$indicators_select) == 0) {
      showNotification("Please select at least one indicator.", type = "warning")
      return()
    }
    
    values$selected_indicators <- input$indicators_select
    
    # Prepare data
    processing_data <- values$raw_data
    
    # Filter minimum data points per entity if specified
    if (!is.null(values$entity_column) && input$min_data_points > 1) {
      entity_counts <- processing_data %>%
        group_by(!!sym(values$entity_column)) %>%
        summarise(count = n(), .groups = "drop") %>%
        filter(count >= input$min_data_points)
      
      processing_data <- processing_data %>%
        filter(!!sym(values$entity_column) %in% entity_counts[[values$entity_column]])
    }
    
    # Remove missing values if requested
    if (input$remove_missing) {
      processing_data <- processing_data[complete.cases(processing_data[, values$selected_indicators]), ]
    }
    # Apply direction logic - invert values for "lower is better" indicators
    if (input$direction_option == "lower") {
      # For "lower is better", invert all selected indicators
      for (indicator in values$selected_indicators) {
        if (indicator %in% names(processing_data) && is.numeric(processing_data[[indicator]])) {
          processing_data[[indicator]] <- -processing_data[[indicator]]
        }
      }
    } else if (input$direction_option == "mixed") {
      # For mixed direction, check individual indicator directions
      for (indicator in values$selected_indicators) {
        direction_input_id <- paste0("dir_", make.names(indicator))
        if (!is.null(input[[direction_input_id]]) && input[[direction_input_id]] == "lower") {
          if (indicator %in% names(processing_data) && is.numeric(processing_data[[indicator]])) {
            processing_data[[indicator]] <- -processing_data[[indicator]]
          }
        }
      }
    }
    # Note: For "higher is better", no transformation is needed
    
    
    # Apply normalization if requested
    if (input$normalization_method != "none") {
      for (indicator in values$selected_indicators) {
        if (input$normalization_method == "minmax") {
          # Min-Max normalization (0 to 1 range)
          col_data <- processing_data[[indicator]]
          col_min <- min(col_data, na.rm = TRUE)
          col_max <- max(col_data, na.rm = TRUE)
          
          # Check for zero range
          if (col_min == col_max || is.na(col_min) || is.na(col_max)) {
            showNotification(paste("Warning: Column", indicator, "has zero range. Skipping normalization."), type = "warning")
            next
          }
          
          processing_data[[indicator]] <- (col_data - col_min) / (col_max - col_min)
        }
      }
    }
    
    # Calculate composite index with equal weights
    equal_weights <- rep(1, length(values$selected_indicators))
    names(equal_weights) <- values$selected_indicators
    
    processing_data$composite_index <- calculate_composite_index_universal(
      processing_data, 
      weights = equal_weights, 
      selected_indicators = values$selected_indicators
    )
    
    values$processed_data <- processing_data
    values$processed_data_equal_weights <- processing_data
    values$custom_weights <- equal_weights
    
    # Update UI choices for analysis
    update_analysis_choices()
    
    showNotification("Data processed successfully!", type = "message")
  })
  
  # Function to update analysis UI choices
  update_analysis_choices <- function() {
    if (!is.null(values$processed_data) && !is.null(values$entity_column)) {
      entities <- unique(values$processed_data[[values$entity_column]])
      
      # Update entity choices for various analyses
      updateSelectInput(session, "ts_entity", choices = entities)
      updateSelectInput(session, "forecast_entity", choices = entities)
      updateSelectInput(session, "compare_entities", choices = entities)
      updateSelectInput(session, "multi_ts_entities", choices = entities)
      updateSelectInput(session, "multi_forecast_entities", choices = entities)
      
      # Update metric choices
      metric_choices <- c("composite_index", values$selected_indicators)
      updateSelectInput(session, "ts_metric", choices = metric_choices, selected = "composite_index")
      updateSelectInput(session, "multi_ts_metric", choices = metric_choices, selected = "composite_index")
      updateSelectInput(session, "forecast_metric", choices = metric_choices, selected = "composite_index")
      updateSelectInput(session, "compare_metric", choices = metric_choices, selected = "composite_index")
      
      # Update time period choices if time column exists
      if (!is.null(values$time_column)) {
        time_periods <- sort(unique(values$processed_data[[values$time_column]]))
        updateSelectInput(session, "compare_time_period", choices = time_periods, 
                         selected = max(time_periods, na.rm = TRUE))
      }
    }
  }
  
  # Weight management
  observeEvent(input$select_all_indicators, {
    updateCheckboxGroupInput(session, "indicators_select", 
                           selected = names(values$raw_data)[sapply(values$raw_data, is.numeric)])
  })
  
  observeEvent(input$clear_all_indicators, {
    updateCheckboxGroupInput(session, "indicators_select", selected = character(0))
  })
  
  # Generate weight inputs dynamically
  output$weight_inputs <- renderUI({
    req(values$selected_indicators)
    
    weight_inputs <- lapply(values$selected_indicators, function(indicator) {
      div(class = "weight-input",
        fluidRow(
          column(6, 
            tags$label(indicator, style = "color: white; font-weight: bold;")
          ),
          column(6,
            numericInput(paste0("weight_", indicator), NULL, 
                        value = if(!is.null(values$custom_weights)) values$custom_weights[[indicator]] else 1.0,
                        min = 0, max = 10, step = 0.1, width = "100%")
          )
        )
      )
    })
    
    do.call(tagList, weight_inputs)
  })
  
  # Set equal weights
  observeEvent(input$set_equal_weights, {
    req(values$selected_indicators)
    
    for (indicator in values$selected_indicators) {
      updateNumericInput(session, paste0("weight_", indicator), value = 1.0)
    }
    
    showNotification("All weights set to equal values.", type = "message")
  })
  
  # Apply custom weights
  observeEvent(input$apply_weights, {
    req(values$processed_data_equal_weights, values$selected_indicators)
    
    # Collect weights from inputs
    new_weights <- sapply(values$selected_indicators, function(indicator) {
      input[[paste0("weight_", indicator)]]
    })
    
    if (any(is.na(new_weights)) || all(new_weights == 0)) {
      showNotification("Please ensure all weights are valid numbers and at least one weight is greater than 0.", type = "error")
      return()
    }
    
    values$custom_weights <- new_weights
    
    # Recalculate composite index with custom weights
    values$processed_data <- values$processed_data_equal_weights
    values$processed_data$composite_index <- calculate_composite_index_universal(
      values$processed_data, 
      weights = new_weights, 
      selected_indicators = values$selected_indicators
    )
    
    values$weights_applied <- TRUE
    
    showNotification("Custom weights applied successfully!", type = "message")
  })
  # Data tables and outputs
  output$data_summary <- renderText({
    req(values$raw_data)
    paste("Rows:", nrow(values$raw_data), "| Columns:", ncol(values$raw_data))
  })
  # Upload Data Summary Table
  output$upload_data_summary <- renderDataTable({
    req(values$raw_data)
    
    # Create summary for uploaded data
    summary_data <- data.frame(
      Column = character(),
      Type = character(),
      Count = numeric(),
      Missing = numeric(),
      Missing_Percent = numeric(),
      Unique = numeric(),
      Sample_Values = character(),
      stringsAsFactors = FALSE
    )
    
    for (col_name in names(values$raw_data)) {
      col_data <- values$raw_data[[col_name]]
      missing_count <- sum(is.na(col_data))
      total_count <- length(col_data)
      missing_percent <- round((missing_count / total_count) * 100, 1)
      unique_count <- length(unique(col_data[!is.na(col_data)]))
      
      # Get sample values
      sample_vals <- head(unique(col_data[!is.na(col_data)]), 3)
      sample_text <- paste(sample_vals, collapse = ", ")
      if (nchar(sample_text) > 50) {
        sample_text <- paste0(substr(sample_text, 1, 47), "...")
      }
      
      # Determine column type
      col_type <- if(is.numeric(col_data)) "Numeric" else "Text"
      
      summary_data <- rbind(summary_data, data.frame(
        Column = col_name,
        Type = col_type,
        Count = total_count,
        Missing = missing_count,
        Missing_Percent = missing_percent,
        Unique = unique_count,
        Sample_Values = sample_text,
        stringsAsFactors = FALSE
      ))
    }
    
    DT::datatable(summary_data, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  caption = "Summary of Uploaded Data - All Columns") %>%
      DT::formatStyle("Missing_Percent", 
                      backgroundColor = DT::styleInterval(c(10, 30), 
                                                          c("lightgreen", "yellow", "lightcoral")))
  })
  
  output$uploaded_data_preview <- renderDataTable({
    req(values$raw_data)
    DT::datatable(head(values$raw_data, 100), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$column_analysis_table <- renderDataTable({
    req(values$column_info)
    
    analysis_df <- data.frame(
      Column = names(values$column_info),
      Type = sapply(values$column_info, function(x) x$type),
      Unique_Count = sapply(values$column_info, function(x) x$unique_count),
      Unique_Ratio_Percent = sapply(values$column_info, function(x) paste0(x$unique_ratio, "%")),
      Suggested_Role = sapply(values$column_info, function(x) x$suggested_role),
      Sample_Values = sapply(values$column_info, function(x) x$sample_values),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(analysis_df, options = list(scrollX = TRUE, pageLength = 15))
  })
  
  output$processed_data_table <- renderDataTable({
    req(values$processed_data)
    DT::datatable(head(values$processed_data, 100), options = list(scrollX = TRUE, pageLength = 10)) %>%
      DT::formatRound(c("composite_index", values$selected_indicators), digits = 3)
  })
  # Data Summary Table
  output$data_summary_table <- renderDataTable({
    req(values$processed_data)
    
    # Create summary statistics
    summary_data <- data.frame(
      Column = character(),
      Type = character(),
      Count = numeric(),
      Missing = numeric(),
      Unique = numeric(),
      Mean = character(),
      Median = character(),
      Min = character(),
      Max = character(),
      Std_Dev = character(),
      stringsAsFactors = FALSE
    )
    
    for (col_name in names(values$processed_data)) {
      col_data <- values$processed_data[[col_name]]
      
      if (is.numeric(col_data)) {
        summary_data <- rbind(summary_data, data.frame(
          Column = col_name,
          Type = "Numeric",
          Count = length(col_data),
          Missing = sum(is.na(col_data)),
          Unique = length(unique(col_data[!is.na(col_data)])),
          Mean = round(mean(col_data, na.rm = TRUE), 3),
          Median = round(median(col_data, na.rm = TRUE), 3),
          Min = round(min(col_data, na.rm = TRUE), 3),
          Max = round(max(col_data, na.rm = TRUE), 3),
          Std_Dev = round(sd(col_data, na.rm = TRUE), 3),
          stringsAsFactors = FALSE
        ))
      } else {
        summary_data <- rbind(summary_data, data.frame(
          Column = col_name,
          Type = "Text",
          Count = length(col_data),
          Missing = sum(is.na(col_data)),
          Unique = length(unique(col_data[!is.na(col_data)])),
          Mean = "-",
          Median = "-",
          Min = "-",
          Max = "-",
          Std_Dev = "-",
          stringsAsFactors = FALSE
        ))
      }
    }
    
    DT::datatable(summary_data, 
                  options = list(scrollX = TRUE, pageLength = 15),
                  caption = "Statistical Summary of Processed Data")
  })
  
  output$weights_summary <- renderDataTable({
    req(values$custom_weights)
    
    weights_df <- data.frame(
      Indicator = names(values$custom_weights),
      Weight = as.numeric(values$custom_weights),
      Normalized_Weight = as.numeric(values$custom_weights) / sum(values$custom_weights),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(weights_df, options = list(pageLength = 10)) %>%
      DT::formatRound(c("Weight", "Normalized_Weight"), digits = 3)
  })
  # Analysis plots
  output$ranking_plot <- renderPlotly({
    req(values$processed_data, values$entity_column)
    
    top_entities <- values$processed_data %>%
      arrange(desc(composite_index)) %>%
      head(20)
    
    p <- ggplot(top_entities, aes(x = reorder(!!sym(values$entity_column), composite_index), 
                                 y = composite_index)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      coord_flip() +
      labs(title = "Top 20 Entities by Composite Index",
           x = "Entity", y = "Composite Index") +
      theme_minimal()
    
    ggplotly(p, tooltip = c('x', 'y'))
  })
  
  output$correlation_plot <- renderPlotly({
    req(values$processed_data, values$selected_indicators)
    
    cor_data <- values$processed_data[, c("composite_index", values$selected_indicators)]
    cor_matrix <- cor(cor_data, use = "complete.obs")
    
    # Convert correlation matrix to long format for plotting
    cor_long <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
    cor_long$value <- as.vector(cor_matrix)
    
    p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Indicators Correlation Matrix", x = "", y = "", fill = "Correlation")
    
    ggplotly(p, tooltip = c('x', 'y'))
  })
  
  
  # Fixed composite results table with better error handling
  output$composite_results_table <- renderDataTable({
    req(values$processed_data, values$entity_column)
    
    tryCatch({
      # Create results data with proper column selection
      if (!is.null(values$time_column)) {
        results_data <- values$processed_data %>%
          select(
            Entity = !!sym(values$entity_column),
            Time_Period = !!sym(values$time_column),
            Composite_Index = composite_index,
            all_of(values$selected_indicators)
          ) %>%
          arrange(desc(Composite_Index))
      } else {
        results_data <- values$processed_data %>%
          select(
            Entity = !!sym(values$entity_column),
            Composite_Index = composite_index,
            all_of(values$selected_indicators)
          ) %>%
          arrange(desc(Composite_Index))
      }
      
      # Add ranking column
      results_data <- results_data %>%
        mutate(Rank = row_number()) %>%
        select(Rank, everything())
      
      # Create the datatable with proper formatting
      dt <- DT::datatable(
        results_data, 
        options = list(
          scrollX = TRUE, 
          pageLength = 25,
          order = list(list(0, "asc")),  # Order by rank
          columnDefs = list(
            list(className = "dt-center", targets = 0)  # Center rank column
          )
        ),
        rownames = FALSE
      )
      
      # Format numeric columns
      numeric_cols <- c("Composite_Index", values$selected_indicators)
      existing_numeric_cols <- intersect(numeric_cols, names(results_data))
      
      if (length(existing_numeric_cols) > 0) {
        dt <- dt %>% DT::formatRound(existing_numeric_cols, digits = 3)
      }
      
      return(dt)
      
    }, error = function(e) {
      # Fallback simple table if there are issues
      simple_results <- values$processed_data %>%
        select(!!sym(values$entity_column), composite_index) %>%
        arrange(desc(composite_index)) %>%
        mutate(Rank = row_number()) %>%
        select(Rank, Entity = !!sym(values$entity_column), Composite_Index = composite_index)
      
      DT::datatable(simple_results, options = list(pageLength = 25)) %>%
        DT::formatRound("Composite_Index", digits = 3)
    })
  })
  
  # Also fix the ranking plot to ensure it works
  output$ranking_plot <- renderPlotly({
    req(values$processed_data, values$entity_column)
    
    tryCatch({
      # Get top entities
      top_entities <- values$processed_data %>%
        arrange(desc(composite_index)) %>%
        head(20) %>%
        mutate(Rank = row_number())
      
      # Create the plot
      p <- ggplot(top_entities, aes(x = reorder(!!sym(values$entity_column), composite_index), 
                                   y = composite_index)) +
        geom_col(fill = "steelblue", alpha = 0.8) +
        coord_flip() +
        labs(title = "Top 20 Entities by Composite Index",
             x = "Entity", y = "Composite Index") +
        theme_minimal() +
        theme(
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold")
        )
      
      ggplotly(p, tooltip = c("x", "y"))
      
    }, error = function(e) {
      # Return empty plot with error message
      plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = 'No data available') %>% 
        layout(title = list(text = paste("Error creating plot:", e$message)))
    })
  })
  
  # Enhanced correlation plot with better error handling
  output$correlation_plot <- renderPlotly({
    req(values$processed_data, values$selected_indicators)
    
    tryCatch({
      # Select only numeric columns that exist
      available_indicators <- intersect(values$selected_indicators, names(values$processed_data))
      
      if (length(available_indicators) < 2) {
         return(plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = "Need at least 2 indicators for correlation"))
      }
      
      cor_data <- values$processed_data[, c("composite_index", available_indicators)]
      cor_data <- cor_data[complete.cases(cor_data), ]  # Remove NA rows
      
      if (nrow(cor_data) < 3) {
         return(plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = "Insufficient data for correlation"))
      }
      
      cor_matrix <- cor(cor_data, use = "complete.obs")
      
      # Convert to long format
      cor_long <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
      cor_long$value <- as.vector(cor_matrix)
      
      # Create heatmap
      p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
              axis.text.y = element_text(size = 10)) +
        labs(title = "Indicators Correlation Matrix", x = "", y = "") +
        coord_fixed()
      
      ggplotly(p, tooltip = c("x", "y", "fill"))
      
    }, error = function(e) {
      plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = 'No data available') %>% 
        layout(title = list(text = paste("Error creating correlation plot:", e$message)))
    })
  })
  # Time series functionality
  output$timeseries_plot <- renderPlotly({
    req(values$processed_data, values$time_column, input$ts_entity, input$ts_metric)
    
    ts_data <- values$processed_data %>%
      filter(!!sym(values$entity_column) == input$ts_entity) %>%
      arrange(!!sym(values$time_column))
    
    p <- ggplot(ts_data, aes(x = !!sym(values$time_column), y = !!sym(input$ts_metric))) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "darkblue", size = 2) +
      labs(title = paste("Time Series for", input$ts_entity),
           x = values$time_column, y = input$ts_metric) +
      theme_minimal()
    
    ggplotly(p, tooltip = c('x', 'y'))
  })
  
  output$multi_timeseries_plot <- renderPlotly({
    req(values$processed_data, values$time_column, input$multi_ts_entities, input$multi_ts_metric)
    
    if (length(input$multi_ts_entities) > 10) {
      showNotification("Please select maximum 10 entities for better visualization.", type = "warning")
      return()
    }
    
    multi_ts_data <- values$processed_data %>%
      filter(!!sym(values$entity_column) %in% input$multi_ts_entities) %>%
      arrange(!!sym(values$time_column))
    
    p <- ggplot(multi_ts_data, aes(x = !!sym(values$time_column), y = !!sym(input$multi_ts_metric), 
                                  color = !!sym(values$entity_column))) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.5) +
      labs(title = paste("Multi-Entity Time Series:", input$multi_ts_metric),
           x = values$time_column, y = input$multi_ts_metric, color = "Entity") +
      theme_minimal()
    
    ggplotly(p, tooltip = c('x', 'y'))
  })
  
  # Forecasting
  observeEvent(input$generate_forecast, {
    req(values$processed_data, values$time_column, input$forecast_entity, input$forecast_metric)
    
    entity_data <- values$processed_data %>%
      filter(!!sym(values$entity_column) == input$forecast_entity) %>%
      arrange(!!sym(values$time_column))
    
    forecast_result <- forecast_time_series_universal(
      entity_data, values$entity_column, values$time_column, 
      input$forecast_metric, input$forecast_periods
    )
    
    values$forecast_results <- list(
      entity = input$forecast_entity,
      metric = input$forecast_metric,
      historical_data = entity_data,
      forecast = forecast_result
    )
    
    showNotification("Forecast generated successfully!", type = "message")
  })
  
  output$forecast_plot <- renderPlotly({
    req(values$forecast_results)
    
    historical <- values$forecast_results$historical_data
    forecast_data <- values$forecast_results$forecast
    
    # Create future time periods
    last_time <- max(historical[[values$time_column]], na.rm = TRUE)
    future_times <- seq(last_time + 1, last_time + length(forecast_data$forecast))
    
    # Combine historical and forecast data
    plot_data <- data.frame(
      Time = c(historical[[values$time_column]], future_times),
      Value = c(historical[[values$forecast_results$metric]], forecast_data$forecast),
      Type = c(rep("Historical", nrow(historical)), rep("Forecast", length(forecast_data$forecast))),
      Lower = c(rep(NA, nrow(historical)), forecast_data$lower),
      Upper = c(rep(NA, nrow(historical)), forecast_data$upper)
    )
    
    p <- ggplot(plot_data, aes(x = Time, y = Value, color = Type)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, color = NA, fill = "blue") +
      labs(title = paste("Forecast for", values$forecast_results$entity),
           subtitle = paste("Metric:", values$forecast_results$metric, "| Method:", forecast_data$method),
           x = values$time_column, y = values$forecast_results$metric) +
      theme_minimal() +
      scale_color_manual(values = c("Historical" = "steelblue", "Forecast" = "red"))
    
    ggplotly(p, tooltip = c('x', 'y'))
  })
  
  output$forecast_table <- renderDataTable({
    req(values$forecast_results)
    
    last_time <- max(values$forecast_results$historical_data[[values$time_column]], na.rm = TRUE)
    future_times <- seq(last_time + 1, last_time + length(values$forecast_results$forecast$forecast))
    
    forecast_df <- data.frame(
      Period = future_times,
      Forecast = values$forecast_results$forecast$forecast,
      Lower_CI = values$forecast_results$forecast$lower,
      Upper_CI = values$forecast_results$forecast$upper
    )
    
    DT::datatable(forecast_df, options = list(pageLength = 10)) %>%
      DT::formatRound(c("Forecast", "Lower_CI", "Upper_CI"), digits = 2)
  })
  output$comparison_table <- renderDataTable({
    req(values$processed_data, input$compare_entities, input$compare_metric, input$generate_comparison)
    
    comparison_data <- values$processed_data %>%
      filter(!!sym(values$entity_column) %in% input$compare_entities)
    
    # If time column exists and time period is selected, filter by time
    if (!is.null(values$time_column) && !is.null(input$compare_time_period)) {
      comparison_data <- comparison_data %>%
        filter(!!sym(values$time_column) == input$compare_time_period)
    }
    
    # Select relevant columns for comparison
    comparison_cols <- c(values$entity_column)
    if (!is.null(values$time_column)) comparison_cols <- c(comparison_cols, values$time_column)
    comparison_cols <- c(comparison_cols, "composite_index", values$selected_indicators)
    
    comparison_table <- comparison_data %>%
      select(all_of(comparison_cols)) %>%
      arrange(desc(!!sym(input$compare_metric)))
    
    DT::datatable(comparison_table, options = list(scrollX = TRUE, pageLength = 10)) %>%
      DT::formatRound(c("composite_index", values$selected_indicators), digits = 2)
  })
  
  # Weight impact analysis
  output$weight_impact_plot <- renderPlotly({
    req(values$processed_data, values$processed_data_equal_weights, values$weights_applied)
    
    # Compare rankings between equal weights and custom weights
    equal_weights_ranking <- values$processed_data_equal_weights %>%
      select(Entity = !!sym(values$entity_column), Equal_Weights = composite_index) %>%
      mutate(Equal_Rank = rank(-Equal_Weights))
    
    custom_weights_ranking <- values$processed_data %>%
      select(Entity = !!sym(values$entity_column), Custom_Weights = composite_index) %>%
      mutate(Custom_Rank = rank(-Custom_Weights))
    
    combined_ranking <- merge(equal_weights_ranking, custom_weights_ranking, by = "Entity") %>%
      mutate(Rank_Change = Equal_Rank - Custom_Rank) %>%
      arrange(desc(abs(Rank_Change))) %>%
      head(20)
    
    p <- ggplot(combined_ranking, aes(x = reorder(Entity, abs(Rank_Change)), y = Rank_Change)) +
      geom_col(aes(fill = Rank_Change > 0), alpha = 0.8) +
      coord_flip() +
      labs(title = "Ranking Changes from Custom Weights",
           subtitle = "Top 20 entities with largest ranking changes",
           x = "Entity", y = "Rank Change (Equal - Custom)",
           fill = "Improved Ranking") +
      theme_minimal() +
      scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "green"))
    
    ggplotly(p, tooltip = c('x', 'y'))
  })
  
  output$weight_comparison_table <- renderDataTable({
    req(values$processed_data, values$processed_data_equal_weights, values$weights_applied)
    
    # Compare top entities between equal and custom weights
    equal_top <- values$processed_data_equal_weights %>%
      select(Entity = !!sym(values$entity_column), Equal_Score = composite_index) %>%
      arrange(desc(Equal_Score)) %>%
      mutate(Equal_Rank = row_number()) %>%
      head(20)
    
    custom_top <- values$processed_data %>%
      select(Entity = !!sym(values$entity_column), Custom_Score = composite_index) %>%
      arrange(desc(Custom_Score)) %>%
      mutate(Custom_Rank = row_number()) %>%
      head(20)
    
    comparison <- merge(equal_top, custom_top, by = "Entity", all = TRUE) %>%
      mutate(
        Equal_Rank = ifelse(is.na(Equal_Rank), ">20", as.character(Equal_Rank)),
        Custom_Rank = ifelse(is.na(Custom_Rank), ">20", as.character(Custom_Rank)),
        Score_Change = Custom_Score - Equal_Score
      ) %>%
      arrange(Custom_Rank)
    
    DT::datatable(comparison, options = list(pageLength = 10)) %>%
      DT::formatRound(c("Equal_Score", "Custom_Score", "Score_Change"), digits = 3)
  })
  
  # Multi-entity forecast plot
  output$multi_forecast_plot <- renderPlotly({
    req(values$processed_data, values$time_column, input$multi_forecast_entities)
    
    if (length(input$multi_forecast_entities) == 0) return()
    
    # Generate forecasts for multiple entities
    multi_forecasts <- list()
    
    for (entity in input$multi_forecast_entities) {
      entity_data <- values$processed_data %>%
        filter(!!sym(values$entity_column) == entity) %>%
        arrange(!!sym(values$time_column))
      
      if (nrow(entity_data) >= 3) {
        forecast_result <- forecast_time_series_universal(
          entity_data, values$entity_column, values$time_column, 
          "composite_index", 5
        )
        
        multi_forecasts[[entity]] <- forecast_result
      }
    }
    
    if (length(multi_forecasts) == 0) {
       return(plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = "No valid forecasts available"))
    }
    
    # Create combined plot data
    plot_data_list <- list()
    
    for (entity in names(multi_forecasts)) {
      entity_data <- values$processed_data %>%
        filter(!!sym(values$entity_column) == entity) %>%
        arrange(!!sym(values$time_column))
      
      last_time <- max(entity_data[[values$time_column]], na.rm = TRUE)
      future_times <- seq(last_time + 1, last_time + 5)
      
      # Historical data
      hist_data <- data.frame(
        Entity = entity,
        Time = entity_data[[values$time_column]],
        Value = entity_data$composite_index,
        Type = "Historical"
      )
      
      # Forecast data
      forecast_data <- data.frame(
        Entity = entity,
        Time = future_times,
        Value = multi_forecasts[[entity]]$forecast,
        Type = "Forecast"
      )
      
      plot_data_list[[paste0(entity, "_hist")]] <- hist_data
      plot_data_list[[paste0(entity, "_forecast")]] <- forecast_data
    }
    
    combined_data <- do.call(rbind, plot_data_list)
    
    p <- ggplot(combined_data, aes(x = Time, y = Value, color = Entity, linetype = Type)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.5) +
      labs(title = "Multi-Entity Forecast Comparison",
           x = values$time_column, y = "Composite Index") +
      theme_minimal() +
      scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed"))
    
    ggplotly(p, tooltip = c('x', 'y'))
  })
  # === COMPARISON TAB FIXES ===
# FIXED: Comparison Plot with comprehensive error handling
output$comparison_plot <- renderPlotly({
  req(values$processed_data, input$compare_entities, input$compare_metric)
  
  tryCatch({
    # Check if we have valid entities selected
    if (is.null(input$compare_entities) || length(input$compare_entities) == 0) {
      return(plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = 'No data available') %>% 
             layout(title = "Please select entities to compare"))
    }
    
    # Check if the comparison metric exists
    if (is.null(input$compare_metric) || !input$compare_metric %in% names(values$processed_data)) {
      return(plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = 'No data available') %>% 
             layout(title = "Please select a valid metric to compare"))
    }
    
    # Filter data for selected entities
    comparison_data <- values$processed_data %>%
      filter(!!sym(values$entity_column) %in% input$compare_entities)
    
    if (nrow(comparison_data) == 0) {
      return(plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = 'No data available') %>% 
             layout(title = "No data found for selected entities"))
    }
    
    # Create bar chart comparison
    comparison_data <- comparison_data %>%
      arrange(desc(!!sym(input$compare_metric)))
    
    p <- ggplot(comparison_data, aes(x = reorder(!!sym(values$entity_column), 
                                                !!sym(input$compare_metric)), 
                                    y = !!sym(input$compare_metric))) +
      geom_col(aes(fill = !!sym(values$entity_column)), alpha = 0.8, show.legend = FALSE) +
      coord_flip() +
      labs(title = paste("Entity Comparison:", input$compare_metric),
           x = "Entity",
           y = input$compare_metric) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
    
    # Add value labels on bars
    p <- p + geom_text(aes(label = round(!!sym(input$compare_metric), 2)), 
                      hjust = -0.1, size = 3)
    
    # Convert to plotly
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE)
    
  }, error = function(e) {
    # Return error plot
    plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = 'No data available') %>% 
      layout(title = list(text = paste("Error creating comparison plot:", e$message)))
  })
})
# FIXED: Update comparison choices when data changes
observe({
  req(values$processed_data, values$entity_column)
  
  # Update entity choices
  entity_choices <- unique(values$processed_data[[values$entity_column]])
  updateSelectizeInput(session, "compare_entities", 
                      choices = entity_choices)
  
  # Update metric choices
  numeric_cols <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
  metric_choices <- c("composite_index", values$selected_indicators)
  metric_choices <- intersect(metric_choices, numeric_cols)
  
  updateSelectInput(session, "compare_metric", 
                   choices = metric_choices,
                   selected = "composite_index")
  
  # Update time period choices if time column exists
  if (!is.null(values$time_column) && values$time_column %in% names(values$processed_data)) {
    time_choices <- sort(unique(values$processed_data[[values$time_column]]))
    updateSelectInput(session, "compare_time_period", 
                     choices = as.list(c("All Periods" = "", time_choices)))
  }
})
# FIXED: Comparison summary
output$comparison_summary <- renderText({
  req(values$processed_data, input$compare_entities, input$compare_metric)
  
  tryCatch({
    if (length(input$compare_entities) == 0) {
      return("Please select entities to compare")
    }
    
    comparison_data <- values$processed_data %>%
      filter(!!sym(values$entity_column) %in% input$compare_entities)
    
    if (nrow(comparison_data) == 0) {
      return("No data available for selected entities")
    }
    
    # Calculate summary statistics
    metric_values <- comparison_data[[input$compare_metric]]
    
    if (all(is.na(metric_values))) {
      return("No valid data for selected metric")
    }
    
    best_entity <- comparison_data[which.max(metric_values), values$entity_column]
    worst_entity <- comparison_data[which.min(metric_values), values$entity_column]
    avg_value <- round(mean(metric_values, na.rm = TRUE), 3)
    range_value <- round(max(metric_values, na.rm = TRUE) - min(metric_values, na.rm = TRUE), 3)
    
    paste0(
      "Comparison Summary for ", input$compare_metric, ":
",
      "Best: ", best_entity, " (", round(max(metric_values, na.rm = TRUE), 3), ")
",
      "Worst: ", worst_entity, " (", round(min(metric_values, na.rm = TRUE), 3), ")
",
      "Average: ", avg_value, "
",
      "Range: ", range_value
    )
    
  }, error = function(e) {
    paste("Error calculating comparison summary:", e$message)
  })
})
# FIXED: Excel sheet loading with proper error handling
observeEvent(input$load_excel_sheet, {
  req(input$file, input$excel_sheet)
  
  tryCatch({
    # Show loading message
    showNotification("Loading Excel sheet...", type = "message", duration = 3)
    
    # Read the selected sheet
    if (tools::file_ext(input$file$datapath) %in% c("xlsx", "xls")) {
      values$raw_data <- read_excel(input$file$datapath, sheet = input$excel_sheet)
    } else {
      values$raw_data <- read.csv(input$file$datapath, 
                                 header = input$header,
                                 sep = input$sep,
                                 quote = input$quote,
                                 stringsAsFactors = FALSE)
    }
    
    # Clean column names
    names(values$raw_data) <- make.names(names(values$raw_data))
    
    # Remove completely empty rows and columns
    values$raw_data <- values$raw_data[rowSums(is.na(values$raw_data)) != ncol(values$raw_data), ]
    values$raw_data <- values$raw_data[, colSums(is.na(values$raw_data)) != nrow(values$raw_data)]
    
    # Convert character columns that should be numeric
    for (col in names(values$raw_data)) {
      if (is.character(values$raw_data[[col]])) {
        # Try to convert to numeric if it looks like numbers
        numeric_test <- suppressWarnings(as.numeric(values$raw_data[[col]]))
        if (sum(!is.na(numeric_test)) > sum(!is.na(values$raw_data[[col]])) * 0.5) {
          values$raw_data[[col]] <- numeric_test
        }
      }
    }
    
    # Analyze columns safely
    values$column_analysis <- detect_column_roles(values$raw_data)
    
    showNotification("Excel sheet loaded successfully!", type = "message", duration = 3)
    
  }, error = function(e) {
showNotification(paste("Error loading Excel sheet:", e$message), type = "error", duration = 5)
    values$raw_data <- NULL
    values$column_analysis <- NULL
  })
})
# FIXED: File upload observer with better error handling
observeEvent(input$file, {
  req(input$file)
  
  tryCatch({
    file_ext <- tools::file_ext(input$file$datapath)
    
    if (file_ext %in% c("xlsx", "xls")) {
      # Get sheet names
      sheet_names <- excel_sheets(input$file$datapath)
      updateSelectInput(session, "excel_sheet", choices = sheet_names, selected = sheet_names[1])
      
      # Load first sheet by default
      values$raw_data <- read_excel(input$file$datapath, sheet = sheet_names[1])
      
    } else if (file_ext == "csv") {
      values$raw_data <- read.csv(input$file$datapath, 
                                 header = input$header,
                                 sep = input$sep,
                                 quote = input$quote,
                                 stringsAsFactors = FALSE)
    } else {
showNotification("Unsupported file format. Please use CSV or Excel files.", type = "error", duration = 5)
      return()
    }
    
    # Clean and process data
    if (!is.null(values$raw_data)) {
      names(values$raw_data) <- make.names(names(values$raw_data))
      
      # Remove empty rows/columns
      values$raw_data <- values$raw_data[rowSums(is.na(values$raw_data)) != ncol(values$raw_data), ]
      values$raw_data <- values$raw_data[, colSums(is.na(values$raw_data)) != nrow(values$raw_data)]
      
      # Analyze columns
      values$column_analysis <- detect_column_roles(values$raw_data)
      
      showNotification("File uploaded successfully!", type = "message", duration = 3)
    }
    
  }, error = function(e) {
showNotification(paste("Error uploading file:", e$message), type = "error", duration = 5)
    values$raw_data <- NULL
    values$column_analysis <- NULL
  })
})
# === ENHANCED HEADER OPTIONS ===
# ENHANCED: Server-side logic for header options
output$file_uploaded <- reactive({
  return(!is.null(input$file))
})
outputOptions(output, 'file_uploaded', suspendWhenHidden = FALSE)
output$is_excel_file <- reactive({
  if (is.null(input$file)) return(FALSE)
  file_ext <- tools::file_ext(input$file$datapath)
  return(file_ext %in% c('xlsx', 'xls'))
})
outputOptions(output, 'is_excel_file', suspendWhenHidden = FALSE)
# Enhanced file loading with header options
load_data_with_options <- function() {
  req(input$file)
  
  tryCatch({
    file_ext <- tools::file_ext(input$file$datapath)
    
    if (file_ext %in% c('xlsx', 'xls')) {
      # Excel file handling
      sheet_to_use <- if (!is.null(input$excel_sheet)) input$excel_sheet else 1
      
      # Read with header options
      if (input$first_row_header) {
        raw_data <- read_excel(input$file$datapath, sheet = sheet_to_use)
      } else {
        raw_data <- read_excel(input$file$datapath, sheet = sheet_to_use, col_names = FALSE)
        # Generate column names
        names(raw_data) <- paste0('V', 1:ncol(raw_data))
      }
      
    } else if (file_ext == 'csv') {
      # CSV file handling
      raw_data <- read.csv(input$file$datapath, 
                          header = input$first_row_header,
                          sep = input$sep,
                          quote = input$quote,
                          stringsAsFactors = FALSE)
      
      # If no header, generate column names
      if (!input$first_row_header) {
        names(raw_data) <- paste0('V', 1:ncol(raw_data))
      }
    } else {
      stop('Unsupported file format')
    }
    
    # Handle first column as row identifiers
    if (input$first_col_header && ncol(raw_data) > 1) {
      # Set first column as row names and remove it from data
      row.names(raw_data) <- raw_data[, 1]
      raw_data <- raw_data[, -1, drop = FALSE]
      
      showNotification('First column set as row identifiers', 
                      type = 'message', duration = 3)
    }
    
    # Clean column names
    names(raw_data) <- make.names(names(raw_data))
    
    # Remove completely empty rows and columns
    raw_data <- raw_data[rowSums(is.na(raw_data)) != ncol(raw_data), ]
    raw_data <- raw_data[, colSums(is.na(raw_data)) != nrow(raw_data)]
    
    # Auto-convert character columns that should be numeric
    for (col in names(raw_data)) {
      if (is.character(raw_data[[col]])) {
        numeric_test <- suppressWarnings(as.numeric(raw_data[[col]]))
        if (sum(!is.na(numeric_test)) > sum(!is.na(raw_data[[col]])) * 0.5) {
          raw_data[[col]] <- numeric_test
        }
      }
    }
    
    return(raw_data)
    
  }, error = function(e) {
    showNotification(paste('Error loading file:', e$message), 
                    type = 'error', duration = 5)
    return(NULL)
  })
  # === COMPOSITE INDEX FUNCTIONALITY ===
  
  # COMPOSITE INDEX SERVER LOGIC
  
  # Direction UI
  output$direction_ui <- renderUI({
    req(values$column_analysis)
    
    numeric_cols <- names(values$column_analysis)[
      sapply(values$column_analysis, function(x) x$type == "numeric")
    ]
    
    if (length(numeric_cols) > 0) {
      direction_inputs <- lapply(numeric_cols, function(col) {
        radioButtons(paste0("direction_", col), 
                    paste("Direction for", col, ":"),
                     choices = as.list(c("Positive" = "positive", "Negative" = "negative")),
                    selected = "positive",
                    inline = TRUE)
      })
      do.call(tagList, direction_inputs)
    }
  })
  
  # Dynamic Weight Sliders
  output$dynamic_slider_ui <- renderUI({
    req(values$column_analysis)
    
    numeric_cols <- names(values$column_analysis)[
      sapply(values$column_analysis, function(x) x$type == "numeric")
    ]
    
    if (length(numeric_cols) > 0) {
      weight_sliders <- lapply(numeric_cols, function(col) {
        sliderInput(paste0("weight_", col),
                   paste("Weight for", col, ":"),
                   min = 0, max = 1, value = 1/length(numeric_cols),
                   step = 0.01)
      })
      do.call(tagList, weight_sliders)
    }
  })
  
  # Weights Table
  output$weights_table <- renderDataTable({
    req(values$column_analysis)
    
    numeric_cols <- names(values$column_analysis)[
      sapply(values$column_analysis, function(x) x$type == "numeric")
    ]
    
    if (length(numeric_cols) > 0) {
      weights_df <- data.frame(
        Column = numeric_cols,
        Weight = sapply(numeric_cols, function(col) {
          weight_input <- input[[paste0("weight_", col)]]
          if (is.null(weight_input)) 1/length(numeric_cols) else weight_input
        }),
        stringsAsFactors = FALSE
      )
      
      # Normalize weights
      weights_df$Weight <- weights_df$Weight / sum(weights_df$Weight)
      weights_df$Weight <- round(weights_df$Weight, 3)
      
      values$current_weights <- weights_df
      weights_df
    }
  }, options = list(pageLength = 10, searching = FALSE))
  
  # Sum of weights display
  output$sum_weights <- renderText({
    req(values$current_weights)
    paste("Sum of weights:", round(sum(values$current_weights$Weight), 3))
  })
  
  # Scaling message
  observeEvent(input$save_scaling, {
    output$scaling_message <- renderText({
      paste("Scaling method set to:", input$scaling)
    })
  })
  
  # Imputation message
  observeEvent(input$apply_imputation, {
    req(values$raw_data)
    
    tryCatch({
      data_to_impute <- values$raw_data
      
      if (input$impute_method == "Interpolation") {
        for (col in names(data_to_impute)) {
          if (is.numeric(data_to_impute[[col]])) {
            data_to_impute[[col]] <- na.approx(data_to_impute[[col]], na.rm = FALSE)
          }
        }
      } else if (input$impute_method == "Holt-Winters") {
        for (col in names(data_to_impute)) {
          if (is.numeric(data_to_impute[[col]]) && sum(!is.na(data_to_impute[[col]])) > 10) {
            ts_data <- ts(data_to_impute[[col]])
            hw_model <- tryCatch(HoltWinters(ts_data), error = function(e) NULL)
            if (!is.null(hw_model)) {
              fitted_values <- fitted(hw_model)[,1]
              na_indices <- is.na(data_to_impute[[col]])
              if (length(fitted_values) == length(data_to_impute[[col]])) {
                data_to_impute[[col]][na_indices] <- fitted_values[na_indices]
              }
            }
          }
        }
      } else if (input$impute_method == "MissForest") {
        numeric_data <- data_to_impute[sapply(data_to_impute, is.numeric)]
        if (ncol(numeric_data) > 1) {
          imputed <- missForest(numeric_data)
          data_to_impute[names(numeric_data)] <- imputed$ximp
        }
      }
      
      values$imputed_data <- data_to_impute
      
      output$impute_message <- renderText({
        paste("Imputation completed using:", input$impute_method)
      })
      
    }, error = function(e) {
      output$impute_message <- renderText({
        paste("Imputation failed:", e$message)
      })
    })
  })
  
  # Generate Composite Index
  observeEvent(input$process, {
    req(values$raw_data, values$current_weights)
    
    tryCatch({
      # Use imputed data if available, otherwise raw data
      data_to_process <- if (!is.null(values$imputed_data)) {
        values$imputed_data
      } else {
        values$raw_data
      }
      
      # Get numeric columns
      numeric_cols <- values$current_weights$Column
      processed_data <- data_to_process[numeric_cols]
      
      # Apply directions
      for (col in numeric_cols) {
        direction_input <- input[[paste0("direction_", col)]]
        if (!is.null(direction_input) && direction_input == "negative") {
          processed_data[[col]] <- -processed_data[[col]]
        }
      }
      
      # Apply scaling
      if (input$scaling == "standard") {
        processed_data <- scale(processed_data)
      } else if (input$scaling == "minimax") {
        processed_data <- apply(processed_data, 2, function(x) {
          (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
        })
      }
      
      # Apply weights
      for (i in 1:nrow(values$current_weights)) {
        col <- values$current_weights$Column[i]
        weight <- values$current_weights$Weight[i]
        processed_data[, col] <- processed_data[, col] * weight
      }
      
      # Calculate composite index
      composite_index <- rowSums(processed_data, na.rm = TRUE)
      
      # Normalize index to 0-1 scale
      composite_index <- (composite_index - min(composite_index, na.rm = TRUE)) / 
                        (max(composite_index, na.rm = TRUE) - min(composite_index, na.rm = TRUE))
      
      # Store results
      values$processed_data <- processed_data
      values$composite_index <- composite_index
      values$final_data <- cbind(data_to_process, Index = composite_index)
      
      showNotification("Composite Index generated successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error generating index:", e$message), type = "error", duration = 5)
    })
  })
  
  
  # OUTPUT HANDLERS FOR COMPOSITE INDEX
  
  # Display tables
  output$uploaded_table_composite <- renderDataTable({
    req(values$raw_data)
    values$raw_data
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$scaled_table_composite <- renderDataTable({
    req(values$final_data)
    values$final_data
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Cronbach Alpha
  output$cronbach <- renderPrint({
    req(values$processed_data)
    
    tryCatch({
      alpha_result <- psych::alpha(values$processed_data)
      cat("Cronbach Alpha:", round(alpha_result$total$std.alpha, 3), "
  ")
      cat("Interpretation:", 
          if (alpha_result$total$std.alpha >= 0.9) "Excellent" 
          else if (alpha_result$total$std.alpha >= 0.8) "Good"
          else if (alpha_result$total$std.alpha >= 0.7) "Acceptable"
          else if (alpha_result$total$std.alpha >= 0.6) "Questionable"
          else "Poor")
    }, error = function(e) {
      cat("Could not calculate Cronbach Alpha:", e$message)
    })
  })
  
  # Coefficient of Variation
  output$cv_table <- renderDataTable({
    req(values$processed_data)
    
    cv_results <- data.frame(
      Variable = names(values$processed_data),
      Mean = round(sapply(values$processed_data, mean, na.rm = TRUE), 3),
      SD = round(sapply(values$processed_data, sd, na.rm = TRUE), 3),
      CV = round(sapply(values$processed_data, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)), 3),
      stringsAsFactors = FALSE
    )
    
    cv_results
  }, options = list(pageLength = 10))
  
  # PCA Analysis
  output$pca_summary <- renderPrint({
    req(values$processed_data)
    
    tryCatch({
      pca_result <- prcomp(values$processed_data, scale. = TRUE)
      summary(pca_result)
    }, error = function(e) {
      cat("Could not perform PCA:", e$message)
    })
  })
  
  output$pca_plot <- renderPlot({
    req(values$processed_data)
    
    tryCatch({
      pca_result <- prcomp(values$processed_data, scale. = TRUE)
      biplot(pca_result, main = "PCA Biplot")
    }, error = function(e) {
      plot(1, 1, type = "n", main = "PCA Plot Error")
      text(1, 1, paste("Error:", e$message))
    })
  })
  
  # Sensitivity Analysis
  output$sensitivity_table <- renderDataTable({
    req(values$final_data, values$current_weights)
    
    tryCatch({
      index_base <- values$final_data$Index
      results <- data.frame(Column = character(), Correlation = numeric(), stringsAsFactors = FALSE)
      
      for (i in seq_len(nrow(values$current_weights))) {
        mod_weights <- values$current_weights
        mod_weights$Weight[i] <- mod_weights$Weight[i] * 1.05
        mod_weights$Weight <- mod_weights$Weight / sum(mod_weights$Weight)
        
        # Recalculate index with modified weights
        mod_data <- values$processed_data
        for (j in seq_len(nrow(mod_weights))) {
          col <- mod_weights$Column[j]
          wt <- mod_weights$Weight[j]
          if (col %in% colnames(mod_data)) {
            mod_data[, col] <- values$processed_data[, col] / values$current_weights$Weight[j] * wt
          }
        }
        
        mod_index <- rowSums(mod_data, na.rm = TRUE)
        mod_index <- (mod_index - min(mod_index)) / (max(mod_index) - min(mod_index))
        
        correlation <- cor(index_base, mod_index, use = "complete.obs")
        results[i, ] <- list(mod_weights$Column[i], round(correlation, 3))
      }
      
      colnames(results) <- c("Variable", "Correlation with Original Index")
      results
      
    }, error = function(e) {
      data.frame(Error = paste("Sensitivity analysis failed:", e$message))
    })
  }, options = list(pageLength = 10))
  
  # Correlation Heatmap
  output$sensitivity_heatmap <- renderPlot({
    req(values$final_data)
    
    tryCatch({
      cor_matrix <- cor(values$final_data[sapply(values$final_data, is.numeric)], 
                       use = "pairwise.complete.obs")
      
      corrplot(cor_matrix, 
               method = "color", 
               type = "upper", 
               order = "hclust", 
               addCoef.col = "black",
               tl.col = "black", 
               tl.srt = 45, 
               col = colorRampPalette(c("blue", "white", "red"))(200),
               number.cex = 0.7,
               title = "Correlation Matrix with Composite Index")
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Correlation Heatmap Error")
      text(1, 1, paste("Error:", e$message))
    })
  })
  
  # Sankey Network
  output$sankey <- renderSankeyNetwork({
    req(values$current_weights)
    
    tryCatch({
      links <- data.frame(
        source = rep(0, nrow(values$current_weights)),
        target = seq(1, nrow(values$current_weights)),
        value = values$current_weights$Weight
      )
      
      nodes <- data.frame(
        name = c("Composite Index", 
                 paste0(values$current_weights$Column, " (", 
                       round(values$current_weights$Weight, 2), ")"))
      )
      
      sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target", Value = "value",
                    NodeID = "name", fontSize = 12, nodeWidth = 30)
    }, error = function(e) {
      NULL
    })
  })
  
  # Download Handler
  output$download_scaled <- downloadHandler(
    filename = function() {
      paste0("composite_index_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(values$final_data)
      write.csv(values$final_data, file, row.names = FALSE)
    }
  )
}
# File upload observer
observeEvent(input$file, {
  if (!is.null(input$file)) {
    file_ext <- tools::file_ext(input$file$datapath)
    
    if (file_ext %in% c('xlsx', 'xls')) {
      # Get sheet names for Excel files
      sheet_names <- excel_sheets(input$file$datapath)
      updateSelectInput(session, 'excel_sheet', choices = sheet_names, selected = sheet_names[1])
    }
    
    # Load data with current settings
    values$raw_data <- load_data_with_options()
    
    if (!is.null(values$raw_data)) {
      values$column_analysis <- detect_column_roles(values$raw_data)
      showNotification('File uploaded successfully!', type = 'success', duration = 3)
    }
  }
})
# Reload data when settings change
observeEvent(input$reload_data, {
  if (!is.null(input$file)) {
    values$raw_data <- load_data_with_options()
    
    if (!is.null(values$raw_data)) {
      values$column_analysis <- detect_column_roles(values$raw_data)
      showNotification('Data reloaded with new settings!', type = 'success', duration = 3)
    }
  }
})
# Excel sheet change observer
observeEvent(input$excel_sheet, {
  if (!is.null(input$file) && !is.null(input$excel_sheet)) {
    values$raw_data <- load_data_with_options()
    
    if (!is.null(values$raw_data)) {
      values$column_analysis <- detect_column_roles(values$raw_data)
      showNotification(paste('Loaded sheet:', input$excel_sheet), type = 'message', duration = 3)
    }
  }
})
}
# Launch the app
return(shinyApp(ui = ui, server = server))
}