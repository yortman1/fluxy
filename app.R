library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(viridis) # For color palettes

# User-configurable default variables
# Edit these to change which variables are automatically added when loading a file
default_vars_patterns <- c(
  "Tleaf",  # Temperature variables
  "gsw"     # Conductance variables
)

ui <- fluidPage(
  titlePanel(div("🍃 FLUXY The LI-6800 Time-Series Visualizer", span(style = "color: #666; font-size: 12px; margin-left: 10px;", "v1.0"))),
  # Add JavaScript to set default variables after page load
  tags$head(
    tags$script(HTML("
      $(document).ready(function() {
        // This function will try to set default variables once data is loaded
        function setDefaultVars() {
          var selectize = $('#vars')[0].selectize;
          
          // Only proceed if selectize exists and has no selected items
          if (!selectize || selectize.items.length > 0) return;
          
          var defaultVars = ['Tleaf', 'gsw'];
          var varsToAdd = [];
          var options = selectize.options;
          
          // Priority 1: Exact matches
          for (var value in options) {
            if (defaultVars.indexOf(value) !== -1) {
              varsToAdd.push(value);
            }
          }
          
          // Add variables and trigger update if we found matches
          if (varsToAdd.length > 0) {
            // Set values
            selectize.setValue(varsToAdd, true);
            
            // Trigger change events after a short delay
            setTimeout(function() {
              var event = new Event('change');
              $('#vars')[0].dispatchEvent(event);
            }, 100);
          }
        }
        
        // Try twice after file loads
        $(document).on('shiny:inputchanged', function(event) {
          if (event.name === 'file') {
            setTimeout(setDefaultVars, 2000);
            setTimeout(setDefaultVars, 4000);
          }
        });
        
        // Try once at startup
        setTimeout(setDefaultVars, 2000);
      });
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload LI-6800 File"),
      
      # Use tabsetPanel for organization
      tabsetPanel(
        # Tab 1: Variables
        tabPanel("Variables",
          h4("Select Variables"),
          selectizeInput("vars", "Variables to Plot (Hit TAB to return to VAR field)", 
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(maxItems = 8)),
          
          # Add outlier filter checkbox
          checkboxInput("ignore_outliers", "Ignore Extreme Outliers (±1%)", value = TRUE),
          conditionalPanel(
            condition = "input.ignore_outliers == true",
            numericInput("outlier_percentile", "Outlier Percentile to Ignore (%)", 
                        value = 1, min = 0.1, max = 10, step = 0.1)
          ),
          
          # Add y-axis range sliders (dynamically generated)
          uiOutput("y_range_sliders")
        ),
        
        # Tab 2: Categories
        tabPanel("Categories",
          h4("Category Management"),
          textInput("new_category", "New Category Name"),
          actionButton("add_category", "Add Category"),
          selectInput("selected_category", "Select Category to Assign", choices = NULL),
          fluidRow(
            column(6, actionButton("add_selection", "Add Box Selection to Category")),
            column(6, actionButton("clear_selection", "Clear Selection"))
          ),
          
          hr(),
          h4("Ranges in Selected Category"),
          tableOutput("range_table"),
          actionButton("remove_last", "Remove Last Range")
        ),
        
        # Tab 3: Match Detect
        tabPanel("Match Detect",
          h4("Auto-Detect Licor Matching Events"),
          p("This feature detects periods when the LI-6800 was likely performing a matching calibration."),
          
          # Gap detection method
          radioButtons("matching_method", "Detection Method",
                     choices = c("Time Gap Detection" = "gap", 
                                "Value Spike Detection" = "spike"),
                     selected = "gap"),
          
          # Gap detection parameters
          conditionalPanel(
            condition = "input.matching_method == 'gap'",
            sliderInput("gap_threshold", "Minimum Gap Size (seconds)",
                      min = 5, max = 120, value = 40, step = 5),
            sliderInput("buffer_time", "Buffer Time to Add (seconds)",
                      min = 5, max = 60, value = 20, step = 5)
          ),
          
          # Spike detection parameters (original method)
          conditionalPanel(
            condition = "input.matching_method == 'spike'",
            sliderInput("matching_sensitivity", "Detection Sensitivity",
                       min = 1, max = 10, value = 5, step = 1),
            numericInput("matching_window", "Window Size Around Matching (seconds)",
                        value = 30, min = 10, max = 300),
            
            # Variables to check
            checkboxGroupInput("matching_vars", "Variables to Check for Matching",
                              choices = c("CO2_r", "CO2_s", "H2O_r", "H2O_s", "A"),
                              selected = c("CO2_r", "CO2_s"))
          ),
          
          # Action buttons - Detection controls
          div(style = "margin-bottom: 10px;",
            fluidRow(
              column(6, actionButton("detect_matching", "Detect Matching Events")),
              column(6, actionButton("clear_matching", "Clear Detection"))
            )
          ),
          
          # Action buttons - Category controls
          div(style = "margin-bottom: 15px;",
            fluidRow(
              column(6, actionButton("exclude_matching", "Create Exclusion Category")),
              column(6, actionButton("clear_exclusion", "Clear Exclusion Category"))
            )
          ),
          
          # Results
          h4("Detected Matching Events"),
          tableOutput("matching_events"),
          verbatimTextOutput("matching_info")
        ),
        
        # Tab 4: Export (moved to the end)
        tabPanel("Export",
          h4("Export Data"),
          radioButtons("export_type", "Export Type",
                     choices = c("Consolidated Table" = "consolidated",
                                "Separate Tables" = "separate"),
                     selected = "consolidated"),
          selectInput("export_format", "Export Format", 
                     choices = c("CSV Files" = "csv", 
                                 "RData File" = "rdata",
                                 "R Environment" = "env")),
          textInput("export_prefix", "File Prefix (for CSV/RData)", 
                   value = "li6800_category_"),
          checkboxInput("add_category_column", "Add Category Column to Data", value = TRUE),
          conditionalPanel(
            condition = "input.add_category_column == true",
            textInput("category_column", "Category Column Name", 
                     value = "fluxy_cat")
          ),
          actionButton("export_data", "Export Categorized Data"),
          verbatimTextOutput("export_info")
        )
      )
    ),
    
    mainPanel(
      plotlyOutput("multiPlot", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store categories and their data points
  categories <- reactiveVal(list())
  current_selection <- reactiveVal(NULL)
  available_vars <- reactiveVal(NULL)
  
  data <- reactive({
    req(input$file)
    path <- input$file$datapath
    
    tryCatch({
      # Print what we're reading to help debug
      message("Reading file: ", path)
      
      # Read header to get column names
      message("Reading header...")
    header <- fread(path, skip = 64, nrows = 1, header = FALSE)
    colnames <- unlist(header, use.names = FALSE)
      
      # Print column names to console for debugging
      message("Found columns: ", paste(colnames, collapse=", "))
      
      # Read actual data
      message("Reading data...")
    dt <- fread(path, skip = 66, header = FALSE)
    setnames(dt, colnames)
      
      # Store available variables and update UI
      available_vars(colnames)
      message("Variables stored, total: ", length(colnames))
      
      # Set a flag to trigger default variable selection
      session$userData$need_default_vars <- TRUE
    
    return(dt)
    }, error = function(e) {
      message("Error reading file: ", e$message)
      showNotification(paste("Error loading file:", e$message), type = "error")
      return(data.frame())
    })
  })
  
  # Handle default variable selection
  observeEvent(available_vars(), {
    if (!is.null(session$userData$need_default_vars) && 
        session$userData$need_default_vars && 
        (is.null(input$vars) || length(input$vars) == 0)) {
      
      # We're handling this need, clear the flag
      session$userData$need_default_vars <- FALSE
      
      # Look for default variables
      all_vars <- available_vars()
      selected_vars <- c()
      
      for (pattern in default_vars_patterns) {
        matches <- grep(pattern, all_vars, ignore.case = TRUE, value = TRUE)
        if (length(matches) > 0) {
          selected_vars <- c(selected_vars, matches[1])
        }
      }
      
      if (length(selected_vars) > 0) {
        message("Setting default variables: ", paste(selected_vars, collapse=", "))
        updateSelectizeInput(session, "vars", 
                           choices = all_vars, 
                           selected = selected_vars)
      }
    }
  })
  
  # Update variable selector when file is loaded
  observe({
    req(available_vars())
    
    # Only update choices, not selection
    current_selection <- input$vars
    updateSelectizeInput(session, "vars", 
                         choices = available_vars(), 
                         selected = current_selection)
  })
  
  # Dynamic Y-axis range sliders
  output$y_range_sliders <- renderUI({
    req(input$vars, data())
    
    sliders <- lapply(input$vars, function(var) {
      # Get data range for this variable
      var_data <- data()[[var]]
      
      # Filter outliers if the option is selected
      if (!is.null(input$ignore_outliers) && input$ignore_outliers) {
        # Get the percentile to filter
        percentile <- min(max(input$outlier_percentile, 0.1), 10) / 100
        
        # Calculate the percentile values
        lower_bound <- quantile(var_data, percentile, na.rm = TRUE)
        upper_bound <- quantile(var_data, 1 - percentile, na.rm = TRUE)
        
        # Filter data between these percentiles
        filtered_data <- var_data[var_data >= lower_bound & var_data <= upper_bound]
        
        # Only use filtered data if we have enough values left
        if (length(filtered_data) >= 10) {
          var_data <- filtered_data
        }
      }
      
      # Get min/max values, handling NA/Inf values
      var_min <- min(var_data, na.rm = TRUE)
      var_max <- max(var_data, na.rm = TRUE)
      
      # Check for valid range
      if (!is.finite(var_min) || !is.finite(var_max)) {
        var_min <- 0
        var_max <- 100
      }
      
      # Adjust range
      range_min <- min(0, var_min * 0.9)  # Include 0 or slightly below min
      range_max <- var_max * 1.1  # Go slightly above max
      
      # Format values with reasonable decimal places
      format_value <- function(x) {
        if (abs(x) < 0.1) {
          # Small values: 3 decimal places
          return(round(x, 3))
        } else if (abs(x) < 1) {
          # Medium-small values: 2 decimal places
          return(round(x, 2))
        } else if (abs(x) < 10) {
          # Medium values: 1 decimal place
          return(round(x, 1))
        } else {
          # Large values: no decimal places
          return(round(x, 0))
        }
      }
      
      range_min <- format_value(range_min)
      range_max <- format_value(range_max)
      
      # Default range
      default_range <- c(range_min, range_max)
      
      # Create slider ID
      slider_id <- paste0("range_", var)
      
      # Determine step size based on the range
      if (range_max <= 1) {
        step_size <- 0.01  # Fine steps for small values
      } else if (range_max <= 10) {
        step_size <- 0.1
      } else if (range_max <= 100) {
        step_size <- 1
      } else {
        step_size <- 10
      }
      
      # Create the slider
      sliderInput(slider_id, paste(var, "Range"),
                 min = range_min,
                 max = range_max,
                 value = default_range,
                 step = step_size)
    })
    
    do.call(tagList, sliders)
  })
  
  # Function to get current y-range for a variable
  get_y_range <- function(var) {
    slider_id <- paste0("range_", var)
    
    # Try to get the value from input
    if (!is.null(input[[slider_id]])) {
      return(input[[slider_id]])
    }
    
    # If no slider yet, try to get data range
    if (!is.null(data()) && var %in% names(data())) {
      var_data <- data()[[var]]
      
      # Filter outliers if the option is selected
      if (!is.null(input$ignore_outliers) && input$ignore_outliers) {
        # Get the percentile to filter
        percentile <- min(max(input$outlier_percentile, 0.1), 10) / 100
        
        # Calculate the percentile values
        lower_bound <- quantile(var_data, percentile, na.rm = TRUE)
        upper_bound <- quantile(var_data, 1 - percentile, na.rm = TRUE)
        
        # Filter data between these percentiles
        filtered_data <- var_data[var_data >= lower_bound & var_data <= upper_bound]
        
        # Only use filtered data if we have enough values left
        if (length(filtered_data) >= 10) {
          var_data <- filtered_data
        }
      }
      
      var_min <- min(var_data, na.rm = TRUE)
      var_max <- max(var_data, na.rm = TRUE)
      
      if (is.finite(var_min) && is.finite(var_max)) {
        return(c(min(0, var_min * 0.9), var_max * 1.1))
      }
    }
    
    # Default fallback
    return(c(0, 100))
  }
  
  # Add new category
  observeEvent(input$add_category, {
    req(input$new_category)
    current_categories <- categories()
    if (!input$new_category %in% names(current_categories)) {
      # Generate a more vibrant random color for this category
      # Use HSV color space for more control over vibrancy
      hue <- sample(0:359, 1)  # Random hue
      saturation <- runif(1, 0.7, 0.9)  # High saturation for vibrancy
      value <- runif(1, 0.8, 0.95)  # Bright value
      
      # Convert HSV to RGB then to hex
      color_rgb <- hsv(hue/360, saturation, value)
      
      current_categories[[input$new_category]] <- list(
        ranges = data.frame(
          start = numeric(),
          end = numeric()
        ),
        color = color_rgb
      )
      categories(current_categories)
      
      # Update the selection input
      updateSelectInput(session, "selected_category",
                       choices = names(current_categories))
    }
  })
  
  # Store box selection
  observeEvent(event_data("plotly_selected"), {
    selection <- event_data("plotly_selected")
    if (!is.null(selection)) {
      range <- c(min(selection$x), max(selection$x))
      current_selection(range)
    }
  })
  
  # Add current selection to category
  observeEvent(input$add_selection, {
    req(input$selected_category, current_selection())
    
    current_categories <- categories()
    range <- current_selection()
    
    new_range <- data.frame(
      start = range[1],
      end = range[2]
    )
    
    current_categories[[input$selected_category]]$ranges <- rbind(
      current_categories[[input$selected_category]]$ranges,
      new_range
    )
    
    categories(current_categories)
    current_selection(NULL)  # Clear the current selection
  })
  
  # Clear the current selection
  observeEvent(input$clear_selection, {
    current_selection(NULL)
  })
  
  # Remove last range from current category
  observeEvent(input$remove_last, {
    req(input$selected_category)
    current_categories <- categories()
    cat_data <- current_categories[[input$selected_category]]$ranges
    
    if (nrow(cat_data) > 0) {
      current_categories[[input$selected_category]]$ranges <- cat_data[-nrow(cat_data), ]
      categories(current_categories)
    }
  })
  
  # Display ranges table for selected category
  output$range_table <- renderTable({
    req(input$selected_category)
    cat_data <- categories()[[input$selected_category]]$ranges
    if (nrow(cat_data) == 0) return(data.frame())
    
    data.frame(
      Range = seq_len(nrow(cat_data)),
      Start = round(cat_data$start, 2),
      End = round(cat_data$end, 2)
    )
  })
  
  # Create categorized datasets
  get_categorized_data <- reactive({
    req(data())
    full_data <- data()
    result_list <- list()
    
    # Check if we have a Matching_Excluded category
    have_exclusions <- "Matching_Excluded" %in% names(categories())
    exclusion_ranges <- NULL
    
    if (have_exclusions) {
      exclusion_ranges <- categories()[["Matching_Excluded"]]$ranges
    }
    
    for (cat_name in names(categories())) {
      # Skip the exclusion category itself
      if (cat_name == "Matching_Excluded") next
      
      cat_ranges <- categories()[[cat_name]]$ranges
      if (nrow(cat_ranges) > 0) {
        # Initialize an empty data frame for this category
        cat_data <- data.frame()
        
        # Combine all ranges for this category
        for (i in 1:nrow(cat_ranges)) {
          range_start <- cat_ranges$start[i]
          range_end <- cat_ranges$end[i]
          
          # Filter data within this range
          range_data <- full_data[full_data$elapsed >= range_start & 
                                 full_data$elapsed <= range_end, ]
          
          # If we have exclusions, remove excluded points
          if (!is.null(exclusion_ranges) && nrow(exclusion_ranges) > 0) {
            # For each exclusion range, remove points within it
            for (j in 1:nrow(exclusion_ranges)) {
              excl_start <- exclusion_ranges$start[j]
              excl_end <- exclusion_ranges$end[j]
              
              # Remove points in the exclusion range
              range_data <- range_data[!(range_data$elapsed >= excl_start & 
                                       range_data$elapsed <= excl_end), ]
            }
          }
          
          # Append to the category data
          cat_data <- rbind(cat_data, range_data)
        }
        
        # Add to result list if we have data
        if (nrow(cat_data) > 0) {
          # Add category column if option is selected
          if (!is.null(input$add_category_column) && input$add_category_column) {
            cat_col_name <- ifelse(!is.null(input$category_column) && nzchar(input$category_column),
                                 input$category_column, "fluxy_cat")
            cat_data[[cat_col_name]] <- cat_name
          }
          
          result_list[[cat_name]] <- cat_data
        }
      }
    }
    
    return(result_list)
  })
  
  # Create a consolidated dataset for export
  get_consolidated_data <- reactive({
    cat_data_list <- get_categorized_data()
    
    # If there's no data, return empty data frame
    if (length(cat_data_list) == 0) {
      return(data.frame())
    }
    
    # Combine all category data frames into one
    consolidated <- do.call(rbind, cat_data_list)
    
    # Make sure it's not empty
    if (nrow(consolidated) == 0) {
      return(data.frame())
    }
    
    # Sort by elapsed time
    if ("elapsed" %in% names(consolidated)) {
      consolidated <- consolidated[order(consolidated$elapsed), ]
    }
    
    return(consolidated)
  })
  
  # Export data based on selected format
  observeEvent(input$export_data, {
    req(input$export_format, input$export_type)
    
    # Choose data source based on export type
    if (input$export_type == "consolidated") {
      # Use consolidated data
      consolidated_data <- get_consolidated_data()
      
      if (nrow(consolidated_data) == 0) {
        output$export_info <- renderPrint({
          cat("No categorized data to export.\n")
        })
        return()
      }
      
      # Get category column info for messages
      cat_col_info <- ""
      if (!is.null(input$add_category_column) && input$add_category_column) {
        cat_col_name <- ifelse(!is.null(input$category_column) && nzchar(input$category_column),
                             input$category_column, "fluxy_cat")
        cat_col_info <- paste0(" with category column '", cat_col_name, "'")
      }
      
      # Export based on format
      if (input$export_format == "csv") {
        # Create directory if it doesn't exist
        dir_name <- "li6800_categories"
        if (!dir.exists(dir_name)) {
          dir.create(dir_name)
        }
        
        # Export as single CSV
        file_name <- file.path(dir_name, paste0(input$export_prefix, "consolidated.csv"))
        write.csv(consolidated_data, file = file_name, row.names = FALSE)
        
        output$export_info <- renderPrint({
          cat("Exported consolidated data as CSV file", cat_col_info, ".\n")
          cat("File: ", file_name, "\n", sep="")
          cat("Total rows: ", nrow(consolidated_data), "\n", sep="")
        })
      } else if (input$export_format == "rdata") {
        # Export as R data file
        file_name <- paste0(input$export_prefix, "consolidated.RData")
        save(consolidated_data, file = file_name)
        
        output$export_info <- renderPrint({
          cat("Exported consolidated data to", file_name, cat_col_info, "\n")
          cat("The data is stored as 'consolidated_data' with", nrow(consolidated_data), "rows.\n")
        })
      } else if (input$export_format == "env") {
        # Export to global environment
        object_name <- paste0(input$export_prefix, "consolidated")
        assign(object_name, consolidated_data, envir = .GlobalEnv)
        
        output$export_info <- renderPrint({
          cat("Exported consolidated data to R global environment", cat_col_info, ".\n")
          cat("Object: ", object_name, " (", nrow(consolidated_data), " rows)\n", sep="")
        })
      }
    } else {
      # Use separate categories (original approach)
      cat_data <- get_categorized_data()
      
      if (length(cat_data) == 0) {
        output$export_info <- renderPrint({
          cat("No categorized data to export.\n")
        })
        return()
      }
      
      # Get category column info for messages
      cat_col_info <- ""
      if (!is.null(input$add_category_column) && input$add_category_column) {
        cat_col_name <- ifelse(!is.null(input$category_column) && nzchar(input$category_column),
                            input$category_column, "fluxy_cat")
        cat_col_info <- paste0(" with category column '", cat_col_name, "'")
      }
      
      # Different export methods
      if (input$export_format == "csv") {
        # Create directory if it doesn't exist
        dir_name <- "li6800_categories"
        if (!dir.exists(dir_name)) {
          dir.create(dir_name)
        }
        
        # Export each category as a CSV file
        for (cat_name in names(cat_data)) {
          file_name <- file.path(dir_name, paste0(input$export_prefix, cat_name, ".csv"))
          write.csv(cat_data[[cat_name]], file = file_name, row.names = FALSE)
        }
        
        output$export_info <- renderPrint({
          cat("Exported", length(cat_data), "categories as CSV files to", dir_name, "folder", cat_col_info, ".\n")
          cat("Files:\n")
          for (cat_name in names(cat_data)) {
            cat("- ", input$export_prefix, cat_name, ".csv\n", sep = "")
          }
        })
      } else if (input$export_format == "rdata") {
        # Export all categories as a single RData file
        file_name <- paste0(input$export_prefix, "categories.RData")
        save(cat_data, file = file_name)
        
        output$export_info <- renderPrint({
          cat("Exported all categories to", file_name, cat_col_info, "\n")
          cat("The data is stored as a list named 'cat_data' with", length(cat_data), "categories.\n")
        })
      } else if (input$export_format == "env") {
        # Export to global environment
        for (cat_name in names(cat_data)) {
          object_name <- paste0(input$export_prefix, cat_name)
          assign(object_name, cat_data[[cat_name]], envir = .GlobalEnv)
        }
        
        output$export_info <- renderPrint({
          cat("Exported", length(cat_data), "categories to R global environment", cat_col_info, ".\n")
          cat("Objects:\n")
          for (cat_name in names(cat_data)) {
            cat("- ", input$export_prefix, cat_name, " (", nrow(cat_data[[cat_name]]), " rows)\n", sep = "")
          }
        })
      }
    }
  })
  
  # Store detected matching events
  matching_events <- reactiveVal(data.frame(
    start = numeric(),
    end = numeric(),
    magnitude = numeric(),
    variable = character(),
    stringsAsFactors = FALSE
  ))
  
  # Update matching variable choices based on available data
  observe({
    req(available_vars())
    vars <- available_vars()
    
    # Look for specific matching-related variables with case-insensitive search
    matching_related <- c()
    
    # Check for CO2 variables
    co2_vars <- grep("co2.*[rs]|co2_(?:ref|sam)|co2_(?:reference|sample)", 
                   vars, ignore.case = TRUE, value = TRUE)
    if (length(co2_vars) > 0) matching_related <- c(matching_related, co2_vars)
    
    # Check for H2O variables
    h2o_vars <- grep("h2o.*[rs]|h2o_(?:ref|sam)|h2o_(?:reference|sample)", 
                   vars, ignore.case = TRUE, value = TRUE)
    if (length(h2o_vars) > 0) matching_related <- c(matching_related, h2o_vars)
    
    # Add A (photosynthesis) as it shows matching effects
    a_vars <- grep("^a$", vars, ignore.case = TRUE, value = TRUE)
    if (length(a_vars) > 0) matching_related <- c(matching_related, a_vars)
    
    # Update checkbox choices if we found any matches
    if (length(matching_related) > 0) {
      updateCheckboxGroupInput(session, "matching_vars", 
                            choices = matching_related,
                            selected = matching_related[1:min(2, length(matching_related))])
    }
  })
  
  # Detect matching events
  observeEvent(input$detect_matching, {
    req(data())
    
    # Get full dataset
    dt <- data()
    
    # Initialize results dataframe
    all_events <- data.frame(
      start = numeric(),
      end = numeric(),
      magnitude = numeric(),
      variable = character(),
      stringsAsFactors = FALSE
    )
    
    # Choose detection method based on user input
    if (input$matching_method == "gap") {
      # GAP DETECTION METHOD
      
      # Make sure we have elapsed time
      if (!("elapsed" %in% names(dt))) {
        showNotification("No elapsed time column found", type = "error")
        return()
      }
      
      # Sort data by elapsed time and remove rows with NA elapsed times
      dt <- dt[!is.na(dt$elapsed), ]
      dt <- dt[order(dt$elapsed), ]
      
      # Check if we have enough data after removing NAs
      if (nrow(dt) < 2) {
        showNotification("Not enough valid data points for gap detection", type = "error")
        return()
      }
      
      # Calculate time differences between consecutive measurements
      time_diffs <- diff(dt$elapsed)
      
      # Remove any NA values from time differences
      valid_diffs <- !is.na(time_diffs) & is.finite(time_diffs)
      
      if (sum(valid_diffs) == 0) {
        showNotification("No valid time differences found", type = "warning")
        all_events <- data.frame(
          start = numeric(),
          end = numeric(),
          magnitude = numeric(),
          variable = character(),
          stringsAsFactors = FALSE
        )
      } else {
        # Find gaps that exceed the threshold (only among valid differences)
        gap_threshold <- max(5, input$gap_threshold) # Minimum 5 seconds
        gap_indices <- which(valid_diffs & time_diffs > gap_threshold)
        
        # If no gaps found, inform the user
        if (length(gap_indices) == 0) {
          showNotification("No time gaps exceeding the threshold were found", type = "warning")
          all_events <- data.frame(
            start = numeric(),
            end = numeric(),
            magnitude = numeric(),
            variable = character(),
            stringsAsFactors = FALSE
          )
        } else {
          # For each gap, create an event
          buffer <- max(5, input$buffer_time) # Minimum 5 seconds buffer
          
          for (idx in gap_indices) {
            # Calculate start and end times for this event
            # The event is centered on the gap
            gap_start_time <- dt$elapsed[idx]
            gap_end_time <- dt$elapsed[idx + 1]
            
            # Apply buffer before and after the gap
            event_start <- gap_start_time - buffer
            event_end <- gap_end_time + buffer
            
            # Calculate magnitude (size of the gap)
            magnitude <- gap_end_time - gap_start_time
            
            # Add to results
            all_events <- rbind(all_events, data.frame(
              start = event_start,
              end = event_end,
              magnitude = magnitude,
              variable = "time_gap"
            ))
          }
          
          # Sort by start time
          all_events <- all_events[order(all_events$start), ]
        }
      }
    } else {
      # ORIGINAL SPIKE DETECTION METHOD
      req(input$matching_vars)
      
      # Process each selected variable
      for (var in input$matching_vars) {
        # Make sure variable exists in data
        if (!(var %in% names(dt))) {
          message("Variable not found: ", var)
          next
        }
        
        # Get variable data and time, removing NA values
        var_data <- dt[[var]]
        time_data <- dt$elapsed
        
        # Remove rows where either variable or time is NA
        valid_rows <- !is.na(var_data) & !is.na(time_data) & is.finite(var_data) & is.finite(time_data)
        
        if (sum(valid_rows) < 10) {
          message("Not enough valid data for variable: ", var)
          next
        }
        
        # Filter to valid data only
        var_data <- var_data[valid_rows]
        time_data <- time_data[valid_rows]
        
        # Sort by time (just to be safe)
        time_order <- order(time_data)
        var_data <- var_data[time_order]
        time_data <- time_data[time_order]
        
        # Calculate rate of change (derivative)
        data_diff <- diff(var_data)
        time_diff <- diff(time_data)
        
        # Avoid division by zero and filter out invalid differences
        valid_time_diff <- time_diff > 0 & is.finite(time_diff) & is.finite(data_diff)
        
        if (sum(valid_time_diff) < 5) {
          message("Not enough valid time differences for variable: ", var)
          next
        }
        
        # Calculate rate of change only for valid differences
        rate_of_change <- rep(NA, length(time_diff))
        rate_of_change[valid_time_diff] <- abs(data_diff[valid_time_diff] / time_diff[valid_time_diff])
        
        # Remove any remaining NA or infinite values
        valid_rates <- is.finite(rate_of_change) & !is.na(rate_of_change)
        
        if (sum(valid_rates) < 5) {
          message("Not enough valid rate calculations for variable: ", var)
          next
        }
        
        # Determine threshold based on sensitivity
        # Higher sensitivity = lower threshold = more events detected
        threshold_percentile <- 100 - (input$matching_sensitivity * 0.5)
        threshold <- quantile(rate_of_change[valid_rates], threshold_percentile/100, na.rm = TRUE)
        
        # Find points exceeding threshold (only among valid rates)
        spikes <- which(valid_rates & rate_of_change > threshold)
        
        # If no spikes found, continue to next variable
        if (length(spikes) == 0) {
          message("No matching events found in variable: ", var)
          next
        }
        
        # Group nearby spikes into events
        events <- list()
        current_event <- c(spikes[1])
        
        for (i in 2:length(spikes)) {
          # If this spike is close to the previous one, add it to current event
          if (time_data[spikes[i]] - time_data[spikes[i-1]] < 10) {  # Within 10 seconds
            current_event <- c(current_event, spikes[i])
          } else {
            # This is a new event, save the old one and start a new one
            events[[length(events) + 1]] <- current_event
            current_event <- c(spikes[i])
          }
        }
        # Add the last event
        events[[length(events) + 1]] <- current_event
        
        # Convert events to time ranges with buffer window
        for (event in events) {
          # Calculate event boundaries
          event_start <- time_data[min(event)] - input$matching_window
          event_end <- time_data[max(event)] + input$matching_window
          
          # Calculate magnitude (max rate of change during event)
          magnitude <- max(rate_of_change[event], na.rm = TRUE)
          
          # Add to results
          all_events <- rbind(all_events, data.frame(
            start = event_start,
            end = event_end,
            magnitude = magnitude,
            variable = var
          ))
        }
      }
      
      # Sort by start time
      if (nrow(all_events) > 0) {
        all_events <- all_events[order(all_events$start), ]
      }
    }
    
    # Store results
    matching_events(all_events)
    
    # Update info output
    output$matching_info <- renderPrint({
      if (nrow(all_events) == 0) {
        if (input$matching_method == "gap") {
          cat("No time gaps exceeding", input$gap_threshold, "seconds were detected.\n")
          cat("Try reducing the gap threshold or switching to value spike detection.")
        } else {
          cat("No matching events detected. Try adjusting the sensitivity or selecting different variables.")
        }
      } else {
        if (input$matching_method == "gap") {
          cat("Detected", nrow(all_events), "time gaps exceeding", input$gap_threshold, "seconds.\n")
          cat("These are visualized on the plots with semi-transparent red areas and red boundary lines.\n")
          cat("Use 'Create Exclusion Category' to permanently mark these areas for exclusion from analysis.")
        } else {
          cat("Detected", nrow(all_events), "potential matching events across", length(unique(all_events$variable)), "variables.\n")
          cat("These are visualized on the plots with semi-transparent red areas and red boundary lines.\n")
          cat("Use 'Create Exclusion Category' to permanently mark these areas for exclusion from analysis.")
        }
      }
    })
  })
  
  # Display matching events table
  output$matching_events <- renderTable({
    events <- matching_events()
    if (nrow(events) == 0) return(NULL)
    
    # Format for display
    if (all(events$variable == "time_gap")) {
      # For time gap method
      data.frame(
        Start = round(events$start, 1),
        End = round(events$end, 1),
        Duration = round(events$end - events$start, 1),
        "Gap Size (s)" = round(events$magnitude, 1)
      )
    } else {
      # For spike method
      data.frame(
        Start = round(events$start, 1),
        End = round(events$end, 1),
        Duration = round(events$end - events$start, 1),
        Variable = events$variable
      )
    }
  })
  
  # Create exclusion category from matching events
  observeEvent(input$exclude_matching, {
    req(matching_events())
    events <- matching_events()
    
    if (nrow(events) == 0) {
      showNotification("No matching events to exclude", type = "warning")
      return()
    }
    
    # Create a consolidated set of non-overlapping events
    # Sort events by start time
    events <- events[order(events$start), ]
    
    consolidated_events <- data.frame(
      start = numeric(),
      end = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Start with the first event
    current_start <- events$start[1]
    current_end <- events$end[1]
    
    # Process all events
    for (i in 2:nrow(events)) {
      # If this event overlaps with current range, extend the range
      if (events$start[i] <= current_end) {
        current_end <- max(current_end, events$end[i])
      } else {
        # This is a new non-overlapping event, save the previous one
        consolidated_events <- rbind(consolidated_events, data.frame(
          start = current_start,
          end = current_end
        ))
        
        # Start a new event
        current_start <- events$start[i]
        current_end <- events$end[i]
      }
    }
    
    # Add the last event
    consolidated_events <- rbind(consolidated_events, data.frame(
      start = current_start,
      end = current_end
    ))
    
    # Create or get the "Matching_Excluded" category
    current_categories <- categories()
    
    if (!("Matching_Excluded" %in% names(current_categories))) {
      # Create new category with a distinctive color
      current_categories[["Matching_Excluded"]] <- list(
        ranges = data.frame(
          start = numeric(),
          end = numeric()
        ),
        color = "#ff4f4f60"  # Bright red for exclusions
      )
    }
    
    # Replace any existing ranges with the consolidated ones
    current_categories[["Matching_Excluded"]]$ranges <- consolidated_events
    
    # Update categories
    categories(current_categories)
    
    # Clear matching_events to avoid double showing
    matching_events(data.frame(
      start = numeric(),
      end = numeric(),
      magnitude = numeric(),
      variable = character(),
      stringsAsFactors = FALSE
    ))
    
    # Update category dropdown
    updateSelectInput(session, "selected_category",
                    choices = names(current_categories),
                    selected = "Matching_Excluded")
    
    # Show notification
    showNotification(paste("Added", nrow(consolidated_events), "consolidated matching events to exclusion category"), 
                   type = "message")
  })
  
  # Clear matching detection
  observeEvent(input$clear_matching, {
    # Clear all detected matching events
    matching_events(data.frame(
      start = numeric(),
      end = numeric(),
      magnitude = numeric(),
      variable = character(),
      stringsAsFactors = FALSE
    ))
    
    # Clear the info output
    output$matching_info <- renderPrint({
      cat("Matching detection cleared. You can run a new detection with different parameters.")
    })
    
    # Force a redraw of the plots by introducing a random change
    # that triggers reactivity but doesn't affect functionality
    session$userData$clear_timestamp <- Sys.time()
    
    # Show notification
    showNotification("Matching detection cleared", type = "message")
  })
  
  # Clear exclusion category
  observeEvent(input$clear_exclusion, {
    # Check if the Matching_Excluded category exists
    current_categories <- categories()
    
    if (!("Matching_Excluded" %in% names(current_categories))) {
      showNotification("No exclusion category found", type = "warning")
      return()
    }
    
    # Remove the category
    current_categories[["Matching_Excluded"]] <- NULL
    
    # Update categories
    categories(current_categories)
    
    # Force a redraw of the plots
    session$userData$clear_timestamp <- Sys.time()
    
    # Update dropdown if needed
    updateSelectInput(session, "selected_category",
                     choices = names(current_categories))
    
    # Show notification
    showNotification("Exclusion category removed", type = "message")
    
    # Update info output
    output$matching_info <- renderPrint({
      cat("Exclusion category has been removed. You can run a new detection or create a new exclusion category.")
    })
  })
  
  output$multiPlot <- renderPlotly({
    # Check if we have data loaded
    if (is.null(data()) || nrow(data()) == 0) {
      # Return an empty plot with a message when no data is loaded
      return(
        plot_ly() %>%
          add_annotations(
            text = "Upload a LI-6800 file to view data",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 16)
          ) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
      )
    }
    
    # Check for variable selection
    if (is.null(input$vars) || length(input$vars) == 0) {
      # Return an empty plot with a message when no variables are selected
      return(
        plot_ly() %>%
          add_annotations(
            text = "Select variables to plot in the Variables tab",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 16)
          ) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
      )
    }
    
    # Create a dependency on the clear timestamp to force redraw
    if (!is.null(session$userData$clear_timestamp)) {
      # Just reference it to create the dependency, but don't use it
      invisible(session$userData$clear_timestamp)
    }
    
    dt <- data()
    
    # Check that all selected vars are in the dataset
    vars_in_data <- input$vars[input$vars %in% names(dt)]
    if (length(vars_in_data) == 0) {
      # Return an empty plot with a message when no valid variables are found
      return(
        plot_ly() %>%
          add_annotations(
            text = "No valid variables selected. Please select variables present in the dataset.",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 16)
          ) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
      )
    }
    
    # Reshape to long format
    dt_long <- dt[, c("elapsed", vars_in_data), with = FALSE] |>
      pivot_longer(cols = -elapsed, names_to = "variable", values_to = "value")
    
    # Generate a color palette based on number of variables
    num_vars <- length(vars_in_data)
    
    # Use viridis palette for good contrast
    col_palette <- viridis(num_vars, option = "turbo")
    names(col_palette) <- vars_in_data
    
    # Create a completely fresh approach - use plotly directly instead of ggplot2
    plots <- list()
    
    for (varname in vars_in_data) {
      subdata <- dt_long[dt_long$variable == varname, ]
      ylims <- get_y_range(varname)
      
      # Create a simple scatter plot with plotly
      p <- plot_ly(
        data = subdata,
        x = ~elapsed,
        y = ~value,
        type = 'scatter',
        mode = 'markers',
        marker = list(
          color = col_palette[varname],
          size = 3
        ),
        name = varname
      ) %>%
        layout(
          title = varname,
          xaxis = list(
            title = "Time (seconds)",
            showline = TRUE,
            linecolor = "#cccccc",
            linewidth = 2,
            mirror = TRUE
          ),
          yaxis = list(
            title = varname,
            range = ylims,
            showline = TRUE,
            linecolor = "#cccccc",
            linewidth = 2,
            mirror = TRUE
          ),
          showlegend = FALSE,
          plot_bgcolor = 'white',
          paper_bgcolor = 'white',
          margin = list(l=80, r=80, t=50, b=50)
        )
      
      # Add shapes directly to this plot
      shapes_list <- list()
      
      # Add border rectangle as the first shape
      shapes_list[[1]] <- list(
        type = "rect",
        xref = "paper",
        yref = "paper",
        x0 = 0,
        y0 = 0,
        x1 = 1,
        y1 = 1,
        line = list(
          color = "#cccccc",
          width = 2
        ),
        fillcolor = "transparent"
      )
      
      # Add category ranges
      for (cat_name in names(categories())) {
        cat_ranges <- categories()[[cat_name]]$ranges
        if (nrow(cat_ranges) > 0) {
          for (i in 1:nrow(cat_ranges)) {
            # Add the base rectangle
            shapes_list[[length(shapes_list) + 1]] <- list(
              type = "rect",
              x0 = cat_ranges$start[i],
              x1 = cat_ranges$end[i],
              y0 = ylims[1],
              y1 = ylims[2],
              fillcolor = categories()[[cat_name]]$color,
              opacity = 0.5,
              line = list(width = 0)
            )
            
            # Add stripes for Matching_Excluded category
            if (cat_name == "Matching_Excluded") {
              stripe_spacing <- (ylims[2] - ylims[1]) / 30  # Adjust density of stripes
              for (j in seq(ylims[1], ylims[2], by = stripe_spacing)) {
                shapes_list[[length(shapes_list) + 1]] <- list(
                  type = "line",
                  x0 = cat_ranges$start[i],
                  x1 = cat_ranges$end[i],
                  y0 = j,
                  y1 = j + stripe_spacing,
                  line = list(
                    color = '#ff4f4fa0',
                    width = 2,
                    dash = 'solid',
                    opacity = 0.1
                  )
                )
              }
            }
          }
        }
      }
      
      # Add detected matching events (if any)
      events <- matching_events()
      if (nrow(events) > 0) {
        # Create a set of non-overlapping events for cleaner visualization
        combined_events <- data.frame(
          start = numeric(),
          end = numeric(),
          stringsAsFactors = FALSE
        )
        
        # Sort events by start time
        events <- events[order(events$start), ]
        
        # Combine overlapping events
        current_start <- events$start[1]
        current_end <- events$end[1]
        
        for (i in 2:nrow(events)) {
          # If this event overlaps with current range, extend the range
          if (events$start[i] <= current_end) {
            current_end <- max(current_end, events$end[i])
          } else {
            # This is a new non-overlapping event
            combined_events <- rbind(combined_events, data.frame(
              start = current_start,
              end = current_end
            ))
            
            current_start <- events$start[i]
            current_end <- events$end[i]
          }
        }
        
        # Add the last event
        combined_events <- rbind(combined_events, data.frame(
          start = current_start,
          end = current_end
        ))
        
        # Add each combined event as a red semi-transparent rectangle with red borders
        for (i in 1:nrow(combined_events)) {
          # Add the fill rectangle
          shapes_list[[length(shapes_list) + 1]] <- list(
            type = "rect",
            x0 = combined_events$start[i],
            x1 = combined_events$end[i],
            y0 = ylims[1],
            y1 = ylims[2],
            fillcolor = "#ff4f4f",
            opacity = 0.15,
            line = list(width = 0)
          )
          
          # Add left boundary line
          shapes_list[[length(shapes_list) + 1]] <- list(
            type = "line",
            x0 = combined_events$start[i],
            x1 = combined_events$start[i],
            y0 = ylims[1],
            y1 = ylims[2],
            line = list(
              color = 'red',
              width = 2,
              dash = 'solid'
            )
          )
          
          # Add right boundary line
          shapes_list[[length(shapes_list) + 1]] <- list(
            type = "line",
            x0 = combined_events$end[i],
            x1 = combined_events$end[i],
            y0 = ylims[1],
            y1 = ylims[2],
            line = list(
              color = 'red',
              width = 2,
              dash = 'solid'
            )
          )
        }
      }
      
      # Add current selection
      if (!is.null(current_selection())) {
        range <- current_selection()
        
        # Add the rectangle for current selection
        shapes_list[[length(shapes_list) + 1]] <- list(
          type = "rect",
          x0 = range[1],
          x1 = range[2],
          y0 = ylims[1],
          y1 = ylims[2],
          fillcolor = "#b9c7cc",
          opacity = 0.3,
          line = list(width = 0)
        )
        
        # Add left boundary line
        shapes_list[[length(shapes_list) + 1]] <- list(
          type = "line",
          x0 = range[1],
          x1 = range[1],
          y0 = ylims[1],
          y1 = ylims[2],
          line = list(
            color = 'cyan',
            width = 2,
            dash = 'solid'
          )
        )
        
        # Add right boundary line
        shapes_list[[length(shapes_list) + 1]] <- list(
          type = "line",
          x0 = range[2],
          x1 = range[2],
          y0 = ylims[1],
          y1 = ylims[2],
          line = list(
            color = 'cyan',
            width = 2,
            dash = 'solid'
          )
        )
      }
      
      # Apply shapes to the plot
      if (length(shapes_list) > 0) {
        p <- p %>% layout(shapes = shapes_list)
      }
      
      plots[[length(plots) + 1]] <- p
    }
    
    # Combine plots
    p <- subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE)
    
    # Add selection functionality
    p <- p %>% 
      layout(dragmode = "select") %>%
      config(modeBarButtonsToAdd = list("select2d")) 
    
    # Register the event - this must be done after all other operations
    p <- event_register(p, "plotly_selected")
    
    return(p)
  })
}

shinyApp(ui = ui, server = server)
