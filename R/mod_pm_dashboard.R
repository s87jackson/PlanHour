#' pm_dashboard UI Function
#'
#' @description A shiny Module for Project Manager dashboard with staff assignment matrix
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 



mod_pm_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Project Manager Dashboard"),
      
      # Project Information --------------
      fluidRow(
        column(12,
               wellPanel(
                 h3("Project Information"),
                 
                 # Project Name
                 fluidRow(class = "form-group-aligned",
                          column(3,
                                 tags$label("Project Name:", class = "control-label-aligned")
                          ),
                          column(9,
                                 div(class = "input-wrapper",
                                     selectizeInput(ns("project_name"), 
                                                    "", 
                                                    choices = NULL,
                                                    options = list(
                                                      create = TRUE,
                                                      placeholder = "Type new project name or select existing...",
                                                      maxOptions = 50,
                                                      searchField = c('value', 'text')
                                                    ),
                                                    width = "100%")
                                 )
                          )
                 ),
                 
                 # Project ID
                 fluidRow(class = "form-group-aligned",
                          column(3,
                                 tags$label("Project ID:", class = "control-label-aligned")
                          ),
                          column(9,
                                 div(class = "input-wrapper",
                                     fluidRow(
                                       column(10,
                                              textInput(ns("project_id"), "", value = "", width = "100%")
                                       ),
                                       column(2,
                                              actionButton(ns("lookup_project_id"), "", icon = icon("search"), 
                                                           class = "btn-info btn-sm",
                                                           style = "margin-top: 0px; height: 34px; padding: 6px 8px;",
                                                           title = "Search WeWorked Projects")
                                       )
                                     )
                                 )
                          )
                 ),
                 
                 # Client Name
                 fluidRow(class = "form-group-aligned",
                          column(3,
                                 tags$label("Client Name:", class = "control-label-aligned")
                          ),
                          column(9,
                                 div(class = "input-wrapper",
                                     textInput(ns("client_name"), "", value = "", width = "100%")
                                 )
                          )
                 ),
                 
                 # Project Manager
                 fluidRow(class = "form-group-aligned",
                          column(3,
                                 tags$label("Project Manager:", class = "control-label-aligned")
                          ),
                          column(9,
                                 div(class = "input-wrapper",
                                     selectInput(ns("project_manager"), "", 
                                                 choices = c("Select a project manager..." = "", WWusers$fullName),
                                                 selected = "",
                                                 width = "100%")
                                 )
                          )
                 ),                 
                 # Period of Performance (single dateRangeInput)
                 fluidRow(class = "form-group-aligned",
                          column(3,
                                 tags$label("Period of Performance:", class = "control-label-aligned")
                          ),
                          column(9,
                                 div(class = "input-wrapper",
                                     dateRangeInput(ns("period_of_performance"), "", 
                                                    start = Sys.Date(), 
                                                    end = Sys.Date() + 365, 
                                                    format = "mm/dd/yyyy",
                                                    separator = " to ",
                                                    width = "100%")
                                 )
                          )
                 ),
                 
                 # Total Dollar Value
                 fluidRow(class = "form-group-aligned",
                          column(3,
                                 tags$label("Total Dollar Value:", class = "control-label-aligned")
                          ),
                          column(9,
                                 div(class = "input-wrapper",
                                     numericInput(ns("total_dollar_value"), "", 
                                                  value = 0, min = 0, step = 1000, width = "100%")
                                 )
                          )
                 ),
                 
                 # Project Description
                 fluidRow(class = "form-group-aligned",
                          column(3,
                                 tags$label("Project Description:", class = "control-label-aligned")
                          ),
                          column(9,
                                 div(class = "input-wrapper",
                                     textAreaInput(ns("project_description"), "", 
                                                   value = "", width = "100%", height = "80px", 
                                                   placeholder = "Enter project description...")
                                 )
                          )
                 ),
                 
                 # Project Notes
                 fluidRow(class = "form-group-aligned",
                          column(3,
                                 tags$label("Project Notes:", class = "control-label-aligned")
                          ),
                          column(9,
                                 div(class = "input-wrapper",
                                     textAreaInput(ns("project_notes"), "", 
                                                   value = "", width = "100%", height = "80px", 
                                                   placeholder = "Note any significant changes in scope, staffing, contract value, period of performance, or management.")
                                 )
                          )
                 ),
                 
                 # WeWorked Integration Status ----
                 fluidRow(class = "form-group-aligned",
                          column(3,
                                 tags$label("WeWorked Status:", class = "control-label-aligned")
                          ),
                          column(9,
                                 div(class = "input-wrapper",
                                     div(id = ns("weworked_status"), 
                                         style = "padding: 8px; border-radius: 4px; background-color: #f8f9fa; border: 1px solid #dee2e6;",
                                         icon("clock"), " Not yet loaded")
                                 )
                          )
                 )
                 
               )
        )
      ),
      
      # Task Budget Matrix ---------------
      fluidRow(
        column(12,
               wellPanel(
                 h3("Task Budget Matrix"),
                 p("Define tasks with their budget allocations. These tasks will be available for staff assignment below."),
                 
                 # Control buttons
                 fluidRow(
                   column(6,
                          actionButton(ns("add_task_budget"), "Add Task", 
                                       class = "btn-primary", icon = icon("plus"))
                   ),
                   column(6,
                          actionButton(ns("sort_tasks"), "Sort by Task Number", 
                                       class = "btn-info", icon = icon("sort-numeric-up"))
                   )
                 ),
                 
                 br(),
                 
                 # Task budget table
                 div(style = "width: 100%;",
                     rhandsontable::rHandsontableOutput(ns("task_budget_table"))
                 ),
                 
                 br()
               )
        )
      ),
      
      # Deliverables -----------
      fluidRow(
        column(12,
               wellPanel(
                 h3("Deliverables"),
                 p("Add deliverables and link them to tasks defined above."),
                 fluidRow(
                   column(4,
                          textInput(ns("deliverable_label"), "Deliverable Name:", value = "", width = "100%")
                   ),
                   column(4,
                          selectInput(ns("linked_task"), "Linked Task:", 
                                      choices = list("Select a task..." = ""),
                                      width = "100%")
                   ),
                   column(4,
                          dateInput(ns("deliverable_due_date"), "Due Date:", value = Sys.Date() + 30, width = "140px")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          actionButton(ns("add_deliverable"), "Add Deliverable", class = "btn-primary", icon = icon("plus")),
                          actionButton(ns("remove_deliverable"), "Remove Selected", class = "btn-warning", icon = icon("minus"))
                   )
                 ),
                 br(),
                 DT::DTOutput(ns("deliverables_table"))
               )
        )
      ),
      
      # Staffing Section --------------
      fluidRow(
        column(12,
               wellPanel(
                 h3("Staffing"),
                 
                 
                 
                 # Tabbed tables for Rates, Hours, and Staff
                 fluidRow(
                   column(12,
                          tabsetPanel(
                            id = ns("staffing_tabs"),
                            
                            tabPanel("Labor Categories",
                                     fluidRow(
                                       column(6,
                                              h4("Manage Labor Categories"),
                                              textInput(ns("new_laborCat_name"), "Labor Category:", 
                                                        placeholder = "e.g., Senior Analyst, Project Manager"),
                                              fluidRow(
                                                column(6,
                                                       actionButton(ns("add_laborCat"), "Add Labor Category", 
                                                                    class = "btn-primary btn-block",
                                                                    icon = icon("plus"))
                                                ),
                                                column(6,
                                                       actionButton(ns("remove_laborCat"), "Remove Selected", 
                                                                    class = "btn-warning btn-block", 
                                                                    icon = icon("minus"))
                                                )
                                              ),
                                       ),
                                       column(6,
                                              h4("Current Labor Categories"),
                                              DT::dataTableOutput(ns("laborCats_table"))
                                       )
                                     ),
                            ),
                            
                            tabPanel("Rates", 
                                     br(),
                                     p("Enter hourly rates for each labor-category-task combination:"),
                                     div(style = "width: 100%;",
                                         rhandsontable::rHandsontableOutput(ns("rates_table"))
                                     )
                            ),
                            tabPanel("Hours",
                                     br(), 
                                     p("Enter planned hours for each labor-category-task combination:"),
                                     div(style = "width: 100%;",
                                         rhandsontable::rHandsontableOutput(ns("hours_table"))
                                     )
                            ),
                            tabPanel("Staff",
                                     br(),
                                     p("Assign staff members to each labor category:"),
                                     div(id = ns("staff_assignments_container"),
                                         uiOutput(ns("dynamic_staff_assignments"))
                                     )
                            ),
                            tabPanel("Cost Check",
                                     br(),
                                     p("Compare task budgets with planned labor costs based on provided rates and hours:"),
                                     div(style = "width: 100%; margin-top: 15px;",
                                         DT::DTOutput(ns("cost_check_table"))
                                     ),
                                     p("Note: plans within 2% of task budgets are considered on track."),
                                     
                            )     
                          )
                   )
                 ),
                 
                 hr(),
                 fluidRow(
                   column(6,
                          radioButtons(ns("rates_hours_vary_by"), "Rates and Hours vary by:",
                                       choices = list("Task" = "Task", 
                                                      "Year" = "Year", 
                                                      "Both" = "Both"),
                                       selected = "Task",
                                       inline = TRUE)
                   ),
                   column(6,
                          conditionalPanel(
                            condition = paste0("input['", ns("rates_hours_vary_by"), "'] == 'Year' || input['", ns("rates_hours_vary_by"), "'] == 'Both'"),
                            dateInput(ns("year_2_starts"), "Year 2 starts:", 
                                      value = Sys.Date() + 365, 
                                      width = "200px")
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(6,
                          numericInput(ns("overhead_multiplier"), "Overhead Multiplier:", 
                                       value = 1.6, min = 1, max = 5, step = 0.1)
                   )
                 )
               )
        )
      ),
      
      # Gantt Chart --------------
      fluidRow(
        column(12,
               wellPanel(
                 h3("Project Gantt Chart"),
                 p("Interactive timeline showing all tasks and deliverables. Hover over items for details."),
                 div(style = "background: white; border: 1px solid #ddd; border-radius: 4px; padding: 15px; margin: 10px 0;",
                     ggiraph::girafeOutput(ns("gantt_chart"), height = "500px")
                 )
               )
        )
      ),
      
      # Spending --------------
      fluidRow(
        column(12,
               wellPanel(
                 h3("Project Spending Analytics"),
                 p("Compare planned vs actual spending by task. Click on spending bars to see detailed breakdowns."),
                 
                 # Status indicators
                 fluidRow(
                   column(6,
                          div(id = ns("spending_status"), 
                              style = "padding: 8px; border-radius: 4px; background-color: #f8f9fa; border: 1px solid #dee2e6;",
                              icon("calculator"), " Calculating spending data...")
                   ),
                   column(6,
                          div(id = ns("data_sync_status"), 
                              style = "padding: 8px; border-radius: 4px; background-color: #f8f9fa; border: 1px solid #dee2e6;",
                              icon("sync"), " Syncing with WeWorked...")
                   )
                 ),
                 
                 br(),
                 
                 # Graph 1: Task Budget vs Actual Spending (Always Visible)
                 div(style = "background: white; border: 1px solid #ddd; border-radius: 4px; padding: 15px; margin: 10px 0;",
                     h4("Task Budget vs Actual Spending"),
                     plotlyOutput(ns("task_spending_chart"), height = "400px")
                 )
               )
        )
      )
    ),
    
    
    # WeWorked Project Lookup Modal ----
    bsModal(id = ns("project_lookup_modal"), 
            title = "Find your project ID", 
            trigger = ns("lookup_project_id"),
            size = "medium",
            div(
              #p("Search and select a project from WeWorked:"),
              #br(),
              fluidRow(
                column(12,
                       selectizeInput(ns("weworked_project_search"), 
                                      label = NULL,
                                      choices = NULL,
                                      width = "100%",
                                      options = list(
                                        placeholder = "Type to search projects...",
                                        maxOptions = 100,
                                        create = FALSE
                                      ))
                )
              ),
              fluidRow(
                column(12,
                       div(#style = "margin-top: 25px;",
                           #strong("Project ID:"),
                           #br(),
                           span(id = ns("modal_project_id"), 
                                style = "font-family: monospace; font-size: 16px; color: #2c3e50; background-color: #f8f9fa; padding: 4px 8px; border-radius: 3px; border: 1px solid #dee2e6;",
                                "...")
                       )
                )
              ),
              br(),
              fluidRow(
                column(12,
                       actionButton(ns("use_selected_project"), "Use This Project ID", 
                                    class = "btn-success", icon = icon("check"),
                                    disabled = TRUE)
                )
              )
            )
    ),
    # Save changes ----
    tags$script("
  Shiny.addCustomMessageHandler('triggerSaveProject', function(message) {
    document.getElementById('pm_dashboard-save_project').click();
  });
"),
    tags$div(style = "display: none;",
             actionButton(ns("save_project"), "Hidden Save", class = "btn-success")
    )
    
  )
}

#' pm_dashboard Server Functions
#'
#' @noRd 

mod_pm_dashboard_server <- function(id, clear_trigger = reactive(NULL)){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Staff list ----
    staff_options <- WWusers$fullName 
    
    # Reactive Values ----
    
    ## Project list ----
    project_list <- reactiveVal(data.frame())
    weworked_projects_data <- reactiveVal(NULL)
    
    
    ## Staffing ----
    laborCats_data <- reactiveVal()
    rates_data <- reactiveVal()  
    hours_data <- reactiveVal()
    laborCat_staff_data <- reactiveVal()
    loading_staff_assignments <- reactiveVal(FALSE)
    staff_ui_trigger <- reactiveVal(0)
    
    
    ## Year boundaries based ----
    year_boundaries <- reactive({
      req(input$period_of_performance, input$year_2_starts)
      
      project_start <- input$period_of_performance[1]
      project_end <- input$period_of_performance[2]
      year_2_start <- input$year_2_starts
      
      # Calculate how many years we need
      years <- list()
      current_year_start <- project_start
      year_num <- 1
      
      while (current_year_start <= project_end) {
        if (year_num == 1) {
          year_end <- year_2_start - 1
        } else {
          # Each subsequent year starts on same month/day as year_2_starts
          year_end <- as.Date(paste0(as.numeric(format(current_year_start, "%Y")) + 1, 
                                     format(year_2_start, "-%m-%d"))) - 1
        }
        
        # Don't extend beyond project end
        year_end <- min(year_end, project_end)
        
        years[[paste0("Year_", year_num)]] <- list(
          start = current_year_start,
          end = year_end
        )
        
        year_num <- year_num + 1
        current_year_start <- year_end + 1
        
        # Safety check to prevent infinite loop
        if (year_num > 10) break
      }
      
      return(years)
    })
    
    ## Cost Check calculation ----
    cost_check_data <- reactive({
      req(task_budget_data(), laborCat_rates_data(), laborCat_hours_data(), input$overhead_multiplier)
      
      tasks <- task_budget_data()
      rates_data_db <- laborCat_rates_data()
      hours_data_db <- laborCat_hours_data()
      overhead <- input$overhead_multiplier
      vary_by <- input$rates_hours_vary_by
      
      # DIAGNOSTIC CODE - ADD THIS BLOCK
      cat("=== FULL DIAGNOSTIC ===\n")
      cat("Vary by mode:", vary_by, "\n")
      cat("Total rates in database reactive:", nrow(rates_data_db), "\n")
      cat("Total hours in database reactive:", nrow(hours_data_db), "\n")
      
      if (nrow(rates_data_db) > 0) {
        cat("ALL RATES DATA:\n")
        for (i in 1:min(10, nrow(rates_data_db))) {
          row <- rates_data_db[i, ]
          cat("  Row", i, ":", row$labor_cat_name, "| task_number:", paste0("'", row$task_number, "'"), 
              "| year_number:", paste0("'", row$year_number, "'"), "| rate:", row$hourly_rate, "\n")
        }
        cat("Unique task_numbers in rates:", paste(unique(rates_data_db$task_number), collapse = ", "), "\n")
        cat("Unique year_numbers in rates:", paste(unique(rates_data_db$year_number), collapse = ", "), "\n")
      }
      
      if (nrow(hours_data_db) > 0) {
        cat("ALL HOURS DATA:\n")
        for (i in 1:min(10, nrow(hours_data_db))) {
          row <- hours_data_db[i, ]
          cat("  Row", i, ":", row$labor_cat_name, "| task_number:", paste0("'", row$task_number, "'"), 
              "| year_number:", paste0("'", row$year_number, "'"), "| hours:", row$planned_hours, "\n")
        }
        cat("Unique task_numbers in hours:", paste(unique(hours_data_db$task_number), collapse = ", "), "\n")
        cat("Unique year_numbers in hours:", paste(unique(hours_data_db$year_number), collapse = ", "), "\n")
      }
      
      # cat("TASKS WE'RE LOOKING FOR:\n")
      # valid_tasks <- tasks[!is.na(tasks$Task_Name) & tasks$Task_Name != "", ]
      # for (i in 1:nrow(valid_tasks)) {
      #   cat("  Task", i, "number:", paste0("'", valid_tasks$Task_Number[i], "'"), "\n")
      # }
      
      
      cat("=== COST CHECK INITIAL DEBUG ===\n")
      cat("Total rates in DB:", nrow(rates_data_db), "\n")
      cat("Total hours in DB:", nrow(hours_data_db), "\n")
      cat("Vary by mode:", vary_by, "\n")
      
      if (nrow(rates_data_db) > 0) {
        cat("Sample rate:", rates_data_db$labor_cat_name[1], "Task:", rates_data_db$task_number[1], "Year:", rates_data_db$year_number[1], "\n")
      }
      if (nrow(hours_data_db) > 0) {
        cat("Sample hours:", hours_data_db$labor_cat_name[1], "Task:", hours_data_db$task_number[1], "Year:", hours_data_db$year_number[1], "\n")
      }
      
      if (nrow(tasks) == 0) {
        return(data.frame())
      }
      
      # Get valid tasks and sort by task number
      valid_tasks <- tasks[!is.na(tasks$Task_Name) & tasks$Task_Name != "", ]
      if (nrow(valid_tasks) == 0) {
        return(data.frame())
      }
      
      valid_tasks <- valid_tasks[order(as.numeric(valid_tasks$Task_Number)), ]
      
      # Create separate vectors for each row to maintain data types
      budget_row <- numeric(0)
      calculated_row <- numeric(0)
      variance_row <- numeric(0)
      variance_pct_row <- numeric(0)
      status_row <- character(0)
      
      # Track totals
      total_budget <- 0
      total_calculated <- 0
      
      # Process each task
      for (i in 1:nrow(valid_tasks)) {
        task_num <- valid_tasks$Task_Number[i]
        task_budget <- as.numeric(valid_tasks$Toxcel_Labor_Budget[i])
        
        # Calculate total cost for this task across all labor categories and years
        task_calculated_cost <- 0
        
        if (nrow(rates_data_db) > 0 && nrow(hours_data_db) > 0) {
          # Filter rates and hours for this task (considering variation type)
          if (vary_by == "Task") {
            task_rates <- rates_data_db[rates_data_db$task_number == task_num, ]
            task_hours <- hours_data_db[hours_data_db$task_number == task_num, ]
          } else if (vary_by == "Year") {
            # For Year mode, use default task "1" and sum across all years
            task_rates <- rates_data_db[rates_data_db$task_number == "1", ]
            task_hours <- hours_data_db[hours_data_db$task_number == "1", ]
          } else if (vary_by == "Both") {
            # For Both mode, filter by actual task number and include all years
            task_rates <- rates_data_db[rates_data_db$task_number == task_num, ]
            task_hours <- hours_data_db[hours_data_db$task_number == task_num, ]
          }
          
          cat("=== COST CHECK DEBUG ===\n")
          cat("Task:", task_num, "Vary by:", vary_by, "\n")
          cat("Task rates found:", nrow(task_rates), "\n")
          cat("Task hours found:", nrow(task_hours), "\n")
          
          if (nrow(task_rates) > 0 && nrow(task_hours) > 0) {
            # Debug: Show what we have before merging
            if (nrow(task_rates) > 0) {
              cat("Sample rate entries:\n")
              for (r in 1:min(3, nrow(task_rates))) {
                row <- task_rates[r, ]
                cat("  ", row$labor_cat_name, "Task:", row$task_number, "Year:", row$year_number, "Rate:", row$hourly_rate, "\n")
              }
            }
            if (nrow(task_hours) > 0) {
              cat("Sample hours entries:\n")
              for (h in 1:min(3, nrow(task_hours))) {
                row <- task_hours[h, ]
                cat("  ", row$labor_cat_name, "Task:", row$task_number, "Year:", row$year_number, "Hours:", row$planned_hours, "\n")
              }
            }
            
            # Ensure data types match for merge (convert to character to avoid type mismatches)
            task_rates$task_number <- as.character(task_rates$task_number)
            task_rates$year_number <- as.character(task_rates$year_number)
            task_hours$task_number <- as.character(task_hours$task_number) 
            task_hours$year_number <- as.character(task_hours$year_number)
            
            cat("DEBUG MERGE PREP: Task", task_num, "\n")
            cat("  Available rate combinations:\n")
            if (nrow(task_rates) > 0) {
              for (r in 1:nrow(task_rates)) {
                cat("    ", task_rates$labor_cat_name[r], task_rates$task_number[r], task_rates$year_number[r], "$", task_rates$hourly_rate[r], "\n")
              }
            }
            cat("  Available hour combinations:\n")
            if (nrow(task_hours) > 0) {
              for (h in 1:nrow(task_hours)) {
                cat("    ", task_hours$labor_cat_name[h], task_hours$task_number[h], task_hours$year_number[h], task_hours$planned_hours[h], "hrs\n")
              }
            }
            
            # Merge rates and hours by labor category, task number, and year number
            merged_data <- merge(task_rates, task_hours, 
                                 by = c("labor_cat_name", "task_number", "year_number"))
            
            cat("Merged data rows:", nrow(merged_data), "\n")
            
            if (nrow(merged_data) > 0) {
              cat("Successfully merged entries:\n")
              for (m in 1:nrow(merged_data)) {
                row <- merged_data[m, ]
                cat("  ", row$labor_cat_name, "Task:", row$task_number, "Year:", row$year_number, 
                    "Rate:", row$hourly_rate, "Hours:", row$planned_hours, "\n")
              }
              
              # Calculate loaded cost for each labor category-year combination
              merged_data$loaded_rate <- as.numeric(merged_data$hourly_rate) * as.numeric(overhead)
              merged_data$total_cost <- merged_data$loaded_rate * as.numeric(merged_data$planned_hours)
              
              cat("Cost calculations:\n")
              for (m in 1:nrow(merged_data)) {
                row <- merged_data[m, ]
                cat("  ", row$labor_cat_name, row$year_number, ": $", row$hourly_rate, " * ", overhead, " * ", row$planned_hours, " = $", row$total_cost, "\n")
              }
              
              # Sum all costs for this task across all labor categories and years
              task_calculated_cost <- sum(merged_data$total_cost, na.rm = TRUE)
              
              cat("Task", task_num, "TOTAL calculated cost:", task_calculated_cost, "\n")
              
            } else {
              cat("ERROR: No merged data for task", task_num, "\n")
              cat("Available rate labor_cats:", paste(unique(task_rates$labor_cat_name), collapse = ", "), "\n")
              cat("Available hours labor_cats:", paste(unique(task_hours$labor_cat_name), collapse = ", "), "\n")
              cat("Rate task_numbers:", paste(unique(task_rates$task_number), collapse = ", "), "\n")
              cat("Hours task_numbers:", paste(unique(task_hours$task_number), collapse = ", "), "\n")
              cat("Rate year_numbers:", paste(unique(task_rates$year_number), collapse = ", "), "\n")
              cat("Hours year_numbers:", paste(unique(task_hours$year_number), collapse = ", "), "\n")
            }
          } else {
            cat("No rates or hours data found for task", task_num, "\n")
          }
        }
        
        # Add to totals
        total_budget <- total_budget + task_budget
        total_calculated <- total_calculated + task_calculated_cost
        
        # Calculate variance and status
        variance <- task_calculated_cost - task_budget
        variance_pct <- ifelse(task_budget > 0, (variance / task_budget) * 100, 0)
        
        status <- if (abs(variance_pct) < 2) {
          "On Track"
        } else if (variance_pct > 0) {
          "Over Budget"
        } else {
          "Under Budget"
        }
        
        # Append to vectors
        budget_row <- c(budget_row, task_budget)
        calculated_row <- c(calculated_row, task_calculated_cost)
        variance_row <- c(variance_row, variance)
        variance_pct_row <- c(variance_pct_row, variance_pct)
        status_row <- c(status_row, status)
      }
      
      # Add Total column
      total_variance <- total_calculated - total_budget
      total_variance_pct <- ifelse(total_budget > 0, (total_variance / total_budget) * 100, 0)
      
      total_status <- if (abs(total_variance_pct) < 5) {
        "On Track"
      } else if (total_variance_pct > 0) {
        "Over Budget"
      } else {
        "Under Budget"
      }
      
      # Add totals to vectors
      budget_row <- c(budget_row, total_budget)
      calculated_row <- c(calculated_row, total_calculated)
      variance_row <- c(variance_row, total_variance)
      variance_pct_row <- c(variance_pct_row, total_variance_pct)
      status_row <- c(status_row, total_status)
      
      # Create column names
      task_cols <- paste0("Task_", valid_tasks$Task_Number)
      col_names <- c(task_cols, "Total")
      
      # Build the data frame with proper data types
      cost_check_df <- data.frame(
        `Cost Analysis` = c("Budget", "Planned", "Variance", "Variance %", "Status"),
        stringsAsFactors = FALSE
      )
      
      # Add numeric columns
      for (i in 1:length(col_names)) {
        cost_check_df[[col_names[i]]] <- c(
          budget_row[i],        # numeric
          calculated_row[i],    # numeric
          variance_row[i],      # numeric
          variance_pct_row[i],  # numeric
          status_row[i]         # character
        )
      }
      
      return(cost_check_df)
    })    
    
    ## Misc ----
    currently_loading <- reactiveVal(FALSE)
    start_date <- reactive({ input$period_of_performance[1] })
    end_date <- reactive({ input$period_of_performance[2] })
    
    # Initialize labor_categories data
    observe({
      if (is.null(laborCats_data())) {
        initial_labor_categories <- data.frame(
          LaborCategory = character(0),
          stringsAsFactors = FALSE
        )
        laborCats_data(initial_labor_categories)
      }
    })
    
    # Define reactive values for labor-category-based data that will be returned
    laborCat_rates_data <- reactiveVal(data.frame(
      labor_cat_name = character(0),
      task_number = character(0), 
      hourly_rate = numeric(0),
      stringsAsFactors = FALSE
    ))
    
    laborCat_hours_data <- reactiveVal(data.frame(
      labor_cat_name = character(0),
      task_number = character(0),
      planned_hours = numeric(0),
      stringsAsFactors = FALSE
    ))
    
    
    # Staff_matrix_data ----
    # Initialize staff_matrix_data as reactiveVal
    staff_matrix_data <- reactiveVal(data.frame(
      Staff_Name = character(0),
      LaborCategory = character(0),
      Direct_Rate = numeric(0),
      stringsAsFactors = FALSE
    ))
    
    # Create a separate reactive to compute staff matrix from labor category data
    computed_staff_matrix <- reactive({
      req(laborCat_rates_data(), laborCat_hours_data(), laborCat_staff_data(), task_budget_data())
      
      rates <- laborCat_rates_data()
      hours <- laborCat_hours_data() 
      staff_raw <- laborCat_staff_data()
      tasks <- task_budget_data()
      
      # Convert laborCat_staff_data format to expected format
      staff <- data.frame(
        labor_cat_name = staff_raw$LaborCategory,
        staff_names = staff_raw$Staff_Members,
        stringsAsFactors = FALSE
      )
      
      if (nrow(rates) == 0 || nrow(hours) == 0 || nrow(staff_raw) == 0) {
        return(data.frame(
          Staff_Name = character(0),
          LaborCategory = character(0),
          Direct_Rate = numeric(0),
          stringsAsFactors = FALSE
        ))
      }
      
      # Convert laborCat_staff_data format to expected format
      staff <- data.frame(
        labor_cat_name = staff_raw$LaborCategory,
        staff_names = staff_raw$Staff_Members,
        stringsAsFactors = FALSE
      )
      
      if (nrow(rates) == 0 || nrow(hours) == 0 || nrow(staff_raw) == 0) {        return(data.frame(
          Staff_Name = character(0),
          LaborCategory = character(0),
          Direct_Rate = numeric(0),
          stringsAsFactors = FALSE
        ))
      }
      
      # Build staff matrix from labor-category-based data
      staff_matrix <- data.frame()
      
      for (i in 1:nrow(staff)) {
        labor_cat_name <- staff$labor_cat_name[i]
        staff_names <- strsplit(as.character(staff$staff_names[i]), ",")[[1]]
        staff_names <- trimws(staff_names[staff_names != ""])
        
        if (length(staff_names) > 0) {
          for (staff_name in staff_names) {
            # Get average rate for this labor category across all tasks
            labor_cat_rates <- rates[rates$labor_cat_name == labor_cat_name, ]
            avg_rate <- if(nrow(labor_cat_rates) > 0) mean(labor_cat_rates$hourly_rate, na.rm = TRUE) else 0
            
            staff_row <- data.frame(
              Staff_Name = staff_name,
              LaborCategory = labor_cat_name,
              Direct_Rate = avg_rate,
              stringsAsFactors = FALSE
            )
            
            # Add task hour columns
            if (nrow(tasks) > 0) {
              for (j in 1:nrow(tasks)) {
                task_num <- tasks$Task_Number[j]
                col_name <- paste0("Hours_Task_", task_num)
                
                # Get planned hours for this labor-category-task combination
                task_hours <- hours[hours$labor_cat_name == labor_cat_name & hours$task_number == task_num, ]
                planned_hrs <- if(nrow(task_hours) > 0) task_hours$planned_hours[1] else 0
                
                staff_row[[col_name]] <- planned_hrs
              }
            }
            
            staff_matrix <- rbind(staff_matrix, staff_row)
          }
        }
      }
      
      return(staff_matrix)
    })
    
    
    # Observer to update staff_matrix_data when labor category data changes
    observe({
      computed_matrix <- computed_staff_matrix()
      if (!is.null(computed_matrix) && nrow(computed_matrix) > 0) {
        staff_matrix_data(computed_matrix)
      }
    })
    
    
    # Reactive value to store WeWorked data
    weworked_data <- reactiveVal(data.frame())
    
    # Project Spending reactive values
    spending_data <- reactiveVal(data.frame())
    task_spending_summary <- reactiveVal(data.frame())
    
    
    selected_task_for_modal <- reactiveVal(NULL)
    
    
    # Load available projects on module start ----
    observe({
      projects <- get_project_list()
      
      if (nrow(projects) > 0) {
        choices <- setNames(projects$project_name, projects$project_name)
      } else {
        choices <- character(0)
      }
      
      updateSelectizeInput(session, "project_name", 
                           selected = "",
                           choices = choices,
                           server = TRUE)
    })
    
    # Load WeWorked projects when modal opens ----
    observeEvent(input$lookup_project_id, {
      showNotification("Loading WeWorked projects...", type = "message", duration = 2)
      
      ww_projects <- get_all_projects()
      
      if (is.data.frame(ww_projects) && nrow(ww_projects) > 0) {
        # Store the data
        weworked_projects_data(ww_projects)
        
        # Create choices for selectizeInput (names = what users see/search, values = what gets selected)
        choices <- setNames(ww_projects$ProjectId, ww_projects$ProjectName)
        
        updateSelectizeInput(session, "weworked_project_search",
                             choices = choices,
                             selected = "")
        
        # Reset the project ID display
        runjs(paste0("$('#", ns("modal_project_id"), "').text('...');"))
        shinyjs::disable("use_selected_project")
        
        showNotification(paste("Loaded", length(choices), "WeWorked projects"), 
                         type = "message", duration = 3)
      } else {
        # Handle error case
        updateSelectizeInput(session, "weworked_project_search",
                             choices = list("Error loading projects" = ""),
                             selected = "")
        
        runjs(paste0("$('#", ns("modal_project_id"), "').text('Error loading projects');"))
        
        showNotification("Error loading projects from WeWorked", 
                         type = "error", duration = 5)
      }
    })
    
    # Handle project selection from dropdown
    observeEvent(input$weworked_project_search, {
      req(input$weworked_project_search, input$weworked_project_search != "")
      
      selected_project_id <- input$weworked_project_search
      
      # Update the Project ID display to the right
      runjs(paste0("$('#", ns("modal_project_id"), "').text('", selected_project_id, "');"))
      
      # Enable the use button
      shinyjs::enable("use_selected_project")
    })
    
    # Use selected project ID
    observeEvent(input$use_selected_project, {
      req(input$weworked_project_search, input$weworked_project_search != "")
      
      selected_project_id <- input$weworked_project_search
      
      # Update the project ID textInput directly
      updateTextInput(session, "project_id", value = selected_project_id)
      
      # Close modal
      toggleModal(session, "project_lookup_modal", toggle = "close")
      
      showNotification(paste0("Project ID '", selected_project_id, "' selected!"), 
                       type = "message", duration = 3)
    })    

    # Load selected project automatically when dropdown changes ----
    observeEvent(input$project_name, {
      req(input$project_name)
      
      if (input$project_name == "" || currently_loading()) {
        return()
      }
      
      # Check if this is an existing project
      projects <- get_project_list()
      
      if (nrow(projects) > 0 && input$project_name %in% projects$project_name) {
        # This is an existing project - load it
        currently_loading(TRUE)
        
        showNotification("Loading project...", type = "message", duration = 2)
        
        # Get project_id for the selected project_name  
        project_id <- projects$project_id[projects$project_name == input$project_name]
        
        result <- load_project_from_db(project_id)
        
        # DEBUG: Check what's actually in the result
        cat("=== DEBUGGING RESULT STRUCTURE ===\n")
        cat("result$success:", result$success, "\n")
        cat("result$labor_categories is null:", is.null(result$labor_categories), "\n")
        if (!is.null(result$labor_categories)) {
          cat("result$labor_categories nrow:", nrow(result$labor_categories), "\n")
          cat("result$labor_categories column names:", paste(names(result$labor_categories), collapse = ", "), "\n")
          cat("result$labor_categories content:\n")
          print(result$labor_categories)
        }
        
        if (result$success) {
          project <- result$project
          # Don't update project_name since user already selected it
          updateTextInput(session, "project_id", value = ifelse(is.na(project$project_id_text), "", project$project_id_text))
          updateTextInput(session, "client_name", value = project$client_name)
          updateTextInput(session, "project_manager", value = project$project_manager)
          updateTextAreaInput(session, "project_description", value = project$project_description)
          updateTextAreaInput(session, "project_notes", value = ifelse(is.na(project$project_notes), "", project$project_notes))
          updateDateRangeInput(session, "period_of_performance", 
                               start = as.Date(project$start_date),
                               end = as.Date(project$end_date))
          updateNumericInput(session, "total_dollar_value", value = project$total_dollar_value)
          updateNumericInput(session, "overhead_multiplier", value = project$overhead_multiplier)
          
          # Load task budgets
          if (!is.null(result$tasks) && nrow(result$tasks) > 0) {
            tasks_df <- data.frame(
              Task_Number = result$tasks$task_number,
              Task_Name = result$tasks$task_name,
              Start_Date = result$tasks$start_date,
              End_Date = result$tasks$end_date,
              Toxcel_Labor_Budget = result$tasks$labor_budget,
              stringsAsFactors = FALSE
            )
            task_budget_data(tasks_df)
          } else {
            tasks_df <- data.frame(
              Task_Number = "1",
              Task_Name = "",
              Start_Date = format(Sys.Date(), "%m/%d/%Y"),
              End_Date = format(Sys.Date() + 30, "%m/%d/%Y"),
              Toxcel_Labor_Budget = 0,
              stringsAsFactors = FALSE
            )
            task_budget_data(tasks_df)
          }
          
          # Load deliverables
          if (!is.null(result$deliverables) && nrow(result$deliverables) > 0) {
            deliverables_df <- data.frame(
              Deliverable_Name = result$deliverables$deliverable_name,
              Linked_Task = result$deliverables$linked_task,
              Due_Date = as.Date(result$deliverables$due_date),
              stringsAsFactors = FALSE
            )
            deliverables_data(deliverables_df)
          } else {
            deliverables_data(data.frame(
              Deliverable_Name = character(0),
              Linked_Task = character(0),
              Due_Date = as.Date(character(0)),
              stringsAsFactors = FALSE
            ))
          }
          
          # Load staff assignments
          if (!is.null(result$staff) && nrow(result$staff) > 0) {
            # Reconstruct the staff matrix
            staff_base <- data.frame(
              Staff_Name = result$staff$staff_name,
              Direct_Rate = result$staff$direct_rate,
              stringsAsFactors = FALSE
            )
            
            # Add task hour columns if there are task hours
            if (nrow(result$task_hours) > 0) {
              task_hours <- result$task_hours
              # Get unique task names for columns
              unique_tasks <- unique(task_hours$task_name)
              
              # Create task hour columns
              for (task_name in unique_tasks) {
                col_name <- paste0("Hours_", make.names(task_name))
                staff_base[[col_name]] <- 0
                
                # Fill in the hours for each staff member
                for (i in 1:nrow(staff_base)) {
                  staff_name <- staff_base$Staff_Name[i]
                  hours_row <- task_hours[task_hours$staff_name == staff_name & 
                                            task_hours$task_name == task_name, ]
                  if (nrow(hours_row) > 0) {
                    staff_base[i, col_name] <- hours_row$hours[1]
                  }
                }
              }
            }
            
            staff_matrix_data(staff_base)
          } else {
            # Reset to empty staff matrix if no staff
            initial_data <- data.frame(
              Staff_Name = "",
              Direct_Rate = 0,
              stringsAsFactors = FALSE
            )
            staff_matrix_data(initial_data)
          }
          
          ## Load labor_categories data ----
          cat("=== CHECKING ROLES CONDITION ===\n")
          cat("result$labor_categories is null:", is.null(result$labor_categories), "\n")
          cat("result$labor_categories nrow:", if(is.null(result$labor_categories)) "NULL" else nrow(result$labor_categories), "\n")
          
          if (!is.null(result$labor_categories) && nrow(result$labor_categories) > 0) {
            cat("=== Loading Labor Categories ===\n")
            cat("Labor Categories found:", nrow(result$labor_categories), "\n")
            cat("LaborCategory names:", paste(result$labor_categories$labor_cat_name, collapse = ", "), "\n")
            
            labor_categories_df <- data.frame(
              LaborCategory = result$labor_categories$labor_cat_name,
              stringsAsFactors = FALSE
            )
            cat("Setting laborCats_data with", nrow(labor_categories_df), "labor_categories\n")
            laborCats_data(labor_categories_df)
            cat("Labor Categories_data set. Current value:", nrow(laborCats_data()), "labor_categories\n")
            
          } else {
            cat("No labor_categories found for project - CONDITION FAILED\n")
            cat("Condition check: !is.null(result$labor_categories):", !is.null(result$labor_categories), "\n")
            cat("Condition check: nrow(result$labor_categories) > 0:", if(is.null(result$labor_categories)) "NULL so FALSE" else nrow(result$labor_categories) > 0, "\n")
            labor_categories_df <- data.frame(LaborCategory = character(0), stringsAsFactors = FALSE)
            laborCats_data(labor_categories_df)
          }
          
          ## Rates ----
          if (!is.null(result$labor_cat_rates) && nrow(result$labor_cat_rates) > 0) {
            cat("=== Loading LaborCategory Rates ===\n")
            cat("LaborCategory rates found:", nrow(result$labor_cat_rates), "\n")
            
            # Store database format
            laborCat_rates_data(result$labor_cat_rates)
            
            # Determine variation type from loaded data
            has_multiple_years <- length(unique(result$labor_cat_rates$year_number)) > 1
            has_multiple_tasks <- length(unique(result$labor_cat_rates$task_number)) > 1
            
            vary_by_value <- if (has_multiple_years && has_multiple_tasks) {
              "Both"
            } else if (has_multiple_years) {
              "Year"
            } else {
              "Task"
            }
            
            cat("Detected variation type:", vary_by_value, "\n")
            
            # Update the radio button FIRST
            updateRadioButtons(session, "rates_hours_vary_by", selected = vary_by_value)
            
            # Convert to matrix format for display table
            if (!is.null(labor_categories_df) && nrow(labor_categories_df) > 0 && !is.null(tasks_df) && nrow(tasks_df) > 0) {
              
              # Generate column names based on detected variation type
              if (vary_by_value == "Task") {
                expected_cols <- paste0("Task_", tasks_df$Task_Number)
              } else if (vary_by_value == "Year") {
                unique_years <- unique(result$labor_cat_rates$year_number)
                expected_cols <- sort(unique_years)
              } else if (vary_by_value == "Both") {
                unique_years <- unique(result$labor_cat_rates$year_number)
                unique_tasks <- unique(result$labor_cat_rates$task_number)
                task_year_combinations <- expand.grid(
                  Task = paste0("Task_", sort(as.numeric(unique_tasks))),
                  Year = sort(unique_years),
                  stringsAsFactors = FALSE
                )
                expected_cols <- paste0(task_year_combinations$Task, "_", task_year_combinations$Year)
              }
              
              rates_matrix <- data.frame(LaborCategory = labor_categories_df$LaborCategory, stringsAsFactors = FALSE)
              
              # Fill in rates from database
              for (col_name in expected_cols) {
                rates_matrix[[col_name]] <- 0
                
                for (j in 1:nrow(labor_categories_df)) {
                  labor_cat_name <- labor_categories_df$LaborCategory[j]
                  
                  # Parse column name to get task and year
                  if (vary_by_value == "Task" && startsWith(col_name, "Task_")) {
                    task_num <- sub("Task_(\\d+)", "\\1", col_name)
                    year_num <- "Year_1"
                  } else if (vary_by_value == "Year" && startsWith(col_name, "Year_")) {
                    task_num <- "1"  # Default task number for Year-only mode
                    year_num <- col_name
                  } else if (vary_by_value == "Both" && grepl("Task_\\d+_Year_\\d+", col_name)) {
                    # Parse "Task_1_Year_1" format correctly
                    parts <- strsplit(col_name, "_")[[1]]
                    if (length(parts) >= 4 && parts[1] == "Task" && parts[3] == "Year") {
                      task_num <- parts[2]  # Get "1" from "Task_1_Year_1"
                      year_num <- paste0(parts[3], "_", parts[4])  # Get "Year_1" from "Task_1_Year_1"
                      
                      cat("DEBUG: Parsing column", col_name, "-> task_num:", task_num, "year_num:", year_num, "\n")
                    } else {
                      cat("WARNING: Could not parse column name:", col_name, "\n")
                      next
                    }
                  } else {
                    next
                  }
                  
                  # Find matching rate in database
                  rate_entry <- result$labor_cat_rates[
                    result$labor_cat_rates$labor_cat_name == labor_cat_name & 
                      result$labor_cat_rates$task_number == task_num &
                      result$labor_cat_rates$year_number == year_num, 
                  ]
                  
                  cat("DEBUG: Looking for rate - labor_cat:", labor_cat_name, "task_number:", task_num, "year_number:", year_num, "\n")
                  
                  if (nrow(rate_entry) > 0) {
                    rates_matrix[j, col_name] <- rate_entry$hourly_rate[1]
                    cat("SUCCESS: Loaded rate ", labor_cat_name, col_name, "=", rate_entry$hourly_rate[1], "\n")
                  } else {
                    cat("NOT FOUND: No rate entry found for ", labor_cat_name, col_name, "\n")
                  }
                }
              }
              
              isolate(rates_data(rates_matrix))
              
              cat("Rates matrix populated with", ncol(rates_matrix)-1, "columns\n")
            }
            
          } else {
            cat("No labor category rates found\n")
            laborCat_rates_data(data.frame(
              labor_cat_name = character(0),
              task_number = character(0),
              year_number = character(0),
              hourly_rate = numeric(0),
              stringsAsFactors = FALSE
            ))
          }
          
          cat("=== DEBUG: Column to Database Mapping ===\n")
          for (col_name in expected_cols) {
            if (vary_by_value == "Both" && grepl("Task_\\d+_Year_\\d+", col_name)) {
              parts <- strsplit(col_name, "_")[[1]]
              if (length(parts) >= 4) {
                task_num <- parts[2]
                year_num <- paste0(parts[3], "_", parts[4])
                cat("Column:", col_name, "-> Task:", task_num, "Year:", year_num, "\n")
              }
            }
          }
          cat("=== Available in database ===\n")
          for (i in 1:min(10, nrow(result$labor_cat_rates))) {
            row <- result$labor_cat_rates[i, ]
            cat("DB Row:", row$labor_cat_name, "Task:", row$task_number, "Year:", row$year_number, "Rate:", row$hourly_rate, "\n")
          }
          
          
          ## Hours ----
          if (!is.null(result$labor_cat_hours) && nrow(result$labor_cat_hours) > 0) {
            cat("=== Loading LaborCategory Hours ===\n") 
            cat("LaborCategory hours found:", nrow(result$labor_cat_hours), "\n")
            
            # Store database format
            laborCat_hours_data(result$labor_cat_hours)
            
            # Convert to matrix format for display table (using same logic as rates)
            if (!is.null(labor_categories_df) && nrow(labor_categories_df) > 0 && !is.null(tasks_df) && nrow(tasks_df) > 0) {
              
              # Use the same variation type as determined for rates
              has_multiple_years <- length(unique(result$labor_cat_hours$year_number)) > 1
              has_multiple_tasks <- length(unique(result$labor_cat_hours$task_number)) > 1
              
              vary_by_value <- if (has_multiple_years && has_multiple_tasks) {
                "Both"
              } else if (has_multiple_years) {
                "Year"
              } else {
                "Task"
              }
              
              # Generate column names based on variation type
              if (vary_by_value == "Task") {
                expected_cols <- paste0("Task_", tasks_df$Task_Number)
              } else if (vary_by_value == "Year") {
                unique_years <- unique(result$labor_cat_hours$year_number)
                expected_cols <- sort(unique_years)
              } else if (vary_by_value == "Both") {
                unique_years <- unique(result$labor_cat_hours$year_number)
                unique_tasks <- unique(result$labor_cat_hours$task_number)
                task_year_combinations <- expand.grid(
                  Task = paste0("Task_", sort(as.numeric(unique_tasks))),
                  Year = sort(unique_years),
                  stringsAsFactors = FALSE
                )
                expected_cols <- paste0(task_year_combinations$Task, "_", task_year_combinations$Year)
              }
              
              hours_matrix <- data.frame(LaborCategory = labor_categories_df$LaborCategory, stringsAsFactors = FALSE)
              
              # Fill in hours from database
              for (col_name in expected_cols) {
                hours_matrix[[col_name]] <- 0
                
                for (j in 1:nrow(labor_categories_df)) {
                  labor_cat_name <- labor_categories_df$LaborCategory[j]
                  
                  # Parse column name to get task and year (same logic as rates)
                  if (vary_by_value == "Task" && startsWith(col_name, "Task_")) {
                    task_num <- sub("Task_(\\d+)", "\\1", col_name)
                    year_num <- "Year_1"
                  } else if (vary_by_value == "Year" && startsWith(col_name, "Year_")) {
                    task_num <- "1"  # Default task number for Year-only mode
                    year_num <- col_name
                  } else if (vary_by_value == "Both" && grepl("Task_\\d+_Year_\\d+", col_name)) {
                    # Parse "Task_1_Year_1" format correctly
                    parts <- strsplit(col_name, "_")[[1]]
                    if (length(parts) >= 4 && parts[1] == "Task" && parts[3] == "Year") {
                      task_num <- parts[2]  # Get "1" from "Task_1_Year_1"
                      year_num <- paste0(parts[3], "_", parts[4])  # Get "Year_1" from "Task_1_Year_1"
                      
                      cat("DEBUG HOURS: Parsing column", col_name, "-> task_num:", task_num, "year_num:", year_num, "\n")
                    } else {
                      cat("WARNING HOURS: Could not parse column name:", col_name, "\n")
                      next
                    }
                  } else {
                    next
                  }
                  
                  # Find matching hours in database
                  hours_entry <- result$labor_cat_hours[
                    result$labor_cat_hours$labor_cat_name == labor_cat_name & 
                      result$labor_cat_hours$task_number == task_num &
                      result$labor_cat_hours$year_number == year_num, 
                  ]
                  
                  cat("DEBUG HOURS: Looking for hours - labor_cat:", labor_cat_name, "task_number:", task_num, "year_number:", year_num, "\n")
                  
                  if (nrow(hours_entry) > 0) {
                    hours_matrix[j, col_name] <- hours_entry$planned_hours[1]
                    cat("SUCCESS HOURS: Loaded hours ", labor_cat_name, col_name, "=", hours_entry$planned_hours[1], "\n")
                  } else {
                    cat("NOT FOUND HOURS: No hours entry found for ", labor_cat_name, col_name, "\n")
                  }
                }
              }
              
              isolate(hours_data(hours_matrix))
              
              cat("Hours matrix populated with", ncol(hours_matrix)-1, "columns\n")
            }
            
          } else {
            cat("No labor categories hours found\n")
            laborCat_hours_data(data.frame(
              labor_cat_name = character(0),
              task_number = character(0),
              year_number = character(0),
              planned_hours = numeric(0),
              stringsAsFactors = FALSE
            ))
          }
          
          
          ## Staff Assignments ----
          loading_staff_assignments(TRUE)
          if (!is.null(result$labor_cat_staff) && nrow(result$labor_cat_staff) > 0) {
            cat("=== Loading Staff Assignments ===\n")
            cat("Staff assignments found:", nrow(result$labor_cat_staff), "\n")
            cat("Raw labor_cat_staff data from database:\n")
            print(result$labor_cat_staff)
            cat("Column names:", paste(names(result$labor_cat_staff), collapse = ", "), "\n")
            
            staff_assignments_df <- data.frame(
              LaborCategory = result$labor_cat_staff$labor_cat_name,
              Staff_Members = ifelse(is.na(result$labor_cat_staff$staff_names), "", result$labor_cat_staff$staff_names),
              stringsAsFactors = FALSE
            )
            laborCat_staff_data(staff_assignments_df)
          } else {
            cat("No staff assignments found\n")
            # Initialize with labor_categories if they exist
            current_labor_categories <- laborCats_data()
            if (!is.null(current_labor_categories) && nrow(current_labor_categories) > 0) {
              staff_assignments_df <- data.frame(
                LaborCategory = current_labor_categories$LaborCategory,
                Staff_Members = "",
                stringsAsFactors = FALSE
              )
              laborCat_staff_data(staff_assignments_df)
            } else {
              laborCat_staff_data(data.frame(
                LaborCategory = character(0),
                Staff_Members = character(0),
                stringsAsFactors = FALSE
              ))
            }
          }
          
          # Clear loading flag after staff assignments are loaded
          Sys.sleep(0.1)  # Small delay to ensure UI updates
          staff_ui_trigger(staff_ui_trigger() + 1)  # Trigger UI update first
          
          # Delay clearing the loading flag to allow UI to render and populate
          shinyjs::delay(1500, {
            loading_staff_assignments(FALSE)
            cat("Loading flag cleared after delay\n")
          })
          
          

          # Fetch hours from WeWorked -----
          cat("=== Starting WeWorked Integration ===\n")
          cat("Project ID Text:", ifelse(is.na(project$project_id_text), "NULL", project$project_id_text), "\n")
          cat("Project Name:", project$project_name, "\n")
          
          # Use project_id_text for WeWorked lookup
          if (!is.null(project$project_id_text) && !is.na(project$project_id_text) && project$project_id_text != "") {
            ww_result <- get_project_hours(
              project_id = project$project_id_text,
              period_start = as.Date(project$start_date),
              period_end = Sys.Date()
            )
            
            if (ww_result$success && nrow(ww_result$data) > 0) {
              cat("=== Processing WeWorked Data ===\n")
              ww_hours <- ww_result$data
              
              # Group by fullName and task to sum hours
              ww_summary <- ww_hours %>%
                group_by(fullName, task) %>%
                summarise(total_hours = sum(hours, na.rm = TRUE), .groups = 'drop')
              
              cat("WeWorked Summary:\n")
              print(ww_summary)
              
              # Store WeWorked data in a reactive value for future use
              weworked_data(ww_summary)
              
              # Show notification about WeWorked data
              total_ww_hours <- sum(ww_hours$hours, na.rm = TRUE)
              unique_staff_count <- length(unique(ww_hours$fullName))
              unique_tasks_count <- length(unique(ww_hours$task))
              
              showNotification(
                paste0("WeWorked data loaded: ",format(round(total_ww_hours, 1), big.mark = ","), 
                       " billable hours from ", unique_staff_count, " staff across ", 
                       unique_tasks_count, " tasks"),
                type = "message",
                duration = 5
              )
              
              cat("=== WeWorked Integration Complete ===\n")
              
            } else if (!ww_result$success) {
              cat("WeWorked API Error:", ww_result$message, "\n")
              showNotification(
                paste0("WeWorked API issue: ", ww_result$message),
                type = "warning",
                duration = 5
              )
            } else {
              cat("No WeWorked hours found\n")
              showNotification(
                "No billable hours found in WeWorked for this project",
                type = "message",
                duration = 3
              )
            }
          } else {
            cat("No project ID available for WeWorked lookup\n")
            showNotification(
              "No Project ID available for WeWorked integration. Please set Project ID field.",
              type = "warning",
              duration = 4
            )
          }
          
          showNotification(
            paste0("Project '", project$project_name, "' loaded successfully!"), 
            type = "message", 
            duration = 3
          )
          
        } else {
          showNotification(paste("Error loading project:", result$message), type = "error")
        }
        
        # Delay clearing loading flag to ensure matrices are fully set
        shinyjs::delay(500, {
          currently_loading(FALSE)
          cat("Loading flag cleared - observers can now run\n")
        })
        
      }
      # If it's a new project name, do nothing - let user fill in the details
    }, ignoreInit = TRUE)
    
    
    # Update Year 2 starts default when period of performance changes
    observeEvent(input$period_of_performance, {
      if (!is.null(input$period_of_performance) && length(input$period_of_performance) == 2) {
        default_year_2 <- input$period_of_performance[1] + 365
        updateDateInput(session, "year_2_starts", value = default_year_2)
      }
    }, ignoreInit = TRUE)
    
    
    # Clear form ----
    observeEvent(clear_trigger(), {
      if(!is.null(clear_trigger())) {
        # Reset all inputs to defaults
        updateTextInput(session, "project_name", value = "")
        updateTextInput(session, "project_id", value = "")
        updateTextInput(session, "client_name", value = "")
        updateTextInput(session, "project_manager", value = "")
        updateTextAreaInput(session, "project_description", value = "")
        updateTextAreaInput(session, "project_notes", value = "")
        updateDateRangeInput(session, "period_of_performance", 
                             start = Sys.Date(),
                             end = Sys.Date() + 365)
        updateNumericInput(session, "total_dollar_value", value = 0)
        updateNumericInput(session, "overhead_multiplier", value = 1.0)
        
        
        # Clear WeWorked Status display
        runjs(paste0('$("#', ns("weworked_status"), '").html("<i class=\\"fa fa-clock\\"></i> Not yet loaded");'))
        runjs(paste0('$("#', ns("weworked_status"), '").css("background-color", "#f8f9fa");'))
        # Clear Spending Status display 
        runjs(paste0('$("#', ns("spending_status"), '").html("<i class=\\"fa fa-calculator\\"></i> Waiting for data...");'))
        runjs(paste0('$("#', ns("spending_status"), '").css("background-color", "#f8f9fa");'))
        
        # Reset WeWorked reactive data
        weworked_data(data.frame())
        
        # Clear staffing data
        cat("=== CLEARING ROLES DATA (clear_trigger) ===\n")
        laborCats_data(data.frame(LaborCategory = character(0), stringsAsFactors = FALSE))
        rates_data(data.frame())
        hours_data(data.frame()) 
        laborCat_staff_data(data.frame())
        
        # Reset dropdown
        updateSelectInput(session, "project_name", selected = "")
        
        # Reset reactive data
        initial_task_data <- data.frame(
          Task_Number = "1",
          Task_Name = "",
          Start_Date = format(Sys.Date(), "%m/%d/%Y"),
          End_Date = format(Sys.Date() + 30, "%m/%d/%Y"),
          Toxcel_Labor_Budget = 0,
          stringsAsFactors = FALSE
        )
        task_budget_data(initial_task_data)
        
        deliverables_data(data.frame(
          Deliverable_Name = character(0),
          Linked_Task = character(0),
          Due_Date = as.Date(character(0)),
          stringsAsFactors = FALSE
        ))
        
        initial_staff_data <- data.frame(
          Staff_Name = "",
          Direct_Rate = 0,
          stringsAsFactors = FALSE
        )
        staff_matrix_data(initial_staff_data)
        
        showNotification("Form cleared", type = "message", duration = 2)
      }
    })
    
    # Reactive values for storing data
    task_budget_data <- reactiveVal()
    
    deliverables_data <- reactiveVal(data.frame(
      Deliverable_Name = character(0),
      Linked_Task = character(0),
      Due_Date = as.Date(character(0)),
      stringsAsFactors = FALSE
    ))
  
    
    # Initialize task budget ----
    observe({
      if (is.null(task_budget_data())) {
        initial_data <- data.frame(
          Task_Number = "1",
          Task_Name = "",
          Start_Date = format(Sys.Date(), "%m/%d/%Y"),
          End_Date = format(Sys.Date() + 30, "%m/%d/%Y"),
          Toxcel_Labor_Budget = 0,
          stringsAsFactors = FALSE
        )
        task_budget_data(initial_data)
      }
    })
  
    
    # Project summary ----
    output$project_summary <- renderText({
      req(input$project_name, input$client_name)
      
      duration <- as.numeric(input$period_of_performance[2] - input$period_of_performance[1])
      tasks <- task_budget_data()
      deliverables <- deliverables_data()
      
      task_count <- if(!is.null(tasks)) nrow(tasks) else 0
      deliverable_count <- if(!is.null(deliverables)) nrow(deliverables) else 0
      
      paste0(
        "Project: ", input$project_name, "\n",
        "Client: ", input$client_name, "\n",
        "Manager: ", input$project_manager, "\n",
        "Duration: ", duration, " days\n",
        "Tasks: ", task_count, " | Deliverables: ", deliverable_count, "\n",
        "Total Value: $", format(round(input$total_dollar_value, 2), nsmall = 2, big.mark = ","), "\n",
        "Description: ", substr(input$project_description, 1, 100),
        if(nchar(input$project_description) > 100) "..." else ""
      )
    })
    
 
    
    # Add task ----
    observeEvent(input$add_task_budget, {
      current_data <- task_budget_data()
      next_number <- get_next_task_number(current_data)
      
      new_row <- data.frame(
        Task_Number = next_number,
        Task_Name = "",
        Start_Date = format(Sys.Date(), "%m/%d/%Y"),
        End_Date = format(Sys.Date() + 30, "%m/%d/%Y"),
        Toxcel_Labor_Budget = 0,
        stringsAsFactors = FALSE
      )
      
      updated_data <- rbind(current_data, new_row)
      task_budget_data(updated_data)
    })
    
    
    
    # Sort tasks by task number ----
    observeEvent(input$sort_tasks, {
      current_data <- task_budget_data()
      if (is.null(current_data) || nrow(current_data) <= 1) {
        showNotification("No tasks to sort", type = "warning")
        return()
      }
      
      sorted_indices <- sort_task_numbers(current_data$Task_Number)
      sorted_data <- current_data[sorted_indices, ]
      
      task_budget_data(sorted_data)
      showNotification("Tasks sorted hierarchically", type = "message")
    })
    
    # Render task budget table ----
    output$task_budget_table <- rhandsontable::renderRHandsontable({
      req(task_budget_data())
      
      data <- task_budget_data()
      
      if (nrow(data) == 0) return(NULL)
      
      hot <- rhandsontable::rhandsontable(data, rowHeaders = FALSE, height = 300) %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
        rhandsontable::hot_cols(columnSorting = FALSE, manualRowMove = TRUE) %>%
        rhandsontable::hot_col("Task_Number", type = "text", readOnly = FALSE) %>%
        rhandsontable::hot_col("Task_Name", type = "text", allowInvalid = FALSE) %>%
        rhandsontable::hot_col("Start_Date", type = "text") %>%
        rhandsontable::hot_col("End_Date", type = "text") %>%
        rhandsontable::hot_col("Toxcel_Labor_Budget", type = "numeric", format = "$0,0.00", allowInvalid = FALSE)
      
      # Set readable column headers
      hot %>% rhandsontable::hot_cols(colHeaders = c("Task #", "Task Name", "Start Date", "End Date", "Labor Budget ($)"))
    })
    
    # Update task budget data when table is edited ----
    observeEvent(input$task_budget_table, {
      if (!is.null(input$task_budget_table)) {
        updated_data <- rhandsontable::hot_to_r(input$task_budget_table)
        task_budget_data(updated_data)
      }
    })
    
    # Update staff assignment data from dynamic inputs with validation ----
    observe({
      labor_categories <- laborCats_data()
      if (is.null(labor_categories) || nrow(labor_categories) == 0) return()
      
      # Don't collect during loading to prevent overwriting loaded data
      cat("=== STAFF OBSERVER TRIGGERED ===\n")
      cat("loading_staff_assignments flag:", loading_staff_assignments(), "\n")
      if (loading_staff_assignments()) {
        cat("OBSERVER BLOCKED by loading flag\n")
        return()
      }
      
      # Also block if currently loading project
      if (currently_loading()) {
        cat("OBSERVER BLOCKED by currently_loading flag\n") 
        return()
      }
      cat("OBSERVER PROCEEDING to collect staff data\n")
      
      # Collect data from all staff assignment inputs
      new_assignments <- data.frame(
        LaborCategory = character(0),
        Staff_Members = character(0),
        stringsAsFactors = FALSE
      )
      
      # Track for validation
      all_selected_staff <- c()
      validation_messages <- c()
      inputs_ready <- TRUE
      
      for (i in 1:nrow(labor_categories)) {
        labor_cat_name <- labor_categories$LaborCategory[i]
        input_id <- paste0("staff_for_laborCat_", make.names(labor_cat_name))
        
        # Check if input exists before reading
        if (is.null(input[[input_id]])) {
          inputs_ready <- FALSE
          cat("Input", input_id, "not ready yet\n")
          break
        }
        
        # Get selected staff for this labor category
        selected_staff <- input[[input_id]]
        
        if (!is.null(selected_staff) && length(selected_staff) > 0) {
          # Check for duplicates
          duplicates <- intersect(selected_staff, all_selected_staff)
          if (length(duplicates) > 0) {
            validation_messages <- c(validation_messages, 
                                     paste("Warning:", paste(duplicates, collapse = ", "), 
                                           "assigned to multiple labor_categories"))
          }
          all_selected_staff <- c(all_selected_staff, selected_staff)
        }
        
        staff_string <- if (is.null(selected_staff) || length(selected_staff) == 0) {
          ""
        } else {
          paste(selected_staff, collapse = "; ")
        }
        
        new_assignments <- rbind(new_assignments, data.frame(
          LaborCategory = labor_cat_name,
          Staff_Members = staff_string,
          stringsAsFactors = FALSE
        ))
      }
      
      # Only update if all inputs are ready
      if (!inputs_ready) {
        cat("OBSERVER BLOCKED - inputs not ready\n")
        return()
      }
      
      # Show validation messages if any
      if (length(validation_messages) > 0) {
        showNotification(
          paste(validation_messages, collapse = "\n"),
          type = "warning",
          duration = 5
        )
      }
      
      # Update reactive value
      laborCat_staff_data(new_assignments)
    })    
    
    
    # Update linked task choices for deliverables ----
    observe({
      tasks <- task_budget_data()
      if (!is.null(tasks) && nrow(tasks) > 0) {
        # Filter out empty task names
        valid_tasks <- tasks[!is.na(tasks$Task_Name) & tasks$Task_Name != "", ]
        if (nrow(valid_tasks) > 0) {
          choices <- setNames(valid_tasks$Task_Name, 
                              paste0("Task ", valid_tasks$Task_Number, ": ", valid_tasks$Task_Name))
          choices <- c("Select a task..." = "", choices)
        } else {
          choices <- list("No tasks defined yet..." = "")
        }
      } else {
        choices <- list("No tasks defined yet..." = "")
      }
      
      updateSelectInput(session, "linked_task", choices = choices)
    })
    
    # Add deliverable ----
    observeEvent(input$add_deliverable, {
      req(input$deliverable_label, input$linked_task)
      
      if (input$linked_task == "") {
        showNotification("Please select a linked task", type = "warning")
        return()
      }
      
      current_deliverables <- deliverables_data()
      
      new_deliverable <- data.frame(
        Deliverable_Name = input$deliverable_label,
        Linked_Task = input$linked_task,
        Due_Date = input$deliverable_due_date,
        stringsAsFactors = FALSE
      )
      
      updated_deliverables <- rbind(current_deliverables, new_deliverable)
      deliverables_data(updated_deliverables)
      
      # Clear inputs
      updateTextInput(session, "deliverable_label", value = "")
      updateSelectInput(session, "linked_task", selected = "")
    })
    
    # Remove selected deliverable ----
    observeEvent(input$remove_deliverable, {
      req(input$deliverables_table_rows_selected)
      
      current_deliverables <- deliverables_data()
      if (nrow(current_deliverables) > 0) {
        selected_rows <- input$deliverables_table_rows_selected
        updated_deliverables <- current_deliverables[-selected_rows, , drop = FALSE]
        deliverables_data(updated_deliverables)
      }
    })
    
    # Render deliverables table ----
    output$deliverables_table <- DT::renderDT({
      deliverables <- deliverables_data()
      if (nrow(deliverables) == 0) {
        return(NULL)  # Don't render table if no deliverables
      }
      
      # Format dates for display properly
      display_deliverables <- deliverables
      display_deliverables$Due_Date <- format(deliverables$Due_Date, "%Y-%m-%d")
      
      DT::datatable(display_deliverables, 
                    selection = "multiple",
                    options = list(
                      dom = 't',
                      paging = FALSE,     
                      searching = FALSE,  
                      info = FALSE,       
                      scrollX = TRUE
                    ))
    })
    

    
    

    
    # Add labor category ----
    observeEvent(input$add_laborCat, {
      req(input$new_laborCat_name)
      
      current_labor_categories <- laborCats_data()
      new_labor_category <- data.frame(
        LaborCategory = trimws(input$new_laborCat_name),
        stringsAsFactors = FALSE
      )
      
      # Check for duplicates
      if (new_labor_category$LaborCategory %in% current_labor_categories$LaborCategory) {
        showNotification("Labor Category already exists!", type = "warning")
        return()
      }
      
      updated_labor_categories <- rbind(current_labor_categories, new_labor_category)
      laborCats_data(updated_labor_categories)
      
      # Force immediate update of rates/hours tables by triggering staff UI update
      staff_ui_trigger(staff_ui_trigger() + 1)
      
      # Clear input
      updateTextInput(session, "new_laborCat_name", value = "")
      
      showNotification("Labor Category added successfully!", type = "message")
    })
    
    
    # Remove selected labor_categories ----
    observeEvent(input$remove_laborCat, {
      req(input$laborCats_table_rows_selected)
      
      current_labor_categories <- laborCats_data()
      if (nrow(current_labor_categories) > 0) {
        selected_rows <- input$laborCats_table_rows_selected
        updated_labor_categories <- current_labor_categories[-selected_rows, , drop = FALSE]
        laborCats_data(updated_labor_categories)
        
        # Force immediate update of rates/hours tables by triggering staff UI update
        staff_ui_trigger(staff_ui_trigger() + 1)
        
        showNotification("Selected labor categories removed successfully!", type = "message")
      }
    })
    
    # Render labor_categories table ----
    output$laborCats_table <- DT::renderDataTable({
      req(laborCats_data())
      
      DT::datatable(
        laborCats_data(),
        editable = FALSE,
        rownames = FALSE,
        selection = "multiple",
        options = list(
          dom = 't',
          pageLength = 20,
          scrollY = "200px",
          scrollCollapse = TRUE
        )
      )
    })
    
    
    # Update rates and hours tables when labor_categories, tasks, or variation type changes ----
    observe({
      labor_categories <- laborCats_data()
      loading_state <- currently_loading()
      vary_by <- input$rates_hours_vary_by
      
      # Don't recreate matrices during project loading - we handle this manually
      if (loading_state) {
        cat("Observer blocked: project loading in progress\n")
        return()
      }
      
      tasks <- task_budget_data()
      
      cat("=== Observer Triggered ===\n")
      cat("Loading state:", loading_state, "\n")
      cat("Labor Categories available:", if(is.null(labor_categories)) 0 else nrow(labor_categories), "\n")
      cat("Tasks available:", if(is.null(tasks)) 0 else nrow(tasks), "\n")
      cat("Vary by:", vary_by, "\n")
      
      if (is.null(labor_categories) || is.null(tasks) || is.null(vary_by) ||
          nrow(labor_categories) == 0 || nrow(tasks) == 0) {
        cat("Insufficient data - resetting tables\n")
        rates_data(data.frame())
        hours_data(data.frame())
        return()
      }
      
      # Check if we need to update matrices based on labor categories, tasks, or variation type
      current_rates <- rates_data()
      current_hours <- hours_data()
      
      # Get valid tasks and sort by task number
      valid_tasks <- tasks[!is.na(tasks$Task_Name) & tasks$Task_Name != "", ]
      valid_tasks <- valid_tasks[order(as.numeric(valid_tasks$Task_Number)), ]
      
      if (nrow(valid_tasks) == 0) {
        cat("No valid tasks found\n")
        rates_data(data.frame())
        hours_data(data.frame())
        return()
      }
      
      # Generate column names based on variation type
      if (vary_by == "Task") {
        expected_cols <- c("LaborCategory", paste0("Task_", valid_tasks$Task_Number))
      } else if (vary_by == "Year") {
        years <- year_boundaries()
        if (length(years) == 0) return()
        expected_cols <- c("LaborCategory", names(years))
      } else if (vary_by == "Both") {
        years <- year_boundaries()
        if (length(years) == 0) return()
        task_year_combinations <- expand.grid(
          Task = paste0("Task_", valid_tasks$Task_Number),
          Year = names(years),
          stringsAsFactors = FALSE
        )
        expected_cols <- c("LaborCategory", paste0(task_year_combinations$Task, "_", task_year_combinations$Year))
      } else {
        return()
      }
      
      current_labor_categories <- if(is.null(current_rates) || ncol(current_rates) == 0) character(0) else current_rates$LaborCategory
      
      structure_needs_update <- FALSE
      
      # Check if structure needs updating
      if (is.null(current_rates) || 
          !identical(sort(current_labor_categories), sort(labor_categories$LaborCategory)) ||
          !identical(names(current_rates), expected_cols)) {
        structure_needs_update <- TRUE
        cat("Structure needs update - labor categories, tasks, or variation type changed\n")
      }
      
      if (!structure_needs_update) {
        cat("Rates and hours matrices already have correct structure - skipping recreation\n")
        return()
      }
      
      cat("Valid tasks:", nrow(valid_tasks), "\n")
      cat("Labor Categories:", paste(labor_categories$LaborCategory, collapse = ", "), "\n")
      cat("Expected columns:", paste(expected_cols, collapse = ", "), "\n")
      
      # Create rates matrix with current data preserved
      rates_matrix <- data.frame(LaborCategory = labor_categories$LaborCategory, stringsAsFactors = FALSE)
      for (col_name in expected_cols[-1]) {  # Skip "LaborCategory"
        # Initialize with 0, but preserve existing values if they exist
        rates_matrix[[col_name]] <- 0
        if (!is.null(current_rates) && col_name %in% names(current_rates)) {
          for (j in 1:nrow(labor_categories)) {
            labor_cat_name <- labor_categories$LaborCategory[j]
            existing_row <- current_rates[current_rates$LaborCategory == labor_cat_name, ]
            if (nrow(existing_row) > 0 && !is.na(existing_row[[col_name]][1])) {
              rates_matrix[j, col_name] <- existing_row[[col_name]][1]
            }
          }
        }
      }
      rates_data(rates_matrix)
      cat("Rates matrix updated with", ncol(rates_matrix)-1, "columns\n")
      
      # Create hours matrix with current data preserved
      hours_matrix <- data.frame(LaborCategory = labor_categories$LaborCategory, stringsAsFactors = FALSE)
      for (col_name in expected_cols[-1]) {  # Skip "LaborCategory"
        # Initialize with 0, but preserve existing values if they exist
        hours_matrix[[col_name]] <- 0
        if (!is.null(current_hours) && col_name %in% names(current_hours)) {
          for (j in 1:nrow(labor_categories)) {
            labor_cat_name <- labor_categories$LaborCategory[j]
            existing_row <- current_hours[current_hours$LaborCategory == labor_cat_name, ]
            if (nrow(existing_row) > 0 && !is.na(existing_row[[col_name]][1])) {
              hours_matrix[j, col_name] <- existing_row[[col_name]][1]
            }
          }
        }
      }
      hours_data(hours_matrix)
      cat("Hours matrix updated with", ncol(hours_matrix)-1, "columns\n")
      
      # Update staff assignment table to match current labor categories
      current_staff <- laborCat_staff_data()
      if (is.null(current_staff) || !identical(sort(current_staff$LaborCategory), sort(labor_categories$LaborCategory))) {
        # Preserve existing staff assignments where possible
        staff_assignments <- data.frame(
          LaborCategory = labor_categories$LaborCategory,
          Staff_Members = "",
          stringsAsFactors = FALSE
        )
        
        if (!is.null(current_staff) && nrow(current_staff) > 0) {
          for (i in 1:nrow(staff_assignments)) {
            labor_cat_name <- staff_assignments$LaborCategory[i]
            existing_assignment <- current_staff[current_staff$LaborCategory == labor_cat_name, ]
            if (nrow(existing_assignment) > 0) {
              staff_assignments$Staff_Members[i] <- existing_assignment$Staff_Members[1]
            }
          }
        }
        
        laborCat_staff_data(staff_assignments)
        cat("Staff assignment table updated to match labor categories\n")
      }
    })    
    
    # Render rates table using rhandsontable ----
    output$rates_table <- rhandsontable::renderRHandsontable({
      req(rates_data())
      
      rates_df <- rates_data()
      
      if (nrow(rates_df) == 0) return(NULL)
      
      # Create column headers for display
      col_names <- names(rates_df)
      display_names <- sapply(col_names, function(name) {
        if (name == "LaborCategory") {
          return("Labor Category")
        } else if (startsWith(name, "Task_")) {
          return(name)  # Keep as "Task_1", "Task_2", etc.
        } else {
          return(name)
        }
      })
      
      hot <- rhandsontable::rhandsontable(rates_df, rowHeaders = FALSE, height = 300) %>%
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        rhandsontable::hot_cols(columnSorting = FALSE, manualRowMove = FALSE) %>%
        rhandsontable::hot_col("LaborCategory", type = "text", readOnly = TRUE, halign = "htLeft")
      
      # Configure task columns
      for (col_name in col_names) {
        if (startsWith(col_name, "Task_")) {
          hot <- hot %>% rhandsontable::hot_col(col_name, 
                                                type = "numeric", 
                                                format = "$0,0.00", 
                                                allowInvalid = FALSE,
                                                halign = "htRight")
        }
      }
      
      # Set column headers
      hot %>% rhandsontable::hot_cols(colHeaders = display_names)
    })
    
    # Render hours table using rhandsontable ----
    output$hours_table <- rhandsontable::renderRHandsontable({
      req(hours_data())
      
      hours_df <- hours_data()
      
      if (nrow(hours_df) == 0) return(NULL)
      
      # Create column headers for display
      col_names <- names(hours_df)
      display_names <- sapply(col_names, function(name) {
        if (name == "LaborCategory") {
          return("Labor Category")
        } else if (startsWith(name, "Task_")) {
          return(name)  # Keep as "Task_1", "Task_2", etc.
        } else {
          return(name)
        }
      })
      
      hot <- rhandsontable::rhandsontable(hours_df, rowHeaders = FALSE, height = 300) %>%
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        rhandsontable::hot_cols(columnSorting = FALSE, manualRowMove = FALSE) %>%
        rhandsontable::hot_col("LaborCategory", type = "text", readOnly = TRUE, halign = "htLeft")
      
      # Configure task columns
      for (col_name in col_names) {
        if (startsWith(col_name, "Task_")) {
          hot <- hot %>% rhandsontable::hot_col(col_name, 
                                                type = "numeric", 
                                                format = "0,0.0", 
                                                allowInvalid = FALSE,
                                                halign = "htRight")
        }
      }
      
      # Set column headers
      hot %>% rhandsontable::hot_cols(colHeaders = display_names)
    })
    
    # Dynamic staff assignment UI generator ----
    output$dynamic_staff_assignments <- renderUI({
      req(laborCats_data())
      
      # React to both laborCats_data() AND staff_ui_trigger changes
      labor_categories <- laborCats_data()
      staff_ui_trigger()  # React to trigger for UI updates
      current_assignments <- laborCat_staff_data()
      
      cat("=== RENDERUI TRIGGERED ===\n")
      cat("Labor Categories:", nrow(labor_categories), "\n")
      cat("Staff assignments:", if(is.null(current_assignments)) "NULL" else nrow(current_assignments), "\n")
      cat("UI Trigger value:", staff_ui_trigger(), "\n")
      
      if (nrow(labor_categories) == 0) {
        return(div(
          style = "text-align: center; color: #666; padding: 20px;",
          "No labor_categories defined yet. Add labor_categories in the Labor Categories tab first."
        ))
      }
      
      # Calculate currently assigned staff across all labor categories
      all_assigned_staff <- c()
      if (!is.null(current_assignments) && nrow(current_assignments) > 0) {
        for (i in 1:nrow(current_assignments)) {
          if (!is.na(current_assignments$Staff_Members[i]) && current_assignments$Staff_Members[i] != "") {
            staff_list <- trimws(strsplit(current_assignments$Staff_Members[i], ";")[[1]])
            all_assigned_staff <- c(all_assigned_staff, staff_list[staff_list != ""])
          }
        }
      }
      
      cat("DEBUG: All assigned staff:", paste(all_assigned_staff, collapse=", "), "\n")
      
      # Create UI elements for each labor category
      labor_category_inputs <- lapply(1:nrow(labor_categories), function(i) {
        labor_cat_name <- labor_categories$LaborCategory[i]
        input_id <- paste0("staff_for_laborCat_", make.names(labor_cat_name))
        
        # Get current selection for this labor category
        current_selection <- c()
        if (!is.null(current_assignments) && nrow(current_assignments) > 0) {
          labor_category_row <- current_assignments[current_assignments$LaborCategory == labor_cat_name, ]
          if (nrow(labor_category_row) > 0 && !is.na(labor_category_row$Staff_Members[1]) && labor_category_row$Staff_Members[1] != "") {
            current_selection <- trimws(strsplit(labor_category_row$Staff_Members[1], ";")[[1]])
            current_selection <- current_selection[current_selection != ""]
          }
        }
        
        # Calculate available staff: 
        # All staff MINUS those assigned to OTHER labor categories PLUS current selections for THIS labor category
        other_assigned <- setdiff(all_assigned_staff, current_selection)
        available_staff <- setdiff(WWusers$fullName, other_assigned)
        
        cat("DEBUG:", labor_cat_name, "- Current selection:", paste(current_selection, collapse=", "), "\n")
        cat("DEBUG:", labor_cat_name, "- Available staff count:", length(available_staff), "\n")
        
        fluidRow(
          column(3,
                 tags$label(paste0(labor_cat_name, ":"), 
                            style = "font-weight: bold; display: inline-block; vertical-align: top; margin-top: 8px;")
          ),
          column(9, 
                 selectizeInput(
                   inputId = ns(input_id),
                   label = NULL,
                   choices = sort(available_staff),  # Sort alphabetically for better UX
                   selected = current_selection,
                   multiple = TRUE,
                   options = list(
                     placeholder = paste("Select staff members for", labor_cat_name, "..."),
                     maxItems = 10,
                     plugins = list('remove_button', 'drag_drop'),
                     create = FALSE  # Prevent creation of new options
                   )
                 )
          )
        )
      })
      
      do.call(tagList, labor_category_inputs)
    })    
    
    
    # Monitor individual staff assignment input changes and update choices ----
    observe({
      labor_categories <- laborCats_data()
      if (is.null(labor_categories) || nrow(labor_categories) == 0) return()
      
      # Don't process during loading
      if (loading_staff_assignments() || currently_loading()) return()
      
      # Monitor changes to any staff assignment input
      lapply(1:nrow(labor_categories), function(i) {
        labor_cat_name <- labor_categories$LaborCategory[i]
        input_id <- paste0("staff_for_laborCat_", make.names(labor_cat_name))
        
        # Create reactive expression for this input
        observeEvent(input[[input_id]], {
          cat("=== STAFF INPUT CHANGE DETECTED ===\n")
          cat("Changed input:", input_id, "\n")
          cat("New value:", paste(input[[input_id]], collapse=", "), "\n")
          
          # Trigger UI refresh after short delay to allow all inputs to update
          shinyjs::delay(200, {
            staff_ui_trigger(staff_ui_trigger() + 1)
            cat("UI trigger incremented due to staff assignment change\n")
          })
        }, ignoreInit = TRUE, ignoreNULL = FALSE)
      })
    })
    
    # Handle edits to rates table ----
    observeEvent(input$rates_table, {
      if (!is.null(input$rates_table)) {
        tryCatch({
          updated_data <- rhandsontable::hot_to_r(input$rates_table)
          # Ensure numeric columns are properly converted
          for (col_name in names(updated_data)) {
            if (startsWith(col_name, "Task_")) {
              updated_data[[col_name]] <- as.numeric(updated_data[[col_name]])
              updated_data[[col_name]][is.na(updated_data[[col_name]])] <- 0
            }
          }
          rates_data(updated_data)
        }, error = function(e) {
          showNotification("Error updating rates table", type = "warning")
        })
      }
    })
    
    # Handle edits to hours table ----
    observeEvent(input$hours_table, {
      if (!is.null(input$hours_table)) {
        tryCatch({
          updated_data <- rhandsontable::hot_to_r(input$hours_table)
          # Ensure numeric columns are properly converted
          for (col_name in names(updated_data)) {
            if (startsWith(col_name, "Task_")) {
              updated_data[[col_name]] <- as.numeric(updated_data[[col_name]])
              updated_data[[col_name]][is.na(updated_data[[col_name]])] <- 0
            }
          }
          hours_data(updated_data)
        }, error = function(e) {
          showNotification("Error updating hours table", type = "warning")
        })
      }
    })
    
    
    # Sync matrix edits to laborCat_rates_data format ----
    observeEvent(rates_data(), {
      if (!is.null(rates_data()) && nrow(rates_data()) > 0) {
        current_rates <- rates_data()
        vary_by <- input$rates_hours_vary_by
        
        # DEBUG: Show what was in the reactive before sync
        old_rates <- laborCat_rates_data()
        cat("DEBUG: Before sync - old rates data rows:", nrow(old_rates), "\n")
        if (nrow(old_rates) > 0) {
          cat("DEBUG: Sample old rate:", old_rates$labor_cat_name[1], "Task:", old_rates$task_number[1], "Year:", old_rates$year_number[1], "\n")
        }
        
        # Clear existing data first
        laborCat_rates_data(data.frame(
          labor_cat_name = character(0),
          task_number = character(0),
          year_number = character(0),
          hourly_rate = numeric(0),
          stringsAsFactors = FALSE
        ))
        
        labor_cat_rates_list <- list()
        
        for (i in 1:nrow(current_rates)) {
          labor_cat_name <- current_rates$LaborCategory[i]
          for (col_name in names(current_rates)) {
            if (col_name != "LaborCategory") {
              hourly_rate <- current_rates[[col_name]][i]
              if (!is.na(hourly_rate)) {
                # Parse column name based on variation type
                if (vary_by == "Task" && startsWith(col_name, "Task_")) {
                  task_number <- sub("Task_(\\d+)", "\\1", col_name)
                  year_number <- "Year_1"  # Default for Task-only mode
                } else if (vary_by == "Year" && startsWith(col_name, "Year_")) {
                  task_number <- "1"  # Default for Year-only mode (just the number)
                  year_number <- col_name
                } else if (vary_by == "Both" && grepl("Task_\\d+_Year_\\d+", col_name)) {
                  # Parse "Task_1_Year_1" format correctly
                  parts <- strsplit(col_name, "_")[[1]]
                  if (length(parts) >= 4 && parts[1] == "Task" && parts[3] == "Year") {
                    task_number <- parts[2]  # Get "1" from "Task_1_Year_1" 
                    year_number <- paste0(parts[3], "_", parts[4])  # Get "Year_1" from "Task_1_Year_1"
                    
                    cat("DEBUG SYNC RATES: Column:", col_name, "-> task_number:", task_number, "year_number:", year_number, "\n")
                  } else {
                    cat("ERROR SYNC RATES: Invalid column format:", col_name, "\n")
                    next
                  }
                } else {
                  next  # Skip invalid column names
                }
                
                cat("DEBUG: About to store - labor_cat:", labor_cat_name, "task_number:", task_number, "year_number:", year_number, "rate:", hourly_rate, "\n")
                
                labor_cat_rates_list[[length(labor_cat_rates_list) + 1]] <- data.frame(
                  labor_cat_name = labor_cat_name,
                  task_number = task_number,
                  year_number = year_number,
                  hourly_rate = hourly_rate,
                  stringsAsFactors = FALSE
                )
              }
            }
          }
        }
        
        if (length(labor_cat_rates_list) > 0) {
          labor_cat_rates_df <- do.call(rbind, labor_cat_rates_list)
          laborCat_rates_data(labor_cat_rates_df)
        }
      }
    })
    
    # Sync matrix edits to laborCat_hours_data format ----
    observeEvent(hours_data(), {
      if (!is.null(hours_data()) && nrow(hours_data()) > 0) {
        current_hours <- hours_data()
        vary_by <- input$rates_hours_vary_by
        # Clear existing data first
        laborCat_hours_data(data.frame(
          labor_cat_name = character(0),
          task_number = character(0),
          year_number = character(0),
          planned_hours = numeric(0),
          stringsAsFactors = FALSE
        ))
        
        labor_cat_hours_list <- list()
        
        for (i in 1:nrow(current_hours)) {
          labor_cat_name <- current_hours$LaborCategory[i]
          for (col_name in names(current_hours)) {
            if (col_name != "LaborCategory") {
              planned_hours <- current_hours[[col_name]][i]
              if (!is.na(planned_hours)) {
                # Parse column name based on variation type
                if (vary_by == "Task" && startsWith(col_name, "Task_")) {
                  task_number <- sub("Task_(\\d+)", "\\1", col_name)
                  year_number <- "Year_1"  # Default for Task-only mode
                } else if (vary_by == "Year" && startsWith(col_name, "Year_")) {
                  task_number <- "1"  # Default for Year-only mode (just the number)
                  year_number <- col_name
                } else if (vary_by == "Both" && grepl("Task_\\d+_Year_\\d+", col_name)) {
                  # Parse "Task_1_Year_1" format correctly  
                  parts <- strsplit(col_name, "_")[[1]]
                  cat("DEBUG Sync HOURS: Column:", col_name, "Parts:", paste(parts, collapse=","), "\n")
                  if (length(parts) >= 4 && parts[1] == "Task" && parts[3] == "Year") {
                    task_number <- parts[2]  # Get "1" from "Task_1_Year_1"
                    year_number <- paste0(parts[3], "_", parts[4])  # Get "Year_1" from "Task_1_Year_1"
                    cat("DEBUG Sync HOURS: Parsed task_number:", task_number, "year_number:", year_number, "\n")
                  } else {
                    cat("ERROR SYNC HOURS: Invalid column format:", col_name, "\n")
                    next  # Skip invalid column names
                  }
                } else {
                  next  # Skip invalid column names
                }
                
                cat("DEBUG: About to store - labor_cat:", labor_cat_name, "task_number:", task_number, "year_number:", year_number, "hours:", planned_hours, "\n")
                
                labor_cat_hours_list[[length(labor_cat_hours_list) + 1]] <- data.frame(
                  labor_cat_name = labor_cat_name,
                  task_number = task_number,
                  year_number = year_number,
                  planned_hours = planned_hours,
                  stringsAsFactors = FALSE
                )
              }
            }
          }
        }
        
        if (length(labor_cat_hours_list) > 0) {
          labor_cat_hours_df <- do.call(rbind, labor_cat_hours_list)
          laborCat_hours_data(labor_cat_hours_df)
        }
      }
    })    
    
    # Render cost check table ----
    output$cost_check_table <- DT::renderDT({
      cost_data <- cost_check_data()
      
      if (nrow(cost_data) == 0) {
        return(NULL)
      }
      
      # Create display data with pre-formatted strings
      display_data <- cost_data
      numeric_cols <- names(display_data)[2:ncol(display_data)]
      
      # Update the values in the Cost Analysis column to include units
      display_data[1, 1] <- "Budget ($)"
      display_data[2, 1] <- "Planned ($)"
      display_data[3, 1] <- "Variance ($)"
      display_data[4, 1] <- "Variance (%)"
      display_data[5, 1] <- "Status"
      
      for (col in numeric_cols) {
        # Format each row appropriately
        values <- display_data[[col]]
        
        # Budget row (1) - format as number only (no $)
        values[1] <- format(round(as.numeric(values[1]), 0), big.mark = ",")
        
        # Calculated row (2) - format as number only (no $)
        values[2] <- format(round(as.numeric(values[2]), 0), big.mark = ",")
        
        # Variance row (3) - format as number only (no $)
        values[3] <- format(round(as.numeric(values[3]), 0), big.mark = ",")
        
        # Variance % row (4) - format as percentage (no % sign)
        pct_val <- as.numeric(values[4])
        values[4] <- paste0(ifelse(pct_val >= 0, "+", ""), round(pct_val, 0))
        
        # Status row (5) - leave as is
        
        display_data[[col]] <- values
      }
      
      # Fix column names: rename first column and replace underscores with spaces
      col_names <- names(display_data)
      col_names[1] <- "Cost"  # Rename the Cost Analysis column to Cost
      col_names <- gsub("_", " ", col_names)  # Replace underscores with spaces in all columns
      names(display_data) <- col_names
      
      dt_table <- DT::datatable(
        display_data,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-left', targets = 0),     # Left-align first column
            list(className = 'dt-center', targets = 1:(ncol(display_data)-1))  # Center-align all other columns
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          columns = "Total",
          fontWeight = "bold",
          backgroundColor = "#f8f9fa"
        )
      
      # Apply color formatting to each task column individually
      task_cols <- names(display_data)[2:ncol(display_data)]
      
      for (col in task_cols) {
        dt_table <- dt_table %>%
          DT::formatStyle(
            columns = col,
            target = 'cell',
            color = DT::styleEqual(
              c("Under Budget", "Over Budget", "On Track"),
              c("#000080", "#FF0000", "#008000")  # Navy blue, Red, Green
            )
          )
      }
      
      return(dt_table)
    })    
    
    # Initialize database ----
    observe({
      setup_database()
    })
    
    # Save to database ----
    observeEvent(input$save_project, {
      # Validate required fields
      if (is.null(input$project_name) || input$project_name == "") {
        showNotification("Please enter a project name before saving.", type = "error")
        return()
      }
      
      if (is.null(input$project_id) || input$project_id == "") {
        showNotification("Please enter a project ID before saving.", type = "error")
        return()
      }
      
      if (is.null(input$client_name) || input$client_name == "") {
        showNotification("Please enter a client name before saving.", type = "error")
        return()
      }
      
      if (is.null(input$period_of_performance) || length(input$period_of_performance) != 2) {
        showNotification("Please select a valid date range before saving.", type = "error")
        return()
      }
      
      # Show progress
      showNotification("Saving project...", type = "message", duration = 2)
      
      cat("\n=== MODULE SAVE DEBUG ===\n")
      cat("Raw input values:\n")
      cat("project_name:", paste(input$project_name, collapse=","), "length:", length(input$project_name), "\n")
      cat("project_id:", paste(input$project_id, collapse=","), "length:", length(input$project_id), "\n")
      cat("client_name:", paste(input$client_name, collapse=","), "length:", length(input$client_name), "\n")
      cat("project_manager:", paste(input$project_manager, collapse=","), "length:", length(input$project_manager), "\n")
      cat("period_of_performance:", paste(input$period_of_performance, collapse=","), "length:", length(input$period_of_performance), "\n")
      cat("total_dollar_value:", paste(input$total_dollar_value, collapse=","), "length:", length(input$total_dollar_value), "\n")
      cat("overhead_multiplier:", paste(input$overhead_multiplier, collapse=","), "length:", length(input$overhead_multiplier), "\n")
      
      # Collect all data
      project_data <- list(
        project_info = list(
          name = input$project_name,
          project_id_text = input$project_id,
          client = input$client_name,
          manager = input$project_manager,
          description = input$project_description,
          notes = if(is.null(input$project_notes)) "" else input$project_notes,
          start_date = input$period_of_performance[1],
          end_date = input$period_of_performance[2],
          total_value = input$total_dollar_value,
          overhead = input$overhead_multiplier
        ),
        task_budget = task_budget_data(),
        deliverables = deliverables_data(),
        labor_categories = laborCats_data(),
        rates = rates_data(),  
        hours = hours_data(),
        labor_cat_rates = laborCat_rates_data(),
        labor_cat_hours = laborCat_hours_data(),
        labor_cat_staff = laborCat_staff_data()
      )
      
      # Save to database
      result <- save_project_to_db(project_data)
      
      if (result$success) {
        showNotification(
          paste0("Project saved successfully! Project ID: ", result$project_id), 
          type = "message", 
          duration = 5
        )
      } else {
        showNotification(result$message, type = "error", duration = 10)
      }
    })   
    
    
    # Gantt ----------
    output$gantt_chart <- ggiraph::renderGirafe({
      req(task_budget_data(), deliverables_data())
      
      p <- create_gantt_chart(
        task_data = task_budget_data(),
        deliverable_data = deliverables_data(),
        project_start = current_project_info()$start_date,
        project_end = current_project_info()$end_date
      )
      
      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_hover_inv(css = "opacity:0.3;"),
          ggiraph::opts_hover(css = "stroke-width:2px; stroke:black;"),
          ggiraph::opts_tooltip(
            css = "background-color:#333;color:white;padding:10px;border-radius:5px;font-size:12px;max-width:300px;"
          ),
          ggiraph::opts_zoom(min = 0.5, max = 3)
        ),
        height_svg = 8,
        width_svg = 12
      )
    })
    
    
    # Update WeWorked status display ----
    observe({
      ww_data <- weworked_data()
      if (!is.null(ww_data) && nrow(ww_data) > 0) {
        total_hours <- sum(ww_data$total_hours, na.rm = TRUE)
        runjs(paste0("
          $('#", ns("weworked_status"), "').html('<i class=\"fa fa-check text-success\"></i> ", 
                     round(total_hours, 1), " hours loaded');
          $('#", ns("weworked_status"), "').css('background-color', '#d4edda');
        "))
      } else if (!is.null(input$project_name) && input$project_name != "") {
        runjs(paste0("
          $('#", ns("weworked_status"), "').html('<i class=\"fa fa-exclamation-triangle text-warning\"></i> No WeWorked data');
          $('#", ns("weworked_status"), "').css('background-color', '#fff3cd');
        "))
      }
    })
    
    # Spending --------------------------
    
    ## Calculate spending data ----
    calculate_spending_data <- reactive({
      req(staff_matrix_data(), task_budget_data(), input$overhead_multiplier)
      
      staff_data <- staff_matrix_data()

      task_data <- task_budget_data()
      ww_data <- weworked_data()
      
      # Get task columns from staff matrix
      task_cols <- names(staff_data)[grepl("^Hours_Task_", names(staff_data))]
      
      if (length(task_cols) == 0) {
        return(data.frame())
      }
      
      # Calculate planned spending for each task
      planned_spending <- data.frame()
      
      # Additional safety check - ensure task_data is not empty
      if (nrow(task_data) == 0) {
        return(data.frame())
      }
      
      for (task_col in task_cols) {
        task_number <- gsub("^Hours_Task_", "", task_col)
        
        # Get task info - fix the column name matching
        task_info <- task_data[task_data$Task_Number == task_number, ]
        if (nrow(task_info) == 0) next
        
        task_name <- task_info$Task_Name[1]
        
        # Calculate planned spending for this task
        task_planned <- 0
        for (i in 1:nrow(staff_data)) {
          if (!is.na(staff_data[i, task_col]) && staff_data[i, task_col] > 0) {
            loaded_rate <- staff_data$Direct_Rate[i] * input$overhead_multiplier
            task_planned <- task_planned + (staff_data[i, task_col] * loaded_rate)
          }
        }
        
        planned_spending <- rbind(planned_spending, data.frame(
          Task_Number = task_number,
          Task_Name = task_name,
          Planned_Spending = task_planned,
          stringsAsFactors = FALSE
        ))
      }
      
      # Calculate actual spending from WeWorked data using labor-category-based rates
      actual_spending <- data.frame()
      
      if (!is.null(ww_data) && nrow(ww_data) > 0) {
        cat("=== CALCULATING ACTUAL SPENDING ===\n")
        cat("WeWorked data rows:", nrow(ww_data), "\n")
        
        # Get labor category assignments and rates data
        labor_cat_staff_assignments <- laborCat_staff_data()
        labor_cat_rates_db <- laborCat_rates_data()
        
        cat("LaborCategory staff assignments rows:", if(is.null(labor_cat_staff_assignments)) 0 else nrow(labor_cat_staff_assignments), "\n")
        cat("LaborCategory rates rows:", if(is.null(labor_cat_rates_db)) 0 else nrow(labor_cat_rates_db), "\n")
        
        if (!is.null(labor_cat_staff_assignments) && nrow(labor_cat_staff_assignments) > 0 &&
            !is.null(labor_cat_rates_db) && nrow(labor_cat_rates_db) > 0) {
          
          # Create elongated labor_cat_staff table using separate_rows
          elongated_labor_cat_staff <- labor_cat_staff_assignments %>%
            filter(!is.na(Staff_Members) & Staff_Members != "") %>%
            separate_rows(Staff_Members, sep = ";") %>%
            mutate(Staff_Name = trimws(Staff_Members)) %>%
            filter(Staff_Name != "") %>%
            select(Staff_Name, LaborCategory) %>%
            as.data.frame()
          
          
          cat("Elongated labor category staff rows:", nrow(elongated_labor_cat_staff), "\n")
          
          if (nrow(elongated_labor_cat_staff) > 0) {
            # Process each WeWorked entry
            ww_enriched <- data.frame()
            
            for (i in 1:nrow(ww_data)) {
              ww_entry <- ww_data[i, ]
              staff_name <- ww_entry$fullName
              task_name <- ww_entry$task
              hours <- ww_entry$total_hours
              
              # Find this staff member's labor category
              staff_labor_category_match <- elongated_labor_cat_staff[elongated_labor_cat_staff$Staff_Name == staff_name, ]
              
              if (nrow(staff_labor_category_match) > 0) {
                # If staff has multiple labor_categories, use the first one
                staff_laborCat <- staff_labor_category_match$LaborCategory[1]
                
                # Map WeWorked task to our task numbers
                matching_task_number <- NA
                for (j in 1:nrow(planned_spending)) {
                  task_num <- planned_spending$Task_Number[j]
                  task_nm <- planned_spending$Task_Name[j]
                  
                  # Try multiple matching strategies
                  if (grepl(task_num, task_name, ignore.case = TRUE) ||
                      grepl(task_nm, task_name, ignore.case = TRUE) ||
                      grepl(task_name, task_nm, ignore.case = TRUE)) {
                    matching_task_number <- task_num
                    break
                  }
                }
                
                if (!is.na(matching_task_number)) {
                  # Find the rate for this labor-category-task combination
                  rate_match <- labor_cat_rates_db[
                    labor_cat_rates_db$labor_cat_name == staff_laborCat & 
                      labor_cat_rates_db$task_number == matching_task_number, 
                  ]
                  
                  if (nrow(rate_match) > 0) {
                    hourly_rate <- rate_match$hourly_rate[1]
                    loaded_rate <- hourly_rate * input$overhead_multiplier
                    actual_cost <- hours * loaded_rate
                    
                    ww_enriched <- rbind(ww_enriched, data.frame(
                      Staff_Name = staff_name,
                      LaborCategory = staff_laborCat,
                      Task_Number = matching_task_number,
                      Hours = hours,
                      Hourly_Rate = hourly_rate,
                      Loaded_Rate = loaded_rate,
                      Actual_Cost = actual_cost,
                      stringsAsFactors = FALSE
                    ))
                    
                    cat("Processed:", staff_name, "in", staff_laborCat, "for task", matching_task_number, 
                        "->", hours, "hrs @", loaded_rate, "= $", round(actual_cost, 2), "\n")
                  } else {
                    cat("No rate found for labor category", staff_laborCat, "task", matching_task_number, "\n")
                  }
                } else {
                  cat("No task match found for WeWorked task:", task_name, "\n")
                }
              } else {
                cat("No labor category found for staff member:", staff_name, "\n")
              }
            }
            
            # Aggregate by task number
            if (nrow(ww_enriched) > 0) {
              actual_by_task <- ww_enriched %>%
                group_by(Task_Number) %>%
                summarise(
                  Actual_Spending = sum(Actual_Cost, na.rm = TRUE),
                  .groups = 'drop'
                ) %>%
                as.data.frame()
              
              # Add task names
              for (i in 1:nrow(actual_by_task)) {
                task_num <- actual_by_task$Task_Number[i]
                task_info <- planned_spending[planned_spending$Task_Number == task_num, ]
                if (nrow(task_info) > 0) {
                  actual_spending <- rbind(actual_spending, data.frame(
                    Task_Number = task_num,
                    Task_Name = task_info$Task_Name[1],
                    Actual_Spending = actual_by_task$Actual_Spending[i],
                    stringsAsFactors = FALSE
                  ))
                }
              }
              
              cat("Final actual spending summary:\n")
              print(actual_spending)
            }
          }
        } else {
          cat("Missing labor category assignments or rates data for actual spending calculation\n")
        }
      }
      
      
      # Merge planned and actual spending - handle case where actual_spending is empty
      if (nrow(actual_spending) == 0) {
        if (nrow(planned_spending) == 0) {
          # Both are empty - return empty structure
          spending_summary <- data.frame(
            Task_Number = character(0),
            Task_Name = character(0),
            Planned_Spending = numeric(0),
            Actual_Spending = numeric(0),
            stringsAsFactors = FALSE
          )
        } else {
          # Only actual is empty - add zero actual spending
          spending_summary <- planned_spending
          spending_summary$Actual_Spending <- 0
        }
      } else {
        spending_summary <- merge(planned_spending, actual_spending, 
                                  by = c("Task_Number", "Task_Name"), all.x = TRUE)
        spending_summary$Actual_Spending[is.na(spending_summary$Actual_Spending)] <- 0
      }
      
      # Add budget data - fix the merge by using the correct column name
      task_budget_subset <- task_data[, c("Task_Number", "Toxcel_Labor_Budget")]
      names(task_budget_subset) <- c("Task_Number", "Task_Budget")
      
      spending_summary <- merge(spending_summary, task_budget_subset, 
                                by = "Task_Number", all.x = TRUE)
      
      return(spending_summary)
    })
    
    ## Update spending data when inputs change ----
    observe({
      spending_summary <- calculate_spending_data()
      task_spending_summary(spending_summary)
      
      if (nrow(spending_summary) > 0) {
        total_planned <- sum(spending_summary$Planned_Spending, na.rm = TRUE)
        total_actual <- sum(spending_summary$Actual_Spending, na.rm = TRUE)
        
        # Update status indicators
        runjs(paste0("
      $('#", ns("spending_status"), "').html('<i class=\"fa fa-check text-success\"></i> $", 
                     format(round(total_planned, 0), big.mark = ","), " planned, $", 
                     format(round(total_actual, 0), big.mark = ","), " actual');
      $('#", ns("spending_status"), "').css('background-color', '#d4edda');
    "))
      }
    })    
    
    
    ## Graph 1: Task Budget vs Actual Spending (Always Visible) ----
    output$task_spending_chart <- renderPlotly({
      req(task_spending_summary())
      
      spending_data <- task_spending_summary()
      
      if (nrow(spending_data) == 0) {
        return(plotly_empty("No spending data available"))
      }
      
      # Prepare data for plotting
      spending_data <- spending_data %>%
        mutate(
          Task_Label = paste0("Task ", Task_Number, ": ", Task_Name),
          Budget = ifelse(is.na(Task_Budget), 0, Task_Budget),
          Actual = Actual_Spending,
          # Convert Task_Number to numeric for proper sorting
          Task_Num_Numeric = as.numeric(Task_Number)
        ) %>%
        arrange(desc(Task_Num_Numeric))  # Sort descending to put Task 1 at top
      
      # Create factor with levels in reverse order for correct y-axis ordering
      spending_data$Task_Label <- factor(spending_data$Task_Label, 
                                         levels = spending_data$Task_Label)
      
      
      
      # Create the plot
      p <- plot_ly(data = spending_data) %>%
        add_bars(
          y = ~Task_Label,
          x = ~Budget,
          name = "Budget",
          marker = list(color = "#3498db"),
          orientation = "h",
          hovertemplate = paste0(
            "<b>%{y}</b><br>",
            "Budget: $%{x:,.0f}<br>",
            "<extra></extra>"
          ),
          customdata = ~Task_Number
        ) %>%
        add_bars(
          y = ~Task_Label,
          x = ~Actual,
          name = "Actual Spending",
          marker = list(color = "#e74c3c"),
          orientation = "h",
          hovertemplate = paste0(
            "<b>%{y}</b><br>",
            "Actual: $%{x:,.0f}<br>",
            "<extra></extra>"
          ),
          customdata = ~Task_Number
        ) %>%
        layout(
          title = "Task Budget vs Actual Spending",
          xaxis = list(title = "", tickformat = "$,.0f"),
          yaxis = list(title = ""),
          barmode = "group",
          hovermode = "closest",
          margin = list(l = 250),  # More space for task labels
          showlegend = TRUE
        ) %>%
        config(displayModeBar = FALSE) %>%
        event_register("plotly_click")  # Fix: Register the click event
      
      # Set source for click handling
      p$x$source <- "task_spending_bars"
      
      return(p)
    })
    
    ## Show modal with detailed graphs ------------------
    observeEvent(event_data("plotly_click", source = "task_spending_bars"), {
      click_data <- event_data("plotly_click", source = "task_spending_bars")
      
      if (!is.null(click_data)) {
        # Get task number from the clicked bar
        task_number <- click_data$customdata
        task_info <- task_spending_summary()[task_spending_summary()$Task_Number == task_number, ]
        
        if (nrow(task_info) > 0) {
          task_name <- task_info$Task_Name[1]
          
          showModal(modalDialog(
            title = paste0("Detailed Analysis: Task ", task_number, " - ", task_name),
            size = "l",
            
            tabsetPanel(
              id = ns("modal_tabs"),
              tabPanel("Summary",
                       br(),
                       plotlyOutput(ns("staff_spending_chart"), height = "400px")
              ),
              tabPanel("Details",
                       br(),
                       plotlyOutput(ns("cumulative_task_chart"), height = "300px"),
                       br(),
                       plotlyOutput(ns("cumulative_staff_chart"), height = "300px")
              )
            ),
            
            footer = modalButton("Close")
          ))
          
          # Store selected task for use in modal graphs
          selected_task_for_modal(task_number)
        }
      }
    })
    
    ### Graph 2: Staff Spending for Selected Task (Modal - Summary Tab) ----
    output$staff_spending_chart <- renderPlotly({
      req(selected_task_for_modal(), !is.null(selected_task_for_modal()), 
          staff_matrix_data(), input$overhead_multiplier)
      
      selected_task <- selected_task_for_modal()
      
      staff_data <- staff_matrix_data()
      
      ww_data <- weworked_data()
      
      # Get task column name
      task_col <- paste0("Hours_Task_", selected_task)
      
      # Check if task column exists
      if (!task_col %in% names(staff_data)) {
        return(plotly_empty("No data for selected task"))
      }
      
      # Build staff comparison data
      staff_comparison <- data.frame()
      
      for (i in 1:nrow(staff_data)) {
        staff_name <- staff_data$Staff_Name[i]
        planned_hours <- staff_data[i, task_col]
        
        # Skip if no planned hours
        # if (is.na(planned_hours) || planned_hours == 0) next
        
        # Calculate planned cost
        loaded_rate <- staff_data$Direct_Rate[i] * input$overhead_multiplier
        planned_cost <- planned_hours * loaded_rate
        
        # Calculate actual hours and cost using labor-category-based rates
        actual_hours <- 0
        actual_cost <- 0
        
        if (!is.null(ww_data) && nrow(ww_data) > 0) {
          # Get staff member's labor category
          labor_cat_staff_assignments <- laborCat_staff_data()
          staff_laborCat <- NA
          
          if (!is.null(labor_cat_staff_assignments) && nrow(labor_cat_staff_assignments) > 0) {
            # Create elongated staff assignments and find labor category
            elongated_staff <- labor_cat_staff_assignments %>%
              filter(!is.na(Staff_Members) & Staff_Members != "") %>%
              separate_rows(Staff_Members, sep = ";") %>%
              mutate(Staff_Name = trimws(Staff_Members)) %>%
              filter(Staff_Name != "") %>%
              select(Staff_Name, LaborCategory)
            
            staff_labor_category_match <- elongated_staff[elongated_staff$Staff_Name == staff_name, ]
            if (nrow(staff_labor_category_match) > 0) {
              staff_laborCat <- staff_labor_category_match$LaborCategory[1]  # Use first labor_category if multiple
            }
          }
          
          if (!is.na(staff_laborCat)) {
            # Find WeWorked entries for this staff and task
            task_info <- task_budget_data()[task_budget_data()$Task_Number == selected_task, ]
            if (nrow(task_info) > 0) {
              task_name <- task_info$Task_Name[1]
              
              # Match by staff name and task (flexible matching)
              ww_matches <- ww_data[
                ww_data$fullName == staff_name & 
                  (grepl(task_name, ww_data$task, ignore.case = TRUE) | 
                     grepl(selected_task, ww_data$task, ignore.case = TRUE)), 
              ]
              
              if (nrow(ww_matches) > 0) {
                actual_hours <- sum(ww_matches$total_hours, na.rm = TRUE)
                
                # Get labor-category-based rate for this task
                labor_cat_rates_db <- laborCat_rates_data()
                if (!is.null(labor_cat_rates_db) && nrow(labor_cat_rates_db) > 0) {
                  rate_match <- labor_cat_rates_db[
                    labor_cat_rates_db$labor_cat_name == staff_laborCat & 
                      labor_cat_rates_db$task_number == selected_task, 
                  ]
                  
                  if (nrow(rate_match) > 0) {
                    laborCat_based_rate <- rate_match$hourly_rate[1] * input$overhead_multiplier
                    actual_cost <- actual_hours * laborCat_based_rate
                  } else {
                    # Fallback to staff matrix rate if no labor_category rate found
                    actual_cost <- actual_hours * loaded_rate
                  }
                } else {
                  # Fallback to staff matrix rate if no labor_category rates available
                  actual_cost <- actual_hours * loaded_rate
                }
              }
            }
          }
        }
        
        staff_comparison <- rbind(staff_comparison, data.frame(
          Staff_Name = staff_name,
          Planned_Hours = planned_hours,
          Actual_Hours = actual_hours,
          Planned_Cost = planned_cost,
          Actual_Cost = actual_cost,
          stringsAsFactors = FALSE
        ))
      }
      
      if (nrow(staff_comparison) == 0) {
        return(plotly_empty("No staff data for selected task"))
      }
      
      # Order by actual spending (highest to lowest)
      staff_comparison <- staff_comparison %>%
        arrange(desc(Actual_Cost))
      
      # Create factor with levels ordered by actual spending
      staff_comparison$Staff_Name <- factor(staff_comparison$Staff_Name, 
                                            levels = staff_comparison$Staff_Name)
      
      
      # Create the plotly chart
      p <- plot_ly(staff_comparison, x = ~Staff_Name, y = ~Planned_Cost, 
                   type = 'bar', name = 'Planned', marker = list(color = '#3498db'),
                   hovertemplate = paste0(
                     "<b>%{x}</b><br>",
                     "Planned: $%{y:,.0f}<br>",
                     "<extra></extra>"
                   )) %>%
        add_trace(y = ~Actual_Cost, name = 'Actual', marker = list(color = '#e74c3c'),
                  hovertemplate = paste0(
                    "<b>%{x}</b><br>",
                    "Actual: $%{y:,.0f}<br>",
                    "<extra></extra>"
                  )) %>%
        layout(
          title = paste("Staff Spending Comparison - Task", selected_task),
          xaxis = list(title = "Staff Member", categoryorder = "array", 
                       categoryarray = levels(staff_comparison$Staff_Name)),
          yaxis = list(title = "Cost ($)", tickformat = "$,.0f"),
          barmode = 'group',
          showlegend = TRUE,
          margin = list(b = 100)  # Extra space for staff names on x-axis
        ) %>%
        config(displayModeBar = FALSE)
      
      return(p)
      
    })
    
    ### Graph 3: Cumulative Task Spending Over Time (Modal - Details Tab) ----
    output$cumulative_task_chart <- renderPlotly({
      # Implementation for Graph 3 - Cumulative spending timeline
      # This would show the 45-degree budget burn line vs actual cumulative spending
      return(plotly_empty("Cumulative task chart - to be implemented"))
    })
    
    ### Graph 4: Staff Cumulative Spending Over Time (Modal - Details Tab) ----
    output$cumulative_staff_chart <- renderPlotly({
      # Implementation for Graph 4 - Individual staff cumulative spending
      # This would show each staff member's cumulative spending over time
      return(plotly_empty("Staff cumulative chart - to be implemented"))
    })
    
    
    
    # Return data for use by other modules
    return(list(
      project_info = reactive({
        list(
          name = input$project_name,
          project_id_text = input$project_id,
          manager = input$project_manager,
          client = input$client_name,
          description = input$project_description,
          notes = if(is.null(input$project_notes)) "" else input$project_notes,
          total_value = input$total_dollar_value,
          start_date = input$period_of_performance[1],
          end_date = input$period_of_performance[2],
          overhead = input$overhead_multiplier
        )
      }),
      task_budget = task_budget_data,
      deliverables = deliverables_data,
      labor_cat_rates = laborCat_rates_data,
      labor_cat_hours = laborCat_hours_data,
      labor_cat_staff = reactive({
        staff_data <- laborCat_staff_data()
        if (is.null(staff_data) || nrow(staff_data) == 0) {
          return(data.frame(labor_cat_name = character(0), staff_names = character(0), stringsAsFactors = FALSE))
        }
        # Convert back to database format for external modules
        data.frame(
          labor_cat_name = staff_data$LaborCategory,
          staff_names = staff_data$Staff_Members,
          stringsAsFactors = FALSE
        )
      }),
      weworked_data = weworked_data, 
      spending_summary = task_spending_summary 
    ))
  })
}