#' pm_dashboard UI Function
#'
#' @description A shiny Module for Project Manager dashboard with staff assignment matrix
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
library(bslib)  # Added for tooltip functionality


setup_database <- function(db_path = "project_manager.db") {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Projects table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS projects (
      project_id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_name TEXT NOT NULL,
      client_name TEXT,
      project_manager TEXT,
      project_description TEXT,
      start_date DATE,
      end_date DATE,
      total_dollar_value REAL,
      overhead_multiplier REAL,
      created_timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Task budgets table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS task_budgets (
      task_id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER,
      task_number TEXT,
      task_name TEXT,
      start_date DATE,
      end_date DATE,
      labor_budget REAL,
      FOREIGN KEY (project_id) REFERENCES projects(project_id)
    )
  ")
  
  # Deliverables table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS deliverables (
      deliverable_id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER,
      deliverable_name TEXT,
      linked_task TEXT,
      due_date DATE,
      FOREIGN KEY (project_id) REFERENCES projects(project_id)
    )
  ")
  
  # Staff assignments table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS staff_assignments (
      assignment_id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER,
      staff_name TEXT,
      direct_rate REAL,
      FOREIGN KEY (project_id) REFERENCES projects(project_id)
    )
  ")
  
  # Task hour assignments table (normalized approach)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS task_hour_assignments (
      assignment_id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER,
      staff_name TEXT,
      task_number TEXT,
      task_name TEXT,
      hours REAL,
      FOREIGN KEY (project_id) REFERENCES projects(project_id)
    )
  ")
  
  dbDisconnect(con)
  return(db_path)
}

#' Save project data to database
#' @param project_data List containing all project information
#' @param db_path Path to database
save_project_to_db <- function(project_data, db_path = "project_manager.db") {
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # Start transaction
    dbBegin(con)
    
    # Check if project with same name already exists
    existing_project <- dbGetQuery(con, "
      SELECT project_id FROM projects WHERE project_name = ?
    ", params = list(project_data$project_info$name))
    
    if (nrow(existing_project) > 0) {
      # UPDATE existing project
      project_id <- existing_project$project_id[1]
      
      # Update project info
      dbExecute(con, "
        UPDATE projects SET 
          client_name = ?, project_manager = ?, project_description = ?,
          start_date = ?, end_date = ?, total_dollar_value = ?, 
          overhead_multiplier = ?, updated_timestamp = CURRENT_TIMESTAMP
        WHERE project_id = ?
      ", params = list(
        project_data$project_info$client,
        project_data$project_info$manager,
        project_data$project_info$description,
        as.character(project_data$project_info$start_date),
        as.character(project_data$project_info$end_date),
        project_data$project_info$total_value,
        project_data$project_info$overhead,
        project_id
      ))
      
      # Delete existing related data
      dbExecute(con, "DELETE FROM task_budgets WHERE project_id = ?", params = list(project_id))
      dbExecute(con, "DELETE FROM deliverables WHERE project_id = ?", params = list(project_id))
      dbExecute(con, "DELETE FROM staff_assignments WHERE project_id = ?", params = list(project_id))
      dbExecute(con, "DELETE FROM task_hour_assignments WHERE project_id = ?", params = list(project_id))
      
      action_message <- "updated"
      
    } else {
      # INSERT new project
      dbExecute(con, "
        INSERT INTO projects (
          project_name, client_name, project_manager, project_description,
          start_date, end_date, total_dollar_value, overhead_multiplier
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(
        project_data$project_info$name,
        project_data$project_info$client,
        project_data$project_info$manager,
        project_data$project_info$description,
        as.character(project_data$project_info$start_date),
        as.character(project_data$project_info$end_date),
        project_data$project_info$total_value,
        project_data$project_info$overhead
      ))
      
      # Get the project_id
      project_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
      action_message <- "saved"
    }
    
    # Insert task budgets (same for both update and insert)
    if (!is.null(project_data$task_budget) && nrow(project_data$task_budget) > 0) {
      for (i in 1:nrow(project_data$task_budget)) {
        task <- project_data$task_budget[i, ]
        dbExecute(con, "
          INSERT INTO task_budgets (
            project_id, task_number, task_name, start_date, end_date, labor_budget
          ) VALUES (?, ?, ?, ?, ?, ?)
        ", params = list(
          project_id,
          task$Task_Number,
          task$Task_Name,
          task$Start_Date,
          task$End_Date,
          task$Toxcel_Labor_Budget
        ))
      }
    }
    
    # Insert deliverables (same for both update and insert)
    if (!is.null(project_data$deliverables) && nrow(project_data$deliverables) > 0) {
      for (i in 1:nrow(project_data$deliverables)) {
        deliv <- project_data$deliverables[i, ]
        dbExecute(con, "
          INSERT INTO deliverables (
            project_id, deliverable_name, linked_task, due_date
          ) VALUES (?, ?, ?, ?)
        ", params = list(
          project_id,
          deliv$Deliverable_Name,
          deliv$Linked_Task,
          as.character(deliv$Due_Date)
        ))
      }
    }
    
    # Insert staff assignments (same for both update and insert)
    if (!is.null(project_data$staff_assignments) && nrow(project_data$staff_assignments) > 0) {
      for (i in 1:nrow(project_data$staff_assignments)) {
        staff <- project_data$staff_assignments[i, ]
        
        # Insert basic staff info
        dbExecute(con, "
          INSERT INTO staff_assignments (
            project_id, staff_name, direct_rate
          ) VALUES (?, ?, ?)
        ", params = list(
          project_id,
          staff$Staff_Name,
          staff$Direct_Rate
        ))
        
        # Insert task hour assignments separately
        task_cols <- names(staff)[grepl("^Hours_", names(staff))]
        if (length(task_cols) > 0) {
          for (task_col in task_cols) {
            hours <- staff[[task_col]]
            if (!is.na(hours) && hours > 0) {
              # Extract task name from column name
              task_name <- gsub("^Hours_", "", task_col)
              task_name <- gsub("\\.", " ", task_name)
              
              # Find corresponding task number from task_budget
              task_info <- project_data$task_budget[project_data$task_budget$Task_Name == task_name, ]
              task_number <- if (nrow(task_info) > 0) task_info$Task_Number[1] else ""
              
              dbExecute(con, "
                INSERT INTO task_hour_assignments (
                  project_id, staff_name, task_number, task_name, hours
                ) VALUES (?, ?, ?, ?, ?)
              ", params = list(
                project_id,
                staff$Staff_Name,
                task_number,
                task_name,
                hours
              ))
            }
          }
        }
      }
    }
    
    # Commit transaction
    dbCommit(con)
    dbDisconnect(con)
    
    return(list(
      success = TRUE, 
      project_id = project_id, 
      message = paste0("Project ", action_message, " successfully!"),
      action = action_message
    ))
    
  }, error = function(e) {
    if (exists("con")) {
      dbRollback(con)
      dbDisconnect(con)
    }
    return(list(success = FALSE, message = paste("Error saving project:", e$message)))
  })
}

#' Load project from database
#' @param project_id Project ID to load
#' @param db_path Path to database
load_project_from_db <- function(project_id, db_path = "project_manager.db") {
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # Load project info
    project <- dbGetQuery(con, "SELECT * FROM projects WHERE project_id = ?", 
                          params = list(project_id))
    
    # Load task budgets
    tasks <- dbGetQuery(con, "SELECT * FROM task_budgets WHERE project_id = ?", 
                        params = list(project_id))
    
    # Load deliverables
    deliverables <- dbGetQuery(con, "SELECT * FROM deliverables WHERE project_id = ?", 
                               params = list(project_id))
    
    # Load staff assignments
    staff <- dbGetQuery(con, "SELECT * FROM staff_assignments WHERE project_id = ?", 
                        params = list(project_id))
    
    # Load task hour assignments
    task_hours <- dbGetQuery(con, "SELECT * FROM task_hour_assignments WHERE project_id = ?", 
                             params = list(project_id))
    
    dbDisconnect(con)
    
    return(list(
      success = TRUE,
      project = project,
      tasks = tasks,
      deliverables = deliverables,
      staff = staff,
      task_hours = task_hours
    ))
    
  }, error = function(e) {
    if (exists("con")) dbDisconnect(con)
    return(list(success = FALSE, message = paste("Error loading project:", e$message)))
  })
}


#' Get list of all projects
get_project_list <- function(db_path = "project_manager.db") {
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), db_path)
    projects <- dbGetQuery(con, "
      SELECT project_id, project_name, client_name, project_manager, 
             created_timestamp, updated_timestamp,
             total_dollar_value,
             start_date, end_date
      FROM projects 
      ORDER BY updated_timestamp DESC
    ")
    dbDisconnect(con)
    return(projects)
  }, error = function(e) {
    if (exists("con")) dbDisconnect(con)
    return(data.frame(
      project_id = integer(0),
      project_name = character(0),
      client_name = character(0),
      project_manager = character(0),
      created_timestamp = character(0),
      updated_timestamp = character(0),
      total_dollar_value = numeric(0),
      start_date = character(0),
      end_date = character(0)
    ))
  })
}





mod_pm_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Project Manager Dashboard"),
      
      fluidRow(
        column(12,
               wellPanel(
                 h3("Load Existing Project"),
                 fluidRow(
                   column(8,
                          selectInput(ns("project_to_load"), 
                                      "Select Project to Load (data will load automatically):",
                                      choices = list("Select a project..." = ""),
                                      width = "100%")
                   ),
                   column(2,
                          actionButton(ns("refresh_projects"), "Refresh List", 
                                       class = "btn-secondary", icon = icon("refresh"))
                   ),
                   column(2,
                          actionButton(ns("clear_form"), "Clear Form", 
                                       class = "btn-warning", icon = icon("eraser"))
                   )
                 )
               )
        )
      ),
      
      # Project Information Section
      fluidRow(
        column(12,
               wellPanel(
                 h3("Project Information"),
                 fluidRow(
                   column(6,
                          textInput(ns("project_name"), "Project Name:", value = "", width = "100%"),
                          textInput(ns("client_name"), "Client Name:", value = "", width = "100%"),      # FIX 1: MOVED UP
                          textInput(ns("project_manager"), "Project Manager:", value = "", width = "100%"), # FIX 1: MOVED DOWN
                          textAreaInput(ns("project_description"), "Project Description:", 
                                        value = "", width = "100%", height = "80px")
                   ),
                   column(6,
                          div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                              tags$label("Period of Performance Start:", style = "margin-right: 10px; min-width: 200px;"),
                              dateInput(ns("start_date"), "", value = Sys.Date(), width = "150px")
                          ),
                          div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                              tags$label("Period of Performance End:", style = "margin-right: 10px; min-width: 200px;"),
                              dateInput(ns("end_date"), "", value = Sys.Date() + 365, width = "150px")
                          ),
                          div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                              tags$label("Total Dollar Value:", style = "margin-right: 10px; min-width: 200px;"),
                              numericInput(ns("total_dollar_value"), "", 
                                           value = 0, min = 0, step = 1000, width = "130px")
                          )
                   )
                 )
               )
        )
      ),
      
      # Task Budget Matrix Section
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
      
      # Define Deliverables Section
      fluidRow(
        column(12,
               wellPanel(
                 h3("Define Deliverables"),
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
      
      # Staff Assignment Matrix Section
      fluidRow(
        column(12,
               wellPanel(
                 h3("Staff Task Assignment Matrix"),
                 p("Assign staff members to tasks and specify hours. Only tasks (not deliverables) appear as columns."),
                 
                 # FIX 3: Overhead multiplier with tooltip
                 fluidRow(
                   column(6,
                          numericInput(ns("overhead_multiplier"), 
                                       label = div(
                                         "Overhead Multiplier:",
                                         span(
                                           icon("info-circle"),
                                           style = "margin-left: 5px; color: #0d6efd; cursor: help;",
                                           title = "This multiplier is applied to Direct Rates to calculate Loaded Rates."
                                         )
                                       ),
                                       value = 1.0, min = 0.1, max = 5, step = 0.1, width = "200px")
                   ),
                   column(6,
                          # Removed the explanatory paragraph - now in tooltip
                   )
                 ),
                 
                 br(),
                 
                 # Control buttons
                 fluidRow(
                   column(6,
                          actionButton(ns("add_staff_row"), "Add Staff Member", 
                                       class = "btn-primary", icon = icon("plus"))
                   ),
                   column(6,
                          # Placeholder for symmetry
                          div()
                   )
                 ),
                 
                 br(),
                 
                 # Matrix table
                 div(style = "width: 100%;",
                     rhandsontable::rHandsontableOutput(ns("staff_matrix"))
                 ),
                 
               )
        )
      ),
      
      fluidRow(
        column(12,
               hr(),
               h5("Save Project"),
               p("Save all project data to database for future reference."),
               actionButton(ns("save_project"), "Save Project to Database", 
                            class = "btn-success", icon = icon("save"),
                            style = "font-size: 16px; padding: 10px 20px;")
        )
      )
    )
  )
}

#' pm_dashboard Server Functions
#'
#' @noRd 
mod_pm_dashboard_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Staff options
    staff_options <- c("", "Alice Johnson", "Bob Smith", "Carol Davis", 
                       "David Wilson", "Emma Brown", "Frank Miller", "Grace Lee",
                       "Henry Chen", "Isabel Rodriguez", "Jack Thompson")
    
    # Load project functionality
    project_list <- reactiveVal(data.frame())
    
    # Load available projects on module start
    observe({
      projects <- get_project_list()
      project_list(projects)
      
      if (nrow(projects) > 0) {
        choices <- setNames(projects$project_id, 
                            paste0(projects$project_name, " (", projects$client_name, ") - ", 
                                   format(as.Date(projects$created_timestamp), "%Y-%m-%d")))
        choices <- c("Select a project..." = "", choices)
      } else {
        choices <- list("No saved projects found" = "")
      }
      
      updateSelectInput(session, "project_to_load", choices = choices)
    })
    
    # Refresh project list
    observeEvent(input$refresh_projects, {
      projects <- get_project_list()
      project_list(projects)
      
      if (nrow(projects) > 0) {
        choices <- setNames(projects$project_id, 
                            paste0(projects$project_name, " (", projects$client_name, ") - ", 
                                   format(as.Date(projects$created_timestamp), "%Y-%m-%d")))
        choices <- c("Select a project..." = "", choices)
      } else {
        choices <- list("No saved projects found" = "")
      }
      
      updateSelectInput(session, "project_to_load", choices = choices)
      showNotification("Project list refreshed", type = "message")
    })
    
    # Load selected project automatically when dropdown changes
    observeEvent(input$project_to_load, {
      req(input$project_to_load)
      
      if (input$project_to_load == "") {
        return()  # Do nothing if "Select a project..." is chosen
      }
      
      showNotification("Loading project...", type = "message", duration = 2)
      
      # Load project data
      result <- load_project_from_db(as.numeric(input$project_to_load))
      
      if (result$success) {
        # Update project information inputs
        project <- result$project
        updateTextInput(session, "project_name", value = project$project_name)
        updateTextInput(session, "client_name", value = project$client_name)
        updateTextInput(session, "project_manager", value = project$project_manager)
        updateTextAreaInput(session, "project_description", value = project$project_description)
        updateDateInput(session, "start_date", value = as.Date(project$start_date))
        updateDateInput(session, "end_date", value = as.Date(project$end_date))
        updateNumericInput(session, "total_dollar_value", value = project$total_dollar_value)
        updateNumericInput(session, "overhead_multiplier", value = project$overhead_multiplier)
        
        # Update task budget data
        if (nrow(result$tasks) > 0) {
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
          # Reset to empty if no tasks
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
        
        # Update deliverables data
        if (nrow(result$deliverables) > 0) {
          deliverables_df <- data.frame(
            Deliverable_Name = result$deliverables$deliverable_name,
            Linked_Task = result$deliverables$linked_task,
            Due_Date = as.Date(result$deliverables$due_date),
            stringsAsFactors = FALSE
          )
          deliverables_data(deliverables_df)
        } else {
          # Reset to empty if no deliverables
          deliverables_data(data.frame(
            Deliverable_Name = character(0),
            Linked_Task = character(0),
            Due_Date = as.Date(character(0)),
            stringsAsFactors = FALSE
          ))
        }
        
        # Update staff assignments (this is more complex due to the matrix structure)
        if (nrow(result$staff) > 0) {
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
        
        showNotification(
          paste0("Project '", project$project_name, "' loaded successfully!"), 
          type = "message", 
          duration = 3
        )
        
      } else {
        showNotification(paste0("Error loading project: ", result$message), 
                         type = "error", duration = 10)
      }
    })
    
    # Add clear form functionality
    observeEvent(input$clear_form, {
      # Reset all inputs to defaults
      updateTextInput(session, "project_name", value = "")
      updateTextInput(session, "client_name", value = "")
      updateTextInput(session, "project_manager", value = "")
      updateTextAreaInput(session, "project_description", value = "")
      updateDateInput(session, "start_date", value = Sys.Date())
      updateDateInput(session, "end_date", value = Sys.Date() + 365)
      updateNumericInput(session, "total_dollar_value", value = 0)
      updateNumericInput(session, "overhead_multiplier", value = 1.0)
      
      # Reset dropdown
      updateSelectInput(session, "project_to_load", selected = "")
      
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
    })
    
    # Reactive values for storing data
    task_budget_data <- reactiveVal()
    
    deliverables_data <- reactiveVal(data.frame(
      Deliverable_Name = character(0),
      Linked_Task = character(0),
      Due_Date = as.Date(character(0)),
      stringsAsFactors = FALSE
    ))
    
    # Staff matrix data
    staff_matrix_data <- reactiveVal()
    
    # Initialize task budget with one empty row
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
    
    # Initialize staff matrix with one empty row
    observe({
      if (is.null(staff_matrix_data())) {
        initial_data <- data.frame(
          Staff_Name = "",
          Direct_Rate = 0,
          stringsAsFactors = FALSE
        )
        staff_matrix_data(initial_data)
      }
    })
    
    # Project summary
    output$project_summary <- renderText({
      req(input$project_name, input$client_name)
      
      duration <- as.numeric(input$end_date - input$start_date)
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
    
    # Helper function to get next task number
    get_next_task_number <- function(current_data) {
      if (is.null(current_data) || nrow(current_data) == 0) {
        return("1")
      }
      
      # Extract main task numbers (ignore subtasks)
      task_numbers <- current_data$Task_Number
      main_numbers <- as.numeric(gsub("\\..*", "", task_numbers))
      main_numbers <- main_numbers[!is.na(main_numbers)]
      
      if (length(main_numbers) == 0) {
        return("1")
      }
      
      next_main <- max(main_numbers) + 1
      return(as.character(next_main))
    }
    
    # Add task
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
    
    
    
    # Sort tasks by task number
    observeEvent(input$sort_tasks, {
      current_data <- task_budget_data()
      if (is.null(current_data) || nrow(current_data) <= 1) {
        showNotification("No tasks to sort", type = "warning")
        return()
      }
      
      # Custom sort function for hierarchical task numbers
      sort_task_numbers <- function(task_nums) {
        # Split into main and sub components
        split_nums <- lapply(task_nums, function(x) {
          parts <- strsplit(as.character(x), "\\.")[[1]]
          as.numeric(parts)
        })
        
        # Sort by main task, then subtask
        order(sapply(split_nums, function(x) x[1]), 
              sapply(split_nums, function(x) ifelse(length(x) > 1, x[2], 0)))
      }
      
      sorted_indices <- sort_task_numbers(current_data$Task_Number)
      sorted_data <- current_data[sorted_indices, ]
      
      task_budget_data(sorted_data)
      showNotification("Tasks sorted hierarchically", type = "message")
    })
    
    # Render task budget table
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
    
    # Update task budget data when table is edited
    observeEvent(input$task_budget_table, {
      if (!is.null(input$task_budget_table)) {
        updated_data <- rhandsontable::hot_to_r(input$task_budget_table)
        task_budget_data(updated_data)
      }
    })
    
    # Update linked task choices for deliverables
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
    
    # Add deliverable
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
    
    # Remove selected deliverable
    observeEvent(input$remove_deliverable, {
      req(input$deliverables_table_rows_selected)
      
      current_deliverables <- deliverables_data()
      if (nrow(current_deliverables) > 0) {
        selected_rows <- input$deliverables_table_rows_selected
        updated_deliverables <- current_deliverables[-selected_rows, , drop = FALSE]
        deliverables_data(updated_deliverables)
      }
    })
    
    # FIX 2: Display deliverables table with no controls - only render if deliverables exist
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
                      paging = FALSE,     # FIX 2: ADDED - removes pagination
                      searching = FALSE,  # FIX 2: ADDED - removes search box
                      info = FALSE,       # FIX 2: ADDED - removes "Showing X entries" text
                      scrollX = TRUE
                    ))
    })
    
    # Update staff matrix columns when tasks change
    observe({
      tasks <- task_budget_data()
      current_matrix <- staff_matrix_data()
      
      if (is.null(current_matrix) || is.null(tasks)) return()
      
      # Get current task names and numbers from task budget table
      task_data <- task_budget_data()
      if (!is.null(task_data) && nrow(task_data) > 0) {
        valid_tasks <- task_data[!is.na(task_data$Task_Name) & task_data$Task_Name != "", ]
        if (nrow(valid_tasks) > 0) {
          task_names <- valid_tasks$Task_Name
          task_numbers <- valid_tasks$Task_Number
        } else {
          task_names <- character(0)
          task_numbers <- numeric(0)
        }
      } else {
        task_names <- character(0)
        task_numbers <- numeric(0)
      }
      
      # Create column names for tasks
      task_cols <- paste0("Hours_", make.names(task_names))
      
      # Preserve existing data structure
      base_cols <- c("Staff_Name", "Direct_Rate")
      all_needed_cols <- c(base_cols, task_cols)
      
      # If columns have changed, update the data frame
      current_cols <- names(current_matrix)
      if (!identical(current_cols, all_needed_cols)) {
        new_matrix <- current_matrix[, base_cols, drop = FALSE]
        
        # Add task columns
        for (task_col in task_cols) {
          if (task_col %in% current_cols) {
            # Preserve existing data
            new_matrix[[task_col]] <- current_matrix[[task_col]]
          } else {
            # Add new column with default values
            new_matrix[[task_col]] <- 0
          }
        }
        
        staff_matrix_data(new_matrix)
      }
    })
    
    # Add staff row
    observeEvent(input$add_staff_row, {
      current_matrix <- staff_matrix_data()
      if (is.null(current_matrix)) return()
      
      # Create new row with same structure
      new_row <- current_matrix[1, , drop = FALSE]
      new_row[1, ] <- NA
      new_row$Staff_Name <- ""
      new_row$Direct_Rate <- 0
      
      # Set task hours to 0
      task_cols <- names(current_matrix)[grepl("^Hours_", names(current_matrix))]
      if (length(task_cols) > 0) {
        new_row[, task_cols] <- 0
      }
      
      # Bind new row
      updated_matrix <- rbind(current_matrix, new_row)
      staff_matrix_data(updated_matrix)
    })
    
    
    
    # Render the staff assignment matrix
    output$staff_matrix <- rhandsontable::renderRHandsontable({
      req(staff_matrix_data(), input$overhead_multiplier)
      
      matrix_data <- staff_matrix_data()
      
      if (nrow(matrix_data) == 0 || ncol(matrix_data) < 2) {
        return(NULL)
      }
      
      # Add calculated columns in the correct order
      enhanced_data <- matrix_data
      
      # Insert Loaded Rate right after Direct_Rate
      direct_rate_col <- which(names(enhanced_data) == "Direct_Rate")
      
      # Create new data frame with correct column order
      new_data <- enhanced_data[, 1:direct_rate_col, drop = FALSE]
      new_data$Loaded_Rate <- enhanced_data$Direct_Rate * input$overhead_multiplier
      
      # Add remaining columns (task hours)
      if (ncol(enhanced_data) > direct_rate_col) {
        remaining_cols <- enhanced_data[, (direct_rate_col + 1):ncol(enhanced_data), drop = FALSE]
        new_data <- cbind(new_data, remaining_cols)
      }
      
      # Get task columns
      task_cols <- names(new_data)[grepl("^Hours_", names(new_data))]
      
      # Add FTE and Total Cost at the end
      if (length(task_cols) > 0) {
        total_hours <- rowSums(new_data[, task_cols, drop = FALSE], na.rm = TRUE)
        new_data$FTE <- round(total_hours / 2000, 2)
        new_data$Total_Cost <- new_data$Loaded_Rate * total_hours
      } else {
        new_data$FTE <- 0
        new_data$Total_Cost <- 0
      }
      
      enhanced_data <- new_data
      
      # Create the base table
      hot <- rhandsontable::rhandsontable(enhanced_data, rowHeaders = FALSE, height = 400) %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
        rhandsontable::hot_cols(columnSorting = FALSE)
      
      # Configure columns in order: Staff Name, Direct Rate, Loaded Rate, Tasks..., FTE, Total Cost
      hot <- hot %>%
        rhandsontable::hot_col("Staff_Name", 
                               type = "dropdown", 
                               source = staff_options,
                               strict = FALSE,
                               allowInvalid = FALSE) %>%
        rhandsontable::hot_col("Direct_Rate", 
                               type = "numeric", 
                               format = "$0,0.00",
                               allowInvalid = FALSE) %>%
        rhandsontable::hot_col("Loaded_Rate", 
                               type = "numeric", 
                               format = "$0,0.00",
                               readOnly = TRUE) %>%
        rhandsontable::hot_col("FTE", 
                               type = "numeric", 
                               format = "0.00",
                               readOnly = TRUE) %>%
        rhandsontable::hot_col("Total_Cost", 
                               type = "numeric", 
                               format = "$0,0.00",
                               readOnly = TRUE)
      
      # Configure task hour columns with comma formatting and no decimals
      for (col in task_cols) {
        hot <- hot %>%
          rhandsontable::hot_col(col, 
                                 type = "numeric", 
                                 format = "0,0",
                                 allowInvalid = FALSE)
      }
      
      # Set column headers
      col_names <- names(enhanced_data)
      display_names <- col_names
      
      # Update task column headers to show "Task [number]"
      task_data <- task_budget_data()
      if (!is.null(task_data) && nrow(task_data) > 0) {
        for (i in seq_along(display_names)) {
          if (grepl("^Hours_", display_names[i])) {
            # Extract task name from column name
            task_name <- gsub("^Hours_", "", display_names[i])
            task_name <- gsub("\\.", " ", task_name)
            
            # Find matching task number
            matching_task <- task_data[task_data$Task_Name == task_name, ]
            if (nrow(matching_task) > 0) {
              display_names[i] <- paste0("Task ", matching_task$Task_Number[1])
            } else {
              display_names[i] <- task_name
            }
          }
        }
      }
      
      # Set friendly column names for other columns
      display_names[display_names == "Direct_Rate"] <- "Direct Rate"
      display_names[display_names == "Loaded_Rate"] <- "Loaded Rate"
      display_names[display_names == "Total_Cost"] <- "Total Cost"
      display_names[display_names == "Staff_Name"] <- "Staff Name"
      
      hot %>% rhandsontable::hot_cols(colHeaders = display_names)
    })
    
    # Update matrix data when table is edited (only save editable columns)
    observeEvent(input$staff_matrix, {
      if (!is.null(input$staff_matrix)) {
        full_data <- rhandsontable::hot_to_r(input$staff_matrix)
        
        # Extract only the base columns that users can edit
        base_cols <- c("Staff_Name", "Direct_Rate")
        task_cols <- names(full_data)[grepl("^Hours_", names(full_data))]
        editable_cols <- c(base_cols, task_cols)
        
        # Save only the editable data
        editable_data <- full_data[, editable_cols, drop = FALSE]
        staff_matrix_data(editable_data)
      }
    })
    
    # Initialize database on module load
    observe({
      setup_database()
    })
    
    
    observeEvent(input$save_project, {
      # Validate required fields
      if (is.null(input$project_name) || input$project_name == "") {
        showNotification("Please enter a project name before saving.", type = "error")
        return()
      }
      
      if (is.null(input$client_name) || input$client_name == "") {
        showNotification("Please enter a client name before saving.", type = "error")
        return()
      }
      
      # Show progress
      showNotification("Saving project...", type = "message", duration = 2)
      
      # Collect all data
      project_data <- list(
        project_info = list(
          name = input$project_name,
          client = input$client_name,
          manager = input$project_manager,
          description = input$project_description,
          start_date = input$start_date,
          end_date = input$end_date,
          total_value = input$total_dollar_value,
          overhead = input$overhead_multiplier
        ),
        task_budget = task_budget_data(),
        deliverables = deliverables_data(),
        staff_assignments = staff_matrix_data()
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
    
    
    
    
    
    
    # Return data for use by other modules
    return(list(
      project_info = reactive({
        list(
          name = input$project_name,
          manager = input$project_manager,
          client = input$client_name,
          description = input$project_description,
          total_value = input$total_dollar_value,
          start_date = input$start_date,
          end_date = input$end_date,
          overhead = input$overhead_multiplier
        )
      }),
      task_budget = task_budget_data,
      deliverables = deliverables_data,
      staff_assignments = staff_matrix_data
    ))
  })
}