# Libraries ----
library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(tidyr)
library(shinyBS)

library(dplyr)
library(shinyjs)
library(bslib)  # Added for tooltip functionality

library(DBI)
library(RSQLite)  
library(jsonlite)

library(httr)

library(scales)
library(ggplot2)
library(ggiraph)
library(plotly)


# Database ----

setup_database <- function(db_path = "project_manager.db") {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Projects table ----
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS projects (
      project_id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_name TEXT NOT NULL,
      project_id_text TEXT,
      client_name TEXT,
      project_manager TEXT,
      project_description TEXT,
      project_notes TEXT,
      start_date DATE,
      end_date DATE,
      total_dollar_value REAL,
      overhead_multiplier REAL,
      created_timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Task budgets table ----
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
  
  # Deliverables table ----
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
  
#   # Roles table ----
#   dbExecute(con, "
#   CREATE TABLE IF NOT EXISTS labor_categories (
#     labor_cat_id INTEGER PRIMARY KEY AUTOINCREMENT,
#     project_id INTEGER,
#     labor_cat_name TEXT NOT NULL,
#     labor_cat_order INTEGER DEFAULT 1,
#     FOREIGN KEY (project_id) REFERENCES projects(project_id)
#   )
# ")
#   
#   # Role rates table ----
#   dbExecute(con, "
#   CREATE TABLE IF NOT EXISTS labor_cat_rates (
#     rate_id INTEGER PRIMARY KEY AUTOINCREMENT,
#     project_id INTEGER,
#     labor_cat_name TEXT,
#     task_number TEXT,
#     hourly_rate REAL DEFAULT 0,
#     FOREIGN KEY (project_id) REFERENCES projects(project_id)
#   )
# ")
#   
#   # Role hours table  ----
#   dbExecute(con, "
#   CREATE TABLE IF NOT EXISTS labor_cat_hours (
#     hours_id INTEGER PRIMARY KEY AUTOINCREMENT,
#     project_id INTEGER,
#     labor_cat_name TEXT,
#     task_number TEXT,
#     planned_hours REAL DEFAULT 0,
#     FOREIGN KEY (project_id) REFERENCES projects(project_id)
#   )
# ")
#   
#   # Role staff assignments table ----
#   dbExecute(con, "
#   CREATE TABLE IF NOT EXISTS labor_cat_staff (
#     role_staff_id INTEGER PRIMARY KEY AUTOINCREMENT,
#     project_id INTEGER,
#     labor_cat_name TEXT,
#     staff_names TEXT, -- semicolon-separated list of staff names
#     FOREIGN KEY (project_id) REFERENCES projects(project_id)
#   )
# ")
  
  # Labor Categories table (renamed from roles) ----
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS labor_categories (
    labor_cat_id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER,
    labor_cat_name TEXT NOT NULL,
    labor_cat_order INTEGER DEFAULT 1,
    FOREIGN KEY (project_id) REFERENCES projects(project_id)
  )
")
  
  # Labor category rates table (renamed from role_rates) ----
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS labor_cat_rates (
    rate_id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER,
    labor_cat_name TEXT,
    task_number TEXT,
    hourly_rate REAL DEFAULT 0,
    FOREIGN KEY (project_id) REFERENCES projects(project_id)
  )
")
  
  # Labor category hours table (renamed from role_hours) ----
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS labor_cat_hours (
    hours_id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER,
    labor_cat_name TEXT,
    task_number TEXT,
    planned_hours REAL DEFAULT 0,
    FOREIGN KEY (project_id) REFERENCES projects(project_id)
  )
")
  
  # Labor category staff assignments table (renamed from role_staff) ----
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS labor_cat_staff (
    labor_cat_staff_id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER,
    labor_cat_name TEXT,
    staff_names TEXT, -- semicolon-separated list of staff names
    FOREIGN KEY (project_id) REFERENCES projects(project_id)
  )
")
  
  dbDisconnect(con)
  return(db_path)
  
}


#' Save project data to database
#' @param project_data List containing all project information
#' @param db_path Path to database
#' 
save_project_to_db <- function(project_data, db_path = "project_manager.db") {  
  tryCatch({
    cat("=== SAVE PROJECT DEBUG START ===\n")
    cat("Project data structure:\n")
    str(project_data$project_info)
    
    cat("\n=== PARAMETER ANALYSIS ===\n")
    info <- project_data$project_info
    param_names <- names(info)
    for (name in param_names) {
      value <- info[[name]]
      cat(sprintf("Field '%s': class=%s, length=%d, value=%s\n", 
                  name, paste(class(value), collapse=","), length(value), 
                  paste(as.character(value), collapse=",")))
    }
    
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
      
      cat("\n=== UPDATE OPERATION DEBUG ===\n")
      cat("Project ID:", project_id, "\n")
      
      # Create parameter list and debug each one
      update_params <- list(
        project_data$project_info$project_id_text,
        project_data$project_info$client,
        project_data$project_info$manager,
        project_data$project_info$description,
        project_data$project_info$notes,
        as.character(project_data$project_info$start_date),
        as.character(project_data$project_info$end_date),
        project_data$project_info$total_value,
        project_data$project_info$overhead,
        project_id
      )
      
      cat("UPDATE Parameters debugging:\n")
      param_labels <- c("project_id_text", "client", "manager", "description", "notes", 
                        "start_date", "end_date", "total_value", "overhead", "project_id")
      
      for (i in 1:length(update_params)) {
        param <- update_params[[i]]
        cat(sprintf("Param %d (%s): class=%s, length=%d, value=%s\n", 
                    i, param_labels[i], paste(class(param), collapse=","), 
                    length(param), paste(as.character(param), collapse=",")))
        
        if (length(param) != 1) {
          cat("*** ERROR: Parameter", i, "has length", length(param), "instead of 1! ***\n")
        }
      }
      
      # Project info ----
      dbExecute(con, "
        UPDATE projects SET 
          project_id_text = ?, client_name = ?, project_manager = ?, project_description = ?, project_notes = ?,
          start_date = ?, end_date = ?, total_dollar_value = ?, 
          overhead_multiplier = ?, updated_timestamp = CURRENT_TIMESTAMP
        WHERE project_id = ?
       ", params = update_params
      )
      
      # Delete existing related data
      dbExecute(con, "DELETE FROM task_budgets WHERE project_id = ?", params = list(project_id))
      dbExecute(con, "DELETE FROM deliverables WHERE project_id = ?", params = list(project_id))
      dbExecute(con, "DELETE FROM labor_cat_rates WHERE project_id = ?", params = list(project_id))
      dbExecute(con, "DELETE FROM labor_cat_hours WHERE project_id = ?", params = list(project_id))
      dbExecute(con, "DELETE FROM labor_cat_staff WHERE project_id = ?", params = list(project_id))
      
      action_message <- "updated"
      
    } else {
      # INSERT new project ----
      cat("\n=== INSERT OPERATION DEBUG ===\n")
      
      # Create parameter list and debug each one
      insert_params <- list(
        project_data$project_info$name,
        project_data$project_info$project_id_text,
        project_data$project_info$client,
        project_data$project_info$manager,
        project_data$project_info$description,
        project_data$project_info$notes,
        as.character(project_data$project_info$start_date),
        as.character(project_data$project_info$end_date),
        project_data$project_info$total_value,
        project_data$project_info$overhead
      )
      
      cat("INSERT Parameters debugging:\n")
      param_labels <- c("name", "project_id_text", "client", "manager", "description", "notes", 
                        "start_date", "end_date", "total_value", "overhead")
      
      for (i in 1:length(insert_params)) {
        param <- insert_params[[i]]
        cat(sprintf("Param %d (%s): class=%s, length=%d, value=%s\n", 
                    i, param_labels[i], paste(class(param), collapse=","), 
                    length(param), paste(as.character(param), collapse=",")))
        
        if (length(param) != 1) {
          cat("*** ERROR: Parameter", i, "has length", length(param), "instead of 1! ***\n")
        }
      }
      
      dbExecute(con, "
        INSERT INTO projects (
          project_name, project_id_text, client_name, project_manager, project_description, project_notes,
          start_date, end_date, total_dollar_value, overhead_multiplier
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = insert_params
      )
      
      # Get the project_id
      project_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
      action_message <- "saved"
    }
    
    # Task budgets (same for both update and insert) ----
    cat("\n=== TASK BUDGETS DEBUG ===\n")
    if (!is.null(project_data$task_budget) && nrow(project_data$task_budget) > 0) {
      cat("Task budget data structure:\n")
      str(project_data$task_budget)
      
      for (i in 1:nrow(project_data$task_budget)) {
        task <- project_data$task_budget[i, ]
        
        cat(sprintf("\nTask %d parameters:\n", i))
        task_params <- list(
          project_id,
          task$Task_Number,
          task$Task_Name,
          task$Start_Date,
          task$End_Date,
          task$Toxcel_Labor_Budget
        )
        
        task_labels <- c("project_id", "Task_Number", "Task_Name", "Start_Date", "End_Date", "Toxcel_Labor_Budget")
        for (j in 1:length(task_params)) {
          param <- task_params[[j]]
          cat(sprintf("  Task Param %d (%s): class=%s, length=%d, value=%s\n", 
                      j, task_labels[j], paste(class(param), collapse=","), 
                      length(param), paste(as.character(param), collapse=",")))
          
          if (length(param) != 1) {
            cat("  *** ERROR: Task Parameter", j, "has length", length(param), "instead of 1! ***\n")
          }
        }
        
        dbExecute(con, "
          INSERT INTO task_budgets (
            project_id, task_number, task_name, start_date, end_date, labor_budget
          ) VALUES (?, ?, ?, ?, ?, ?)
        ", params = task_params)
      }
    } else {
      cat("No task budget data to save\n")
    }
    
    
    # Deliverables (same for both update and insert) ----
    cat("\n=== DELIVERABLES DEBUG ===\n")
    if (!is.null(project_data$deliverables) && nrow(project_data$deliverables) > 0) {
      cat("Deliverables data structure:\n")
      str(project_data$deliverables)
      
      for (i in 1:nrow(project_data$deliverables)) {
        deliv <- project_data$deliverables[i, ]
        
        cat(sprintf("\nDeliverable %d parameters:\n", i))
        deliv_params <- list(
          project_id,
          deliv$Deliverable_Name,
          deliv$Linked_Task,
          as.character(deliv$Due_Date)
        )
        
        deliv_labels <- c("project_id", "Deliverable_Name", "Linked_Task", "Due_Date")
        for (j in 1:length(deliv_params)) {
          param <- deliv_params[[j]]
          cat(sprintf("  Deliv Param %d (%s): class=%s, length=%d, value=%s\n", 
                      j, deliv_labels[j], paste(class(param), collapse=","), 
                      length(param), paste(as.character(param), collapse=",")))
          
          if (length(param) != 1) {
            cat("  *** ERROR: Deliverable Parameter", j, "has length", length(param), "instead of 1! ***\n")
          }
        }
        
        dbExecute(con, "
          INSERT INTO deliverables (
            project_id, deliverable_name, linked_task, due_date
          ) VALUES (?, ?, ?, ?)
        ", params = deliv_params)
      }
    } else {
      cat("No deliverables data to save\n")
    }
    
    
    # Labor Categories ----
    cat("\n=== LABOR CATEGORIES DEBUG ===\n")
    if (!is.null(project_data$labor_categories) && nrow(project_data$labor_categories) > 0) {
      cat("Labor categories data structure:\n")
      str(project_data$labor_categories)
      
      # Clear existing labor_categories
      dbExecute(con, "DELETE FROM labor_categories WHERE project_id = ?", params = list(project_id))
      
      # Insert new labor_categories
      for (i in 1:nrow(project_data$labor_categories)) {
        cat(sprintf("\nLabor Category %d parameters:\n", i))
        
        # Check if the column exists
        if ("Role" %in% names(project_data$labor_categories)) {
          labor_cat_name <- project_data$labor_categories$Role[i]
        } else if ("LaborCategory" %in% names(project_data$labor_categories)) {
          labor_cat_name <- project_data$labor_categories$LaborCategory[i]
        } else {
          cat("ERROR: Neither 'Role' nor 'LaborCategory' column found!\n")
          cat("Available columns:", paste(names(project_data$labor_categories), collapse=", "), "\n")
          next
        }
        
        labor_cat_params <- list(project_id, labor_cat_name, i)
        
        labor_cat_labels <- c("project_id", "labor_cat_name", "labor_cat_order")
        for (j in 1:length(labor_cat_params)) {
          param <- labor_cat_params[[j]]
          cat(sprintf("  LaborCat Param %d (%s): class=%s, length=%d, value=%s\n", 
                      j, labor_cat_labels[j], paste(class(param), collapse=","), 
                      length(param), paste(as.character(param), collapse=",")))
          
          if (length(param) != 1) {
            cat("  *** ERROR: Labor Category Parameter", j, "has length", length(param), "instead of 1! ***\n")
          }
        }
        
        dbExecute(con, "
      INSERT INTO labor_categories (project_id, labor_cat_name, labor_cat_order) 
      VALUES (?, ?, ?)
    ", params = labor_cat_params)
      }
    } else {
      cat("No labor categories data to save\n")
    }
    
    
    # Rates ----
    cat("\n=== RATES DEBUG ===\n")
    if (!is.null(project_data$labor_cat_rates) && nrow(project_data$labor_cat_rates) > 0) {
      cat("Rates data structure:\n")
      str(project_data$labor_cat_rates)
      
      # Clear existing rates
      dbExecute(con, "DELETE FROM labor_cat_rates WHERE project_id = ?", params = list(project_id))
      
      # Insert new rates
      rates_data_db <- project_data$labor_cat_rates
      for (i in 1:nrow(rates_data_db)) {
        rate_params <- list(
          project_id, 
          rates_data_db$labor_cat_name[i], 
          rates_data_db$task_number[i], 
          rates_data_db$year_number[i],
          rates_data_db$hourly_rate[i]
        )
        
        dbExecute(con, "
          INSERT INTO labor_cat_rates (project_id, labor_cat_name, task_number, year_number, hourly_rate) 
          VALUES (?, ?, ?, ?, ?)
        ", params = rate_params)
      }
    } else {
      cat("No rates data to save\n")
    }
    
    # Hours ----
    cat("\n=== HOURS DEBUG ===\n")
    if (!is.null(project_data$labor_cat_hours) && nrow(project_data$labor_cat_hours) > 0) {
      cat("Hours data structure:\n")
      str(project_data$labor_cat_hours)
      
      # Clear existing hours
      dbExecute(con, "DELETE FROM labor_cat_hours WHERE project_id = ?", params = list(project_id))
      
      # Insert new hours
      hours_data_db <- project_data$labor_cat_hours
      for (i in 1:nrow(hours_data_db)) {
        hours_params <- list(
          project_id, 
          hours_data_db$labor_cat_name[i], 
          hours_data_db$task_number[i], 
          hours_data_db$year_number[i],
          hours_data_db$planned_hours[i]
        )
        
        dbExecute(con, "
          INSERT INTO labor_cat_hours (project_id, labor_cat_name, task_number, year_number, planned_hours) 
          VALUES (?, ?, ?, ?, ?)
        ", params = hours_params)
      }
    } else {
      cat("No hours data to save\n")
    }
    
    
    
    # Labor Category Staff ----
    cat("\n=== STAFF ASSIGNMENTS DEBUG ===\n")
    if (!is.null(project_data$labor_cat_staff) && nrow(project_data$labor_cat_staff) > 0) {
      cat("Staff assignments data structure:\n")
      str(project_data$labor_cat_staff)
      
      # Clear existing staff assignments
      dbExecute(con, "DELETE FROM labor_cat_staff WHERE project_id = ?", params = list(project_id))
      
      # Insert new staff assignments
      for (i in 1:nrow(project_data$labor_cat_staff)) {
        cat(sprintf("\nStaff assignment %d parameters:\n", i))
        
        # Check column names
        if ("Role" %in% names(project_data$labor_cat_staff)) {
          labor_cat_name <- project_data$labor_cat_staff$Role[i]
          staff_members <- project_data$labor_cat_staff$Staff_Members[i]
        } else if ("LaborCategory" %in% names(project_data$labor_cat_staff)) {
          labor_cat_name <- project_data$labor_cat_staff$LaborCategory[i]
          staff_members <- project_data$labor_cat_staff$Staff_Members[i]
        } else {
          cat("ERROR: Neither 'Role' nor 'LaborCategory' column found in staff!\n")
          cat("Available columns:", paste(names(project_data$labor_cat_staff), collapse=", "), "\n")
          next
        }
        
        staff_params <- list(project_id, labor_cat_name, staff_members)
        staff_labels <- c("project_id", "labor_cat_name", "staff_names")
        
        for (j in 1:length(staff_params)) {
          param <- staff_params[[j]]
          cat(sprintf("  Staff Param %d (%s): class=%s, length=%d, value=%s\n", 
                      j, staff_labels[j], paste(class(param), collapse=","), 
                      length(param), paste(as.character(param), collapse=",")))
          
          if (length(param) != 1) {
            cat("  *** ERROR: Staff Parameter", j, "has length", length(param), "instead of 1! ***\n")
          }
        }
        
        dbExecute(con, "
          INSERT INTO labor_cat_staff (project_id, labor_cat_name, staff_names)
          VALUES (?, ?, ?)
        ", params = staff_params)
      }
    } else {
      cat("No staff assignments data to save\n")
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
    
    # Load labor_categories
    labor_categories <- dbGetQuery(con, "SELECT labor_cat_name FROM labor_categories WHERE project_id = ? ORDER BY labor_cat_order", 
                        params = list(project_id))
    
    # Load role rates  
    labor_cat_rates <- dbGetQuery(con, "SELECT * FROM labor_cat_rates WHERE project_id = ?", 
                             params = list(project_id))
    
    # Load role hours
    labor_cat_hours <- dbGetQuery(con, "SELECT * FROM labor_cat_hours WHERE project_id = ?",
                             params = list(project_id))
    
    # Load role staff
    labor_cat_staff <- dbGetQuery(con, "SELECT * FROM labor_cat_staff WHERE project_id = ?",
                             params = list(project_id))
    
    cat("=== load_project_from_db Debug ===\n")
    cat("Project ID:", project_id, "\n")
    cat("Roles loaded:", nrow(labor_categories), "\n")
    if(nrow(labor_categories) > 0) cat("Role names:", paste(labor_categories$labor_cat_name, collapse = ", "), "\n")
    cat("Role rates loaded:", nrow(labor_cat_rates), "\n")
    cat("Role hours loaded:", nrow(labor_cat_hours), "\n")
    cat("Role staff loaded:", nrow(labor_cat_staff), "\n")
    
    dbDisconnect(con)
    
    return(list(
      success = TRUE,
      project = project,
      tasks = tasks,
      deliverables = deliverables,
      labor_categories = labor_categories,          
      labor_cat_rates = labor_cat_rates,
      labor_cat_hours = labor_cat_hours,
      labor_cat_staff = labor_cat_staff
    ))
    
  }, error = function(e) {
    if (exists("con")) dbDisconnect(con)
    return(list(success = FALSE, message = paste("Error loading project:", e$message)))
  })
}

# Lists ----

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




# GANTT CHART FUNCTIONS -------------------------------------------------------

#' Create interactive Gantt chart from task and deliverable data
#' @param task_data Data frame with task information
#' @param deliverable_data Data frame with deliverable information
#' @param project_start Optional project start date
#' @param project_end Optional project end date

create_gantt_chart <- function(task_data, deliverable_data, project_start = NULL, project_end = NULL) {
  if (is.null(task_data) && is.null(deliverable_data)) {
    return(ggplot() + 
             theme_void() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, color = "gray"))
  }
  
  gantt_data <- data.frame()
  
  # Process tasks
  if (!is.null(task_data) && nrow(task_data) > 0) {
    valid_tasks <- task_data[!is.na(task_data$Task_Name) & task_data$Task_Name != "", ]
    
    if (nrow(valid_tasks) > 0) {
      # Parse dates - handle multiple formats
      parse_date <- function(date_col) {
        sapply(date_col, function(d) {
          if (is.na(d) || d == "") return(as.Date(NA))
          # Try multiple formats
          for (fmt in c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y")) {
            parsed <- tryCatch(as.Date(as.character(d), format = fmt), error = function(e) NA)
            if (!is.na(parsed)) return(as.character(parsed))
          }
          return(as.character(as.Date(NA)))
        })
      }
      
      task_gantt <- data.frame(
        name = paste0("Task ", valid_tasks$Task_Number, ": ", valid_tasks$Task_Name),
        start = as.Date(parse_date(valid_tasks$Start_Date)),
        end = as.Date(parse_date(valid_tasks$End_Date)),
        type = "Task",
        category = valid_tasks$Task_Number,
        tooltip = paste0("Task ", valid_tasks$Task_Number, ": ", valid_tasks$Task_Name, 
                         "<br>Duration: ", parse_date(valid_tasks$Start_Date), " to ", parse_date(valid_tasks$End_Date),
                         "<br>Budget: $", format(valid_tasks$Toxcel_Labor_Budget, big.mark = ",", scientific = FALSE)),
        stringsAsFactors = FALSE
      )
      
      gantt_data <- rbind(gantt_data, task_gantt)
    }
  }
  
  # Process deliverables
  if (!is.null(deliverable_data) && nrow(deliverable_data) > 0) {
    valid_deliverables <- deliverable_data[!is.na(deliverable_data$Deliverable_Name) & 
                                             deliverable_data$Deliverable_Name != "", ]
    
    if (nrow(valid_deliverables) > 0) {
      deliv_gantt <- data.frame(
        name = valid_deliverables$Deliverable_Name,
        start = as.Date(valid_deliverables$Due_Date) - 1,  # Show as 2-day milestone
        end = as.Date(valid_deliverables$Due_Date),
        type = "Deliverable",
        category = valid_deliverables$Linked_Task,
        tooltip = paste0(valid_deliverables$Deliverable_Name,
                         "<br>Due: ", format(valid_deliverables$Due_Date, "%Y-%m-%d"),
                         "<br>Linked to: ", valid_deliverables$Linked_Task),
        stringsAsFactors = FALSE
      )
      
      gantt_data <- rbind(gantt_data, deliv_gantt)
    }
  }
  
  # Remove rows with invalid dates
  gantt_data <- gantt_data[!is.na(gantt_data$start) & !is.na(gantt_data$end), ]
  
  if (nrow(gantt_data) == 0) {
    return(ggplot() + 
             theme_void() + 
             annotate("text", x = 0.5, y = 0.5, label = "No valid date data available", size = 6, color = "gray"))
  }
  
  # Sort by start date
  gantt_data <- gantt_data[order(gantt_data$start, gantt_data$type), ]
  gantt_data$y_pos <- factor(gantt_data$name, levels = rev(gantt_data$name))
  
  # Create the plot
  p <- ggplot(gantt_data, aes(y = y_pos)) +
    geom_segment_interactive(
      aes(x = start, xend = end, yend = y_pos, 
          color = type, tooltip = tooltip, data_id = name),
      size = 6, alpha = 0.8
    ) +
    scale_color_manual(values = c("Task" = "#3498db", "Deliverable" = "#e74c3c")) +
    scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
    labs(
      title = "Project Timeline",
      x = "Date",
      y = "",
      color = "Type"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold", color = "#2c3e50"),
      legend.position = "bottom"
    )
  
  # Add today's line
  p <- p + geom_vline(xintercept = as.numeric(Sys.Date()), 
                      color = "#f39c12", linetype = "dashed", size = 1, alpha = 0.7)
  
  return(p)
}



# WeWorked =====================================================================

heads  <- add_headers(
  'x-api-key' = 'GFfwz6ntiR4KUaAltF9Uf73BMMcXzCnQ8ylnI16e', 
  'x-ww-user' = 'nicholas.kehoe@toxcel.com')


get_all_users <- function(headers=heads) {
  url <- 'https://api.weworked.com/v1/users'
  res <- GET(url, headers)
  temp <- fromJSON(content(res, "text"))
  users <-  bind_rows(
    temp$Active,
    temp$Disabled
  )
  return(users)
}


WWusers <- get_all_users() %>% 
  mutate(fullName = paste0(lastName, ", ", firstName)) %>%
  arrange(fullName)


get_report <- function(
    period_start, 
    period_end=paste0(Sys.Date()-1), 
    project = 'all', 
    headers=heads,
    summaryOnly=FALSE) {
  
  if(summaryOnly){
    url <- paste0(
      'https://api.weworked.com/v1/reports/summary?fromdate=',
      period_start, '&todate=', period_end, 
      '&projects=', project, 
      '&showzeros=0&taskstatus=all&timestatus=all&users=all')
  } else{
    url <- paste0(
      'https://api.weworked.com/v1/reports/detailed?fromdate=',
      period_start, '&todate=', period_end, 
      '&projects=', project, 
      '&showzeros=0&taskstatus=all&timestatus=all&users=all')
  }
  
  res <- GET(url, headers)
  
  report_data <- 
    as.data.frame(fromJSON(content(res, "text"), flatten = TRUE)) #%>%
    #mutate(task = shortenTaskName(task))
  
  return(report_data)
  
}


#' Fetch WeWorked hours for a specific project ID
#' @param project_id The WeWorked project ID to fetch data for
#' @param period_start Start date for the report (should be project start date)
#' @param period_end End date for the report (default: today)
#' @param headers WeWorked API headers
get_project_hours <- function(project_id, period_start, period_end = NULL, headers = heads) {
  tryCatch({
    # Set default end date if not provided
    if (is.null(period_end)) period_end <- Sys.Date()
    
    # Format dates for API
    start_str <- format(period_start, "%Y-%m-%d")
    end_str <- format(period_end, "%Y-%m-%d")
    
    cat("=== WeWorked API Call ===\n")
    cat("Project ID:", project_id, "\n")
    cat("Date range:", start_str, "to", end_str, "\n")
    
    # Get detailed report for the specific project only
    report_data <- get_report(
      period_start = start_str,
      period_end = end_str,
      project = project_id,  # Use project_id directly
      headers = headers,
      summaryOnly = FALSE
    )
    
    cat("Raw WeWorked response rows:", nrow(report_data), "\n")
    if (nrow(report_data) > 0) {
      cat("WeWorked columns:", paste(names(report_data), collapse = ", "), "\n")
    }
    
    if (nrow(report_data) > 0) {
      # Filter for billable hours only
      billable_data <- report_data[report_data$billable == 1, ]
      cat("Billable rows:", nrow(billable_data), "\n")
      
      if (nrow(billable_data) > 0) {
        # Create fullName by combining lastName, firstName
        result <- billable_data %>%
          mutate(
            fullName = paste0(lastName, ", ", firstName),
            hours = as.numeric(hours),
            date = as.Date(date)
          ) %>%
          select(
            fullName,
            task,
            hours,
            date,
            project,
            projectId
          ) %>%
          filter(hours > 0)
        
        cat("Final processed rows:", nrow(result), "\n")
        cat("Unique staff:", paste(unique(result$fullName), collapse = ", "), "\n")
        cat("Unique tasks:", paste(unique(result$task), collapse = ", "), "\n")
        
        return(list(success = TRUE, data = result))
      }
    }
    
    cat("No billable hours found\n")
    return(list(success = TRUE, data = data.frame(), message = "No billable hours found for this project"))
    
  }, error = function(e) {
    cat("WeWorked API Error:", e$message, "\n")
    return(list(success = FALSE, message = paste("WeWorked API error:", e$message)))
  })
}


#' Get all WeWorked projects for lookup
#' @param headers WeWorked API headers
#' @return A datframe with two columns: ProjectName and ProjectID
get_all_projects <- function(headers = heads) {
  tryCatch({
    url <- 'https://api.weworked.com/v1/projects'
    res <- GET(url, headers)
    
    cat("WeWorked API Status:", status_code(res), "\n")
    
    if (status_code(res) == 200) {
      response_text <- content(res, "text")
      projects_data <- fromJSON(response_text)
      
      result <- distinct(as.data.frame(projects_data$Active), ProjectName, ProjectId)
      
      return(result)
      
    } else {
      cat("API error - Status:", status_code(res), "\n")
      return(list(success = FALSE, message = paste("API returned status", status_code(res))))
    }
    
  }, error = function(e) {
    cat("Error in get_all_projects:", e$message, "\n")
    return(list(success = FALSE, message = paste("Error fetching projects:", e$message)))
  })
}


# Misc ----

# Helper function for empty plotly charts
plotly_empty <- function(message = "No data available") {
  plot_ly() %>%
    add_annotations(
      text = message,
      x = 0.5,
      y = 0.5,
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      font = list(size = 16, color = "gray")
    ) %>%
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
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