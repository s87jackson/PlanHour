# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)

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



setup_database <- function(db_path = "project_manager.db") {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Projects table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS projects (
      project_id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_name TEXT NOT NULL,
      project_id_text TEXT,
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
  
  # Task hour assignments table
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
          project_id_text = ?, client_name = ?, project_manager = ?, project_description = ?,
          start_date = ?, end_date = ?, total_dollar_value = ?, 
          overhead_multiplier = ?, updated_timestamp = CURRENT_TIMESTAMP
        WHERE project_id = ?
      ", params = list(
        project_data$project_info$project_id_text,
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
          project_name, project_id_text, client_name, project_manager, project_description,
          start_date, end_date, total_dollar_value, overhead_multiplier
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(
        project_data$project_info$name,
        project_data$project_info$project_id_text,
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
  users <-  #bind_rows(
    temp$Active #,
    #temp$Disabled
  #)
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
    as.data.frame(fromJSON(content(res, "text"), flatten = TRUE)) %>%
    mutate(task = shortenTaskName(task))
  
  return(report_data)
  
}