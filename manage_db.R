# COPY AND PASTE THIS ENTIRE BLOCK INTO YOUR R CONSOLE

# Load required libraries
library(DBI)
library(RSQLite)

# Clear all data from database (keeps table structure)
clear_database <- function(db_path = "project_manager.db") {
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # Start transaction
    dbBegin(con)
    
    # Delete all data (order matters due to foreign keys)
    dbExecute(con, "DELETE FROM labor_cat_staff")
    dbExecute(con, "DELETE FROM labor_cat_hours") 
    dbExecute(con, "DELETE FROM labor_cat_rates")
    dbExecute(con, "DELETE FROM labor_categories")
    dbExecute(con, "DELETE FROM deliverables")
    dbExecute(con, "DELETE FROM task_budgets")
    dbExecute(con, "DELETE FROM projects")

    
    # Reset auto-increment counters
    dbExecute(con, "DELETE FROM sqlite_sequence WHERE name IN ('projects', 'task_budgets', 'deliverables', 'labor_categories', 'labor_cat_rates', 'labor_cat_hours', 'labor_cat_staff')")
    
    # Commit transaction
    dbCommit(con)
    dbDisconnect(con)
    
    cat("✅ Database cleared successfully! All data removed.\n")
    return(TRUE)
    
  }, error = function(e) {
    if (exists("con")) {
      dbRollback(con)
      dbDisconnect(con)
    }
    cat("❌ Error clearing database:", e$message, "\n")
    return(FALSE)
  })
}

# Delete the entire database file
delete_database_file <- function(db_path = "project_manager.db") {
  if (file.exists(db_path)) {
    file.remove(db_path)
    cat("✅ Database file", db_path, "deleted successfully!\n")
    return(TRUE)
  } else {
    cat("⚠️ Database file", db_path, "does not exist.\n")
    return(FALSE)
  }
}

# Delete a specific project by name
delete_project_by_name <- function(project_name, db_path = "project_manager.db") {
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # Find project ID
    project <- dbGetQuery(con, "SELECT project_id FROM projects WHERE project_name = ?", 
                          params = list(project_name))
    
    if (nrow(project) == 0) {
      dbDisconnect(con)
      cat("⚠️ Project '", project_name, "' not found.\n")
      return(FALSE)
    }
    
    project_id <- project$project_id[1]
    
    # Start transaction
    dbBegin(con)
    
    # Delete in order (foreign key constraints)
    dbExecute(con, "DELETE FROM labor_cat_rates WHERE project_id = ?", params = list(project_id))
    dbExecute(con, "DELETE FROM labor_cat_hours WHERE project_id = ?", params = list(project_id))
    dbExecute(con, "DELETE FROM labor_cat_staff WHERE project_id = ?", params = list(project_id))
    dbExecute(con, "DELETE FROM deliverables WHERE project_id = ?", params = list(project_id))
    dbExecute(con, "DELETE FROM task_budgets WHERE project_id = ?", params = list(project_id))
    dbExecute(con, "DELETE FROM projects WHERE project_id = ?", params = list(project_id))
    
    # Commit transaction
    dbCommit(con)
    dbDisconnect(con)
    
    cat("✅ Project '", project_name, "' deleted successfully!\n")
    return(TRUE)
    
  }, error = function(e) {
    if (exists("con")) {
      dbRollback(con)
      dbDisconnect(con)
    }
    cat("❌ Error deleting project:", e$message, "\n")
    return(FALSE)
  })
}

# View database contents (for debugging)
view_database_contents <- function(db_path = "project_manager.db") {
  if (!file.exists(db_path)) {
    cat("⚠️ Database file does not exist.\n")
    return()
  }
  
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    cat("📊 DATABASE CONTENTS:\n")
    cat(paste(rep("=", 50), collapse = ""), "\n")
    
    # Get list of all tables
    tables <- dbListTables(con)
    cat("Tables found:", length(tables), "-", paste(tables, collapse = ", "), "\n\n")
    
    # Print each table individually (up to 100 rows)
    for(table_name in tables) {
      cat("TABLE:", toupper(table_name), "\n")
      cat(paste(rep("-", 30), collapse = ""), "\n")
      
      # Get table info
      table_info <- dbGetQuery(con, paste("PRAGMA table_info(", table_name, ")"))
      cat("Columns:", paste(table_info$name, collapse = ", "), "\n")
      
      # Get data (limit to 100 rows)
      data <- dbGetQuery(con, paste("SELECT * FROM", table_name, "LIMIT 100"))
      cat("Rows:", nrow(data), "\n")
      
      if (nrow(data) > 0) {
        # Print first few rows
        print(head(data, 10))
        if (nrow(data) > 10) {
          cat("... (showing first 10 of", nrow(data), "rows)\n")
        }
      } else {
        cat("(No data)\n")
      }
      cat("\n")
    }
    
    dbDisconnect(con)
    
  }, error = function(e) {
    if (exists("con")) dbDisconnect(con)
    cat("❌ Error viewing database:", e$message, "\n")
  })
}

# Confirm functions are loaded
cat("✅ Database management functions loaded!\n")
cat("Available functions:\n")
cat("  - clear_database()               # Clear all data but keep tables\n")
cat("  - delete_database_file()         # Delete entire database file\n") 
cat("  - delete_project_by_name('Name') # Delete specific project\n")
cat("  - view_database_contents()       # See what's in database\n")

# NOW YOU CAN RUN THESE COMMANDS:
# clear_database()
# view_database_contents()
# delete_project_by_name("My Project")
# delete_database_file()