# Planhour - Project Management Shiny App

source("global.R")
source("R/mod_pm_dashboard.R")
# source("R/mod_staff_view.R")      # For future implementation
# source("R/mod_finance_view.R")    # For future implementation
source("manage_db.R")


ui <- dashboardPage(
  
  dashboardHeader(title = "Planhour - Project Management"),
  
  dashboardSidebar(
    width = 300,
    div(class = "sidebar-content", style = "position: fixed; height: 100vh; overflow-y: auto; width: 280px;",
        sidebarMenu(
          id = "sidebar",
          menuItem("Project Manager", tabName = "pm_dashboard", icon = icon("tasks")),
          menuItem("Staff View", tabName = "staff_view", icon = icon("users")),
          menuItem("Finance View", tabName = "finance_view", icon = icon("chart-line")),
          br(),
          hr(),
          
          # Project Summary in Sidebar
          div(id = "project_summary_sidebar", style = "margin: 10px;",
              h5("Project Summary", style = "color: white; margin-bottom: 10px;"),
              div(id = "sidebar_summary_content", 
                  style = "background-color: rgba(255,255,255,0.1); padding: 8px; border-radius: 4px; font-size: 11px; color: #ccc;",
                  "Enter project details to see summary...")
          ),
          br(),
          # Staff List in Sidebar
          div(id = "staff_list_sidebar", style = "margin: 10px;",
              h5("Project Staff", style = "color: white; margin-bottom: 10px;"),
              div(id = "sidebar_staff_content", 
                  style = "background-color: rgba(255,255,255,0.1); padding: 8px; border-radius: 4px; font-size: 11px; color: #ccc;",
                  "No staff assigned yet...")
          ),
          
          hr(),
          
          actionButton("save_changes_sidebar", "Save Changes", 
                       class = "btn-success btn-block", 
                       icon = icon("save"),
                       style = "margin: 10px; background-color: #28a745; border-color: #1e7e34;"),
          br(),
          
          actionButton("clear_form_sidebar", "Clear Form", 
                       class = "btn-warning btn-block", 
                       icon = icon("eraser"),
                       style = "margin: 10px; background-color: #f39c12; border-color: #e67e22;")
        )
    )
  ),
  
  dashboardBody(

    useShinyjs(),
    
    tags$script("
  Shiny.addCustomMessageHandler('triggerClearForm', function(message) {
    // Clear the unified project selector without triggering change event
    var selectize = $('#pm_dashboard-project_name_unified').selectize()[0].selectize;
    selectize.off('change');  // Temporarily disable change event
    selectize.clear();
    selectize.on('change');   // Re-enable change event
    
    // Clear other form fields
    $('#pm_dashboard-project_name').val('');
    $('#pm_dashboard-project_id').val('');
    $('#pm_dashboard-client_name').val('');
    $('#pm_dashboard-project_manager').val('').trigger('change');
    $('#pm_dashboard-project_description').val('');
    $('#pm_dashboard-total_dollar_value').val('');
    $('#pm_dashboard-overhead_multiplier').val('1.5');
    
    // Trigger change events to update reactive values
    $('#pm_dashboard-project_name').trigger('change');
  });
"),
    
    # Custom CSS for better styling
    tags$head(
      tags$style(HTML("
    .content-wrapper, .right-side {
      background-color: #f7f7f7;
    }
    .well {
      background-color: white;
      border: 1px solid #ddd;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      padding: 0px 19px 19px 19px;
    }
    /* RESTORE WORKING ALIGNMENT */
    .form-group-aligned {
      display: table !important;
      width: 100% !important;
      table-layout: fixed !important;
      margin: 0 !important;
      padding: 0 !important;
      line-height: 20px !important;
      height: 20px !important;
      border-spacing: 0 0 !important;
    }
    .form-group-aligned + .form-group-aligned {
      margin-top: 1px !important;
    }
    .form-group-aligned .col-sm-3,
    .form-group-aligned .col-md-3,
    .form-group-aligned .col-xs-3 {
      display: table-cell !important;
      vertical-align: top !important;
      width: 30% !important;
      padding: 8px 0 0 0 !important;
      float: none !important;
      margin: 0 !important;
      min-height: 0 !important;
      line-height: 0 !important;
    }
    .form-group-aligned .col-sm-9,
    .form-group-aligned .col-md-9,
    .form-group-aligned .col-xs-9 {
      display: table-cell !important;
      vertical-align: middle !important;
      width: 70% !important;
      padding: 0 !important;
      float: none !important;
      margin: 0 !important;
      min-height: 0 !important;
      line-height: 0 !important;
    }
    .control-label-aligned {
      font-weight: 600 !important;
      color: #333 !important;
      margin: 0 !important;
      padding: 0 !important;
      line-height: normal !important;
      height: auto !important;
      display: block !important;
    }
    .input-wrapper {
      margin: 0 !important;
      padding: 0 !important;
      line-height: 0 !important;
    }
    .input-wrapper .form-control,
    .input-wrapper .selectize-control,
    .input-wrapper .selectize-input {
      vertical-align: middle !important;
      line-height: normal !important;
      margin: 0 !important;
    }
  "))
    ),    
    
    tabItems(
      # PM tab ----
      tabItem(
        tabName = "pm_dashboard",
        mod_pm_dashboard_ui("pm_dashboard")
      ),
      
      # Staff View Tab ----
      tabItem(
        tabName = "staff_view",
        fluidRow(
          column(12,
                 wellPanel(
                   h3("Staff View - Gantt Chart"),
                   p("This view will show a Gantt chart of tasks assigned to the current staff member."),
                   p("Features to be implemented:"),
                   tags$ul(
                     tags$li("Interactive Gantt chart using ggplot2 + ggiraph"),
                     tags$li("Date range: 30 days ago to 60 days in future"),
                     tags$li("Filter to show only current user's assignments"),
                     tags$li("Task details on hover/click")
                   ),
                   div(style = "text-align: center; padding: 50px;",
                       h4("Coming Soon", style = "color: #666;"),
                       icon("calendar", style = "font-size: 48px; color: #ddd;")
                   )
                 )
          )
        )
      ),
      
      # Finance View Tab ----
      tabItem(
        tabName = "finance_view",
        fluidRow(
          column(12,
                 wellPanel(
                   h3("Finance / Management View"),
                   p("This view provides read-only access to financial summaries and reports."),
                   p("Features to be implemented:"),
                   tags$ul(
                     tags$li("Task-level cost summaries across multiple projects"),
                     tags$li("Total hours, total cost, budget vs actual comparisons"),
                     tags$li("Visual dashboards and reports"),
                     tags$li("Export capabilities for financial data")
                   ),
                   div(style = "text-align: center; padding: 50px;",
                       h4("Coming Soon", style = "color: #666;"),
                       icon("chart-bar", style = "font-size: 48px; color: #ddd;")
                   )
                 )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Handle clear form from sidebar
  clear_trigger <- reactiveVal(NULL)
  
  # Save Changes ----
  observeEvent(input$save_changes_sidebar, {
    # Trigger the PM module's save functionality
    session$sendCustomMessage("triggerSaveProject", list())
    showNotification("Attempting to save project...", type = "message", duration = 2)
  })
  
  # Clear form ----
  observeEvent(input$clear_form_sidebar, {
    # Trigger the clear action in the PM module
    clear_trigger(Sys.time())  # Use timestamp to ensure reactivity
  })
  
  
  # Sync sidebar menu with role selector
  observeEvent(input$user_role, {
    role_to_tab <- list(
      "pm" = "pm_dashboard",
      "staff" = "staff_view",
      "finance" = "finance_view"
    )
    
    new_tab <- role_to_tab[[input$user_role]]
    updateTabItems(session, "sidebar", selected = new_tab)
  })
  
  # Sync role selector with sidebar menu
  observeEvent(input$sidebar, {
    tab_to_role <- list(
      "pm_dashboard" = "pm",
      "staff_view" = "staff",
      "finance_view" = "finance"
    )
    
    new_role <- tab_to_role[[input$sidebar]]
    if (!is.null(new_role)) {
      updateSelectInput(session, "user_role", selected = new_role)
    }
  })
  
  # Initialize Project Manager module
  pm_data <- mod_pm_dashboard_server("pm_dashboard", clear_trigger = reactive(clear_trigger()))
  
  
  # Update sidebar project summary
  observe({
    project_info <- pm_data$project_info()
    
    if (!is.null(project_info$name) && project_info$name != "") {
      
      # Calculate duration in months (rounded to integer)
      duration_days <- as.numeric(project_info$end_date - project_info$start_date)
      duration_months <- round(duration_days / 30.44, 0)  # Round to integer
      
      # Get task and deliverable counts
      tasks <- pm_data$task_budget()
      deliverables <- pm_data$deliverables()
      
      task_count <- if(!is.null(tasks)) nrow(tasks) else 0
      deliverable_count <- if(!is.null(deliverables)) nrow(deliverables) else 0
      
      # Format dates as m/d/y
      start_formatted <- format(project_info$start_date, "%m/%d/%Y")
      end_formatted <- format(project_info$end_date, "%m/%d/%Y")
      
      # Build summary text in the requested order
      summary_text <- paste0(
        project_info$name, "\n",
        project_info$client, "\n",
        "POP ", start_formatted, " - ", end_formatted, " (", duration_months, "mo)\n",
        "$", format(project_info$total_value, big.mark = ",", scientific = FALSE), "\n",
        "PM ", project_info$manager, "\n",
        task_count, " Tasks\n",
        deliverable_count, " Deliverables"
      )
      
      # Update sidebar content
      runjs(paste0("
        document.getElementById('sidebar_summary_content').innerHTML = '", 
                   gsub("\n", "<br>", summary_text), "';
      "))
    }
  })
  
  # Update sidebar staff list
  observe({
    staff_data <- pm_data$labor_cat_staff()
    
    if (!is.null(staff_data) && nrow(staff_data) > 0) {
      # Get unique non-empty staff names
      staff_names <- unique(staff_data$Staff_Name)
      staff_names <- staff_names[!is.na(staff_names) & staff_names != ""]
      
      if (length(staff_names) > 0) {
        staff_list <- paste0("• ", staff_names, collapse = "<br>")
        staff_text <- paste0(length(staff_names), " staff members:<br>", staff_list)
      } else {
        staff_text <- "No staff assigned yet..."
      }
      
      # Update sidebar staff content
      runjs(paste0("
        document.getElementById('sidebar_staff_content').innerHTML = '", staff_text, "';
      "))
    }
  })
  

  
  # Global reactive values for cross-module communication
  global_data <- reactiveValues(
    current_user = "demo_user",
    current_role = "pm"
  )
  
  # Session info for debugging
  if (getOption("shiny.debug", FALSE)) {
    observe({
      cat("Current tab:", input$sidebar, "\n")
      cat("Current role:", input$user_role, "\n")
    })
  }
}


shinyApp(ui = ui, server = server)