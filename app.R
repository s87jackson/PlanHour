# Planhour - Project Management Shiny App
# Main application file



# Source all module files (adjust paths as needed for your structure)
# If using {golem} framework, these would be automatically loaded
source("R/mod_pm_dashboard.R")
# source("R/mod_staff_view.R")      # For future implementation
# source("R/mod_finance_view.R")    # For future implementation

# Define UI ----
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
          )
        )
    )
  ),
  
  dashboardBody(
    # Enable shinyjs
    useShinyjs(),
    
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
        }
        .btn-primary {
          background-color: #337ab7;
          border-color: #2e6da4;
        }
        .btn-warning {
          background-color: #f0ad4e;
          border-color: #eea236;
        }
        .sidebar-form {
          margin: 10px;
        }
        .skin-blue .main-sidebar {
          background-color: #2c3e50;
        }
        .sidebar-content {
          position: fixed !important;
          top: 50px;
          height: calc(100vh - 50px);
          overflow-y: auto;
          z-index: 1000;
        }
        .content-wrapper {
          margin-left: 300px;
        }
        .rhandsontable {
          font-size: 12px;
        }
        .dataTables_wrapper {
          margin-top: 20px;
        }
      "))
    ),
    
    tabItems(
      # Project Manager Dashboard Tab
      tabItem(
        tabName = "pm_dashboard",
        mod_pm_dashboard_ui("pm_dashboard"),
        
        fluidRow(
          column(12,
                 wellPanel(
                   h3("Project Gantt Chart"),
                   p("Interactive timeline showing all tasks and deliverables. Hover over items for details."),
                   div(style = "background: white; border: 1px solid #ddd; border-radius: 4px; padding: 15px; margin: 10px 0;",
                       ggiraph::girafeOutput("pm_gantt_chart", height = "500px")
                   )
                 )
          )
        )
      ),
      
      # Staff View Tab (placeholder for future implementation)
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
      
      # Finance View Tab (placeholder for future implementation)
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
  pm_data <- mod_pm_dashboard_server("pm_dashboard")
  
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
    staff_data <- pm_data$staff_assignments()
    
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
  
  
  # Initialize Project Manager module
  pm_data <- mod_pm_dashboard_server("pm_dashboard")
  
  # PM Dashboard Gantt Chart
  output$pm_gantt_chart <- ggiraph::renderGirafe({
    req(pm_data$task_budget(), pm_data$deliverables())
    
    p <- create_gantt_chart(
      task_data = pm_data$task_budget(),
      deliverable_data = pm_data$deliverables(),
      project_start = pm_data$project_info()$start_date,
      project_end = pm_data$project_info()$end_date
    )
    
    # Make it interactive with ggiraph
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
  
  
  # For future modules, you would initialize them here:
  # staff_data <- mod_staff_view_server("staff_view", pm_data)
  # finance_data <- mod_finance_view_server("finance_view", pm_data)
  
  # Optional: Global reactive values for cross-module communication
  global_data <- reactiveValues(
    current_user = "demo_user",
    current_role = "pm"
  )
  
  # Optional: Session info for debugging
  if (getOption("shiny.debug", FALSE)) {
    observe({
      cat("Current tab:", input$sidebar, "\n")
      cat("Current role:", input$user_role, "\n")
    })
  }
}

# Run the application
shinyApp(ui = ui, server = server)