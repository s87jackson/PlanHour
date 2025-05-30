library(shiny)

# Load modules
source("R/mod_pm_dashboard.R")
source("R/mod_staff_dashboard.R")
source("R/mod_finance_dashboard.R")

ui <- fluidPage(
  titlePanel("PlanHour"),
  uiOutput("role_selector_ui"),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  # Get user email from shinyapps.io login
  user_email <- reactive({
    if (is.null(session$user)) {
      # Local dev fallback
      return("dev_user@toxcel.com")
    } else {
      return(session$user)
    }
  })
  
  
  # Select current role/view
  output$role_selector_ui <- renderUI({
    req(user_email())
    selectInput("current_role", "Select your view:",
                choices = c("Project Manager" = "pm",
                            "Staff" = "staff",
                            "Finance / Management" = "finance"))
  })
  
  output$main_ui <- renderUI({
    req(input$current_role)
    
    switch(input$current_role,
           "pm" = mod_pm_dashboard_ui("pm_dash"),
           "staff" = mod_staff_dashboard_ui("staff_dash"),
           "finance" = mod_finance_dashboard_ui("finance_dash"))
  })
  
  observe({
    req(input$current_role)
    
    switch(input$current_role,
           "pm" = mod_pm_dashboard_server("pm_dash", user_email),
           "staff" = mod_staff_dashboard_server("staff_dash", user_email),
           "finance" = mod_finance_dashboard_server("finance_dash", user_email))
  })
}

shinyApp(ui, server)
