# R/mod_finance_dashboard.R

mod_finance_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Finance/Management Dashboard (placeholder)"),
    p("This is where finance can view project costs across all projects.")
  )
}

mod_finance_dashboard_server <- function(id, user_email) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Temporary placeholder logic
    observe({
      cat("Finance dashboard active for:", user_email(), "\n")
    })
  })
}
