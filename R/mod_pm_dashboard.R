# R/mod_pm_dashboard.R

mod_pm_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Project Manager Dashboard"),
    p("This is where the PM will create projects, assign staff, and manage budgets."),
    
    # Project Info
    fluidRow(
      column(4,
             textInput(ns("proj_name"), "Project Name"),
             numericInput(ns("proj_overhead"), "Overhead Multiplier", value = 1.5),
             dateInput(ns("proj_start"), "Start Date"),
             dateInput(ns("proj_end"), "End Date"),
             actionButton(ns("create_project"), "Create / Update Project", icon = icon("save"))
      ),
      column(8, verbatimTextOutput(ns("proj_summary")))
    ),
    
    hr(),
    h4("Define Project Items"),
    fluidRow(
      column(3, textInput(ns("task_name"), "Label")),
      column(3, radioButtons(ns("task_type"), "Type", choices = c("Task", "Deliverable"), inline = TRUE))
    ),
    fluidRow(
      column(4, uiOutput(ns("start_date_ui"))),
      column(4, uiOutput(ns("end_date_ui")))
    ),
    actionButton(ns("add_task"), "Add Item", icon = icon("plus")),
    br(), br(),
    DT::dataTableOutput(ns("task_table")),
    
    hr(),
    h4("Staff Task Assignment Table (Matrix Style)"),
    actionButton(ns("add_row"), "Add Row"),
    actionButton(ns("remove_row"), "Remove Last Row"),
    br(), br(),
    uiOutput(ns("assignment_matrix"))
  )
}

mod_pm_dashboard_server <- function(id, user_email) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    # Project Info
    proj_data <- reactiveValues()
    observeEvent(input$create_project, {
      req(input$proj_name, input$proj_overhead, input$proj_start, input$proj_end)
      proj_data$name <- input$proj_name
      proj_data$overhead <- input$proj_overhead
      proj_data$start <- input$proj_start
      proj_data$end <- input$proj_end
    })
    output$proj_summary <- renderPrint({
      if (is.null(proj_data$name)) return("No project created yet.")
      list(Name = proj_data$name, Overhead = proj_data$overhead, Start = proj_data$start, End = proj_data$end)
    })
    
    # Task List
    task_list <- reactiveVal(data.frame(Name = character(), Type = character(), Start = as.Date(character()), End = as.Date(character())))
    
    observeEvent(input$add_task, {
      req(input$task_name)
      tasks <- task_list()
      new_task <- if (input$task_type == "Task") {
        data.frame(Name = input$task_name, Type = "Task", Start = input$task_start, End = input$task_end)
      } else {
        data.frame(Name = input$task_name, Type = "Deliverable", Start = NA, End = input$task_due)
      }
      task_list(rbind(tasks, new_task))
    })
    
    output$task_table <- DT::renderDataTable({
      DT::datatable(task_list(), options = list(pageLength = 5), rownames = FALSE)
    })
    
    output$start_date_ui <- renderUI({
      if (input$task_type == "Task") dateInput(ns("task_start"), "Start Date") else NULL
    })
    output$end_date_ui <- renderUI({
      if (input$task_type == "Task") {
        dateInput(ns("task_end"), "End Date")
      } else {
        dateInput(ns("task_due"), "Due Date")
      }
    })
    
    # Staff Matrix
    placeholder_staff <- c("Alice", "Bob", "Charlie")
    matrix_data <- reactiveVal(list())
    
    observeEvent(input$add_row, {
      tasks <- task_list()
      task_names <- tasks$Name[tasks$Type == "Task"]
      rows <- matrix_data()
      new_row <- list(
        staff = placeholder_staff[1],
        rate = 100,
        hours = setNames(as.list(rep(0, length(task_names))), task_names)
      )
      matrix_data(append(rows, list(new_row)))
    })
    
    observeEvent(input$remove_row, {
      rows <- matrix_data()
      if (length(rows) > 0) matrix_data(rows[-length(rows)])
    })
    
    output$assignment_matrix <- renderUI({
      tasks <- task_list()
      task_names <- tasks$Name[tasks$Type == "Task"]
      rows <- matrix_data()
      ns <- session$ns
      
      if (length(task_names) == 0 || length(rows) == 0) return(NULL)
      
      ui_list <- list(
        fluidRow(
          column(3, strong("Staff")),
          column(2, strong("Rate")),
          lapply(task_names, function(t) column(1, strong(t)))
        )
      )
      
      for (i in seq_along(rows)) {
        row <- rows[[i]]
        ui_list[[i + 1]] <- fluidRow(
          column(3, selectInput(ns(paste0("staff_", i)), NULL, choices = placeholder_staff, selected = row$staff)),
          column(2, numericInput(ns(paste0("rate_", i)), NULL, value = row$rate, min = 0)),
          lapply(task_names, function(t) {
            val <- row$hours[[t]] %||% 0
            column(1, numericInput(ns(paste0("hours_", i, "_", t)), NULL, value = val, min = 0))
          })
        )
      }
      
      tagList(ui_list)
    })
    
    # Preserve values on redraw
    observe({
      tasks <- task_list()
      task_names <- tasks$Name[tasks$Type == "Task"]
      rows <- matrix_data()
      ns <- session$ns
      
      updated_rows <- lapply(seq_along(rows), function(i) {
        staff <- input[[ns(paste0("staff_", i))]] %||% rows[[i]]$staff
        rate  <- input[[ns(paste0("rate_", i))]] %||% rows[[i]]$rate
        hours <- lapply(task_names, function(t) {
          input[[ns(paste0("hours_", i, "_", t))]] %||% rows[[i]]$hours[[t]] %||% 0
        })
        names(hours) <- task_names
        list(staff = staff, rate = rate, hours = hours)
      })
      matrix_data(updated_rows)
    })
  })
}
