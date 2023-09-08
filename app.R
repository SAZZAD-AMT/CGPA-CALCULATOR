# Load necessary libraries
library(shiny)
library(shinymaterial)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)  # Added shinyWidgets library
library(dplyr)

# Define custom CSS
css <- "
body {
  background-color: #f7f7f7;
  font-family: 'Helvetica Neue', sans-serif;
}

.sidebar {
  background-color: #333;
  color: white;
}

.main-header {
  background-color: #333;
  color: white;
}

.sidebar-menu .treeview-menu > li.active > a {
  background-color: #555;
}

.box {
  border-radius: 5px;
  box-shadow: 0 0 5px rgba(0, 0, 0, 0.2);
}

.btn-primary {
  background-color: #007bff;
  border-color: #007bff;
}

.btn-primary:hover {
  background-color: #0056b3;
  border-color: #0056b3;
}

#calculate {
  margin-top: 20px;
}

#download_pdf {
  margin-top: 20px;
}

#best_wishes {
  font-style: italic;
  color: #888;
}

#cgpa_result {
  font-size: 24px;
  font-weight: bold;
  color: #007bff;
}
"

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "CGPA Calculator", titleWidth = 250),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("History", tabName = "history", icon = icon("history"))
    ),
    tags$style(HTML(css))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "calculator",
        fluidPage(
          sidebarLayout(
            sidebarPanel(
              width = 500,
              numericInput("num_courses", "Number of Courses:", value = 3, min = 1),
              tags$hr(),
              uiOutput("course_info"),
              actionButton("calculate", "Calculate CGPA", class = "btn-primary"),
              downloadButton("download_pdf", "Download as PDF", class = "btn-primary")
            ),
            mainPanel()
          )
        )
      ),
      tabItem(
        tabName = "history",
        dataTableOutput("history_table")
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  history_data <- reactiveVal(data.frame(Time = character(), CGPA = numeric()))
  
  output$course_info <- renderUI({
    num_courses <- input$num_courses
    course_info <- lapply(1:num_courses, function(i) {
      fluidRow(
        column(4,
          selectInput(inputId = paste0("course_grade_", i),
                      label = paste("Course", i, "Grade:"),
                      choices = c("A+"= 4.0, "A" = 4.0, "A-" = 3.7, "B+" = 3.3, "B" = 3.0, "B-" = 2.7, "C+" = 2.3, "C" = 2.0, "C-" = 1.7, "D" = 1.0, "F" = 0),
                      selected = "A")
        ),
        column(4,
          numericInput(inputId = paste0("course_credits_", i),
                       label = paste("Course", i, "Credits:"),
                       value = 3, max = 4.5, min = 1, step = .5)
        )
      )
    })
    do.call(tagList, course_info)
  })
  
  observeEvent(input$calculate, {
    grades <- sapply(1:input$num_courses, function(i) as.numeric(input[[paste0("course_grade_", i)]]))
    credits <- sapply(1:input$num_courses, function(i) as.numeric(input[[paste0("course_credits_", i)]]))
    
    # Calculate GPA based on the selected grade points and credits
    gpacrs <- grades * credits
    total_credits <- sum(credits)
    total_gpacr <- sum(gpacrs)
    cgpa <- total_gpacr / total_credits
    
    # Add results to the history_data
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    history_data_data <- history_data()
    history_data_data <- rbind(history_data_data, data.frame(Time = timestamp, CGPA = cgpa))
    history_data(history_data_data)
    
    # Show CGPA in a modal dialog
    showModal(modalDialog(
      title = div(style = "text-align: center; font-weight: bold; background-color: #007bff; color: white;",
                   "CGPA Result"),
      div(style = "text-align: center;", h4(paste("Your CGPA is:", round(cgpa, 2)))),
      footer = NULL,
      easyClose = TRUE,
      div(style = "text-align: center; color: #007bff; font-weight: bold;", h1("Best Wishes"))
    ))
  })
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("cgpa_report.pdf")
    },
    content = function(file) {
      pdf(file, width = 8, height = 6)
      cat("CGPA Report\n\n")
      cat("Your CGPA is:", round(cgpa, 2), "\n")
      cat("Course Details:\n")
      for (i in 1:input$num_courses) {
        cat("Course", i, "Grade:", input[[paste0("course_grade_", i)]], "\n")
        cat("Course", i, "Credits:", input[[paste0("course_credits_", i)]], "\n\n")
      }
      dev.off()
      file.copy("cgpa_report.pdf", file)
    }
  )
  
  output$history_table <- renderDataTable({
    history_data()
  })
}

# Run the Shiny app
shinyApp(ui, server, options = list(sessionTimeout = 60*60)) # Set to 1 hour

