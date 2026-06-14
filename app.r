install.packages(c("shiny", "dplyr", "ggplot2", "plotly"),
                 repos = "https://cloud.r-project.org")
library(shiny)
library(ggplot2)

ui <- fluidPage(

  fileInput("file", "Įkelk CSV"),

  selectInput("x", "X ašis", choices = NULL),
  selectInput("y", "Y ašis", choices = NULL),
  selectInput("type", "Grafiko tipas",
              choices= c("scatter","line","bar")),

  tableOutput("table"),
  plotOutput("plot")
)

server <- function(input, output, session) {

  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  output$table <- renderTable({
    head(data(), 10)
  })

  observe({
    req(data())
    cols <- names(data())

    updateSelectInput(session, "x", choices = cols)
    updateSelectInput(session, "y", choices = cols)
  })

  output$plot <- renderPlot({
    req(data(), input$x, input$y, input$type)

    df <- data()

    p <- ggplot(df, aes(x = .data[[input$x]], y = .data[[input$y]]))

    if (input$type == "scatter") {
      p <- p + geom_point()
    } else if (input$type == "line") {
      p <- p + geom_line()
    } else if (input$type == "bar") {
      p <- p + geom_col()
    }

    p + theme_minimal()

  #   ggplot(df, aes(x = .data[[input$x]], y = .data[[input$y]])) +
  # geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  # geom_smooth(method = "lm", se = FALSE, color = "red") +
  # theme_minimal() +
  # labs(
  #   title = "CSV analizės grafikas",
  #   x = input$x,
  #   y = input$y
  # )
  })
}

shinyApp(ui, server)



