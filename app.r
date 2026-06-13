install.packages(c("shiny", "dplyr", "ggplot2", "plotly"),
                 repos = "https://cloud.r-project.org")
library(shiny)

ui <- fluidPage(
  titlePanel("CSV analizės app"),

  fileInput("file", "Įkelk CSV failą", accept = ".csv"),

  tableOutput("table"),

  plotOutput("plot")
)

server <- function(input, output, session) {

  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  output$table <- renderTable({
    req(data())
    head(data(),10)
  })

 output$plot <- renderPlot({
  req(data())

  df <- data()

  num_cols <- names(df)[sapply(df, is.numeric)]

  req(length(num_cols) >= 2)

  x <- num_cols[1]
  y <- num_cols[2]

  plot(df[[x]], df[[y]],
       xlab = x,
       ylab = y,
       main = "CSV grafikas")
})
}

shinyApp(ui, server)



