library(shiny)

ui <- fluidPage(
  tags$p("this is a Shiny app."),
  tags$hr(),
  tags$p("This ia s Shiny app."),
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)
