library(shiny)

ui <- fluidPage(
  tags$img(height = 100,
           width = 100,
           src = "cat.jpg"),
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "write a title",
            value = "Histogram of Random Normal Values"),
  
  plotOutput("hist")
)

server <- function(input, output) {
  data <- reactive({rnorm(input$num)})
  output$hist <- renderPlot({
    hist(data(), 
         main = isolate({input$title}));
    lines(density(data()))
  })
}

shinyApp(ui = ui, server = server)
