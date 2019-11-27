library(shiny)

ui <- fluidPage(
  HTML("<div class="container-fluid">
    <h1>My Shiny App</h1>
    <p style="font-family:Impact">
    See other apps in the
<a href="http://www.rstudio.com/products/shiny-user-showcase/">Shiny Showcase</a>
  </p>
  </div>")

)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)
