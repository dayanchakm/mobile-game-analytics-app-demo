library(shiny)

ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(
    theme = shinytheme("paper"),
    HTML(
      '<a style="text-decoration:none;cursor:default;color:#000000;" class="active" href="#">Khazar Analytics</a>'
    ),
    id = "nav",
    windowTitle = "Khazar Analytics",
    tabPanel("Analytics", 
             div(
               shinybrowser::detect(),
               class = "outer",
               tags$head(includeCSS("styles.css")),
               uiOutput("frame")
             )),
    tabPanel(title = "Models")
  )
)



server <- function(input, output) {
  output$frame <- renderUI({
    tags$iframe(src = "https://lookerstudio.google.com/embed/reporting/ffaf50c7-103d-4e84-b74b-e3f4c487dd67/page/DjD",
                width = "100%",
                height = shinybrowser::get_height()-50)
  })
  
}

shinyApp(ui, server)
