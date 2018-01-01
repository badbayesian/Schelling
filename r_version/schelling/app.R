library(shiny)
library(shinycssloaders)

ui <- fluidPage(

   titlePanel("Schelling Segregation Model"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput(inputId = "height", label = "Height",
                     min = 10,
                     max = 100,
                     value = 50),
         sliderInput(inputId = "width", label = "Width",
                     min = 10,
                     max = 100,
                     value = 100),
         sliderInput(inputId = "tolerance", label = "Tolerance",
                     min = 0,
                     max = 1,
                     value = 0.33),
         textInput(inputId = "race_distribution", label = "Race Distribution",
                   value = "0.5, 0.5"),
         sliderInput(inputId = "filled", label = "Spots Filled",
                     min = 0,
                     max = 1,
                     value = 0.95),
         actionButton(inputId = "simulate", label = "Simulate"),
         actionButton(inputId = "reset", label = "Reset")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Schelling Simulation",
                             withSpinner(plotOutput("schelling_plot"))),
                    tabPanel("Satisfaction",
                             plotOutput("satisfaction_plot")))
      )
   )
)

server <- function(input, output) {
  source(file = paste0(getwd(), "/schelling.R"))
  
  board <- reactiveValues(data = init_board())
   
   observeEvent(
     eventExpr = input$simulate,
     handlerExpr = {
       board$data = schelling(board$data, tolerance = input$tolerance)}
     )
   
   observeEvent(
     eventExpr = input$reset,
     handlerExpr = {
       races <- as.numeric(strsplit(input$race_distribution, ",")[[1]])
       board$data = init_board(height = input$height, width = input$width,
                               race_distribution = races,
                               filled = input$filled)}
     )

   output$schelling_plot <- renderPlot({
     plot_board(board$data)
     })
   
   output$satisfaction_plot <- renderPlot({
     plot_satisfaction_board(board$data)
   })
}

shinyApp(ui = ui, server = server)