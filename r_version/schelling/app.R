library(shiny)

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
         sliderInput(inputId = "num_of_races", label = "Number of Races",
                     min = 2,
                     max = 5,
                     value = 3),
         sliderInput(inputId = "tolerance", label = "Tolerance",
                     min = 0,
                     max = 1,
                     value = 0.33),
         actionButton(inputId = "submit", label = "Submit"),
         actionButton(inputId = "reset", label = "Reset")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Schelling Simulation",
                             plotOutput("schelling_plot")),
                    tabPanel("Satisfaction",
                             plotOutput("satisfaction_plot")))
      )
   )
)

server <- function(input, output) {
  source(file = paste0(getwd(), "/schelling.R"))
  
  board <- reactiveValues(data = init_board())
   
   observeEvent(
     eventExpr = input$submit,
     handlerExpr = {
       board$data = schelling(board$data, tolerance = input$tolerance)}
     )
   
   observeEvent(
     eventExpr = input$reset,
     handlerExpr = {
       board$data = init_board(height = input$height, width = input$width,
                               num_of_races = input$num_of_races)}
     )

   output$schelling_plot <- renderPlot({
     plot_board(board$data)
     })
   
   output$satisfaction_plot <- renderPlot({
     plot_satisfaction_board(board$data)
   })
}

shinyApp(ui = ui, server = server)