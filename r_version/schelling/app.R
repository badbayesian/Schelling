library(shiny)

ui <- fluidPage(

   titlePanel("Schelling Segregation Model"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput(inputId = "height",
                     label = "Height",
                     min = 10,
                     max = 100,
                     value = 50),
         sliderInput(inputId = "width",
                     label = "Width",
                     min = 10,
                     max = 100,
                     value = 100),
         sliderInput(inputId = "tolerance",
                     label = "Tolerance",
                     min = 0,
                     max = 1,
                     value = 0.33),
         actionButton(inputId = "submit_loc",
           label = "Submit"),
         actionButton(inputId = "reset",
           label = "Reset")
      ),
      
      mainPanel(
         plotOutput("schelling_plot")
      )
   )
)

server <- function(input, output) {
  
  source(file = paste0(getwd(), "/schelling.R"))
  

     output$schelling_plot <- renderPlot({
       board <- board(height = input$height, width = input$width)
       plot_board(board)
       
       observeEvent(
         eventExpr = input[["submit_loc"]],
         handlerExpr = {
           print("PRESSED")
           board <- schelling(board, tolerance = input$tolerance)
           plot_board(board)
           
           }
         )

       })
    }

shinyApp(ui = ui, server = server)