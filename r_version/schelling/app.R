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
         actionButton(
           inputId = "submit_loc",
           label = "Submit")
      ),
      
      mainPanel(
         plotOutput("schelling_plot")
      )
   )
)

server <- function(input, output) {
  
  source(file = paste0(getwd(), "/schelling.R"))
  
  observeEvent(
    eventExpr = input[["submit_loc"]],
    handlerExpr = {
      print("PRESSED")
  
     output$schelling_plot <- renderPlot({
       
       grid <- schelling(height = input$height,
                         width = input$width,
                         tolerance = input$tolerance)
  
       plot <- ggplot(grid) +
         aes(x = width, y = height, color = as.factor(race)) +
         geom_point(size = 2.5) +
         labs(title = "Schelling", x = "", y = "", color = "Race") +
         theme_bw() +
         coord_cartesian(xlim = c(0, max(grid$width) + 1),
                         ylim = c(0, max(grid$height) + 1),
                         expand = FALSE)
       print(plot)
       })
    }
  )
}

shinyApp(ui = ui, server = server)