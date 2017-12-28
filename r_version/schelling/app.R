#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  source(file = paste0(getwd(), "/schelling.R"))
  
  grid <- schelling()

   output$distPlot <- renderPlot({

     
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

# Run the application 
shinyApp(ui = ui, server = server)

