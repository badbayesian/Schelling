library(shiny)
library(shinydashboard)
library(shinycssloaders)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Schelling Segregation Model",
                  titleWidth = "100%"),
  dashboardSidebar(
    tabsetPanel(type = 'tabs',
                tabPanel("Original Schelling",
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
                         sliderInput(inputId = "filled",
                                     label = "Spots Filled",
                                     min = 0,
                                     max = 1,
                                     value = 0.95),
                         sliderInput(inputId = 'size',
                                     label = "Size for Plot",
                                     min = 1,
                                     max = 10,
                                     value = 4)
                ),
                tabPanel("Extending Schelling",
                         sliderInput(inputId = "neighborhood_size",
                                     label = "Neighborhood Size",
                                     min = 1,
                                     max = 5,
                                     value = 1),
                         textInput(inputId = "race_distribution",
                                   label = "Race Distribution",
                                   value = "0.5, 0.5"),
                         textInput(inputId = "wealth_distribution",
                                   label = "Wealth Distribution",
                                   value = "0.10, 0.70, 0.20"),
                         textInput(inputId = "business_center",
                                   label = "Business Center",
                                   value = "1, 1"),
                         sliderInput(inputId = "max_race_penalty",
                                     label = "Max Race Penalty",
                                     min = 0,
                                     max = 1,
                                     value = 1),
                         sliderInput(inputId = "max_wealth_penalty",
                                     label = "Max Wealth Penalty",
                                     min = 0,
                                     max = 1,
                                     value = 0),
                         sliderInput(inputId = "max_distance_penalty",
                                     label = "Max Distance Penalty",
                                     min = 0,
                                     max = 1,
                                     value = 0)
                )
    ),

    checkboxInput(inputId = "show_wealth",
                  label = "Show wealth distribution",
                  value = FALSE),
    actionButton(inputId = "simulate", label = "Simulate"),
    actionButton(inputId = "reset", label = "Reset"),
    width = '18%'

  ),

  dashboardBody(
    tabsetPanel(type = "tabs", 
                tabPanel("Schelling Simulation",
                         withSpinner(plotOutput("schelling_plot",
                                                width = "82%",
                                                height = "600px"))),
                tabPanel("Satisfaction",
                         plotOutput("satisfaction_plot",
                                    width = "82%",
                                    height = "600px"))
    )
  )
)
  
 

server <- function(input, output) {
  source(file = paste0(getwd(), "/schelling.R"))
  
  board <- reactiveValues(data = init_board())
   
   observeEvent(
     eventExpr = input$simulate,
     handlerExpr = {
       business_center <- as.numeric(strsplit(input$business_center, ",")[[1]])
       board$data = schelling(board$data,
                              neighborhood_size = input$neighborhood_size,
                              tolerance = input$tolerance,
                              business_center = business_center,
                              max_race_penalty = input$max_race_penalty,
                              max_wealth_penalty = input$max_wealth_penalty,
                              max_distance_penalty = input$max_distance_penalty
                              )
       }
     )
   
   observeEvent(
     eventExpr = input$reset,
     handlerExpr = {
       races <- as.numeric(strsplit(input$race_distribution, ",")[[1]])
       wealth <- as.numeric(strsplit(input$wealth_distribution, ",")[[1]])
       board$data = init_board(height = input$height,
                               width = input$width,
                               race_distribution = races,
                               wealth_distribution = wealth,
                               filled = input$filled)
       }
     )

   output$schelling_plot <- renderPlot({
     plot_board(board$data, size = input$size, show_wealth = input$show_wealth)
     })
   
   output$satisfaction_plot <- renderPlot({
     plot_satisfaction_board(board$data, size = input$size)
   })
}

shinyApp(ui = ui, server = server)