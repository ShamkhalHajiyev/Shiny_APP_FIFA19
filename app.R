# 1. PACKAGES -------------------------------------------------------------
source("Packages/packages.R")


# 2. GLOBAL -------------------------------------------------------------
source("Global/Global.R")



# User Interface
ui <-
    navbarPage("FIFA Visualization",
               ###Player Tab
               tabPanel(
                   "Player",
                   fluidRow(
                       
                       column(
                           width = 2,
                           boxPad(h2("Main Player")),
                           pickerInput("tp_league", "League:", choices = ""),
                           pickerInput("tp_team", "Team:", choices =""),
                           pickerInput("tp_player", "Player:", choices ="")
                           
                       ),
                       
                       column(
                           width = 8,
                           fluidRow(
                               column(width = 4),
                               column(width = 2, uiOutput("PlayerImg")), 
                               column(width = 4)
                           ),
                           
                           br(),
                           
                           column(offset = 1, width = 12,
                                  fluidRow(
                                      valueBoxOutput("tp_age",width = 3),
                                      valueBoxOutput("tp_overall",width = 2),
                                      valueBoxOutput("tp_value",width = 2),
                                      valueBoxOutput("tp_contract",width = 3)
                                  )
                           )
                           
                       ),
                       
                       column(
                           width = 2, 
                           boxPad(
                               h2("Find Compare Players")),
                           pickerInput("tp_league2", "League:", choices = ""),
                           pickerInput("tp_team2", "Team:", choices =""),
                           sliderInput(
                               "overall",
                               "Overall:",
                               min = 50,
                               max = 100,
                               value = c(50, 100)
                           ),
                           sliderInput(
                               "height",
                               "Height (cm):",
                               min = 155,
                               max = 203,
                               value = c(155, 203)
                           ),
                           submitButton("Search")
                       )
                   ),
                   
                   tags$hr(),
                   tags$br(),
                   
                   conditionalPanel(
                       condition = "input.start",
                       
                       column(
                           width = 12, 
                           withSpinner(plotlyOutput("tp_radar"))
                           
                       )
                   )
               ))


# Server
server <- function(input, output, session) {
    # Player Tab
    # Reactive
    
    rvPlayer <- reactiveValues(League = NULL, Team = NULL, Player = NULL,
                               League2 = NULL, Team2 = NULL, Player2 = NULL)
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)
