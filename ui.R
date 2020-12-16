source("Global/Global1.R")
#source("Functions/Functions.R")

#source("Libraries/Library.R")

shinyUI(
    fluidPage(
    
 
    
    navbarPage(
        "Fifa Visualization!",
        tabPanel("Player",
                 fluidRow(
                     column(
                         width = 2,
                         boxPad(h2("Select Player")),
                         pickerInput(inputId = "tp_league", "League:", choices = sort(unique(md$League))), 
                         pickerInput(inputId = "tp_team", "Team:", choices = ""),
                         pickerInput(inputId = "tp_player", "Player:", choices = "")
                        
                     ),
                     column(width = 10,
                            boxPad(h2("Details of Player")),
                                valueBoxOutput("tp_age",width = 2),
                                valueBoxOutput("tp_overall",width = 2),
                                valueBoxOutput("tp_value",width = 2),
                                valueBoxOutput("tp_contract",width = 2),
                                valueBoxOutput("tp_nationality",width = 2)
                     )
                 ),
                 fluidRow(
                     column(
                         width = 12,
                         plotlyOutput("tp_radar")
                     )
                 )
                 ),
        tabPanel("World",
                 verbatimTextOutput("summary")),
        navbarMenu(
            "More",
            tabPanel("Developers",
                     DT::dataTableOutput("table")),
            tabPanel("Report")
        )
    )
    
    
))
