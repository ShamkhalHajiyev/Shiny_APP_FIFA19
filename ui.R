source("Global/Global1.R")

#source("Libraries/Library.R")

shinyUI(fluidPage(
    navbarPage(
        "Fifa Visualization!",
        tabPanel("Compare Players",
                 fluidRow(
                     column(
                         width = 2,
                         boxPad(h2("Select Player 1"), align = "center"),
                         pickerInput(inputId = "league1", "League:", choices = sort(unique(md$League))),
                         pickerInput(inputId = "team1", "Team:", choices = ""),
                         pickerInput(inputId = "player1", "Player:", choices = ""),
                         valueBoxOutput("age1", width = 2.4),
                         valueBoxOutput("height1", width = 2.4),
                         valueBoxOutput("weight1", width = 2.4),
                         valueBoxOutput("overall1", width = 2.4),
                         valueBoxOutput("value1", width = 2.4),
                         valueBoxOutput("wage1", width = 2.4),
                         valueBoxOutput("preferredleg1", width = 2.4),     
                         valueBoxOutput("position1", width = 2.4),
                         valueBoxOutput("class1", width = 2.4),
                         valueBoxOutput("contract1", width = 2.4),
                         valueBoxOutput("nationality1", width = 2.4)
                         
                     ),
                     column(width = 8,
                            tabsetPanel(
                                type = "pills",
                                tabPanel("Radar", plotlyOutput("radarplayers", height = "100%", width = "100%")),
                                tabPanel("Histogram", plotlyOutput("histogramplayers"))
                            )),
                     column(
                         width = 2,
                         boxPad(h2("Select Player 2"), align = "center"),
                         pickerInput(inputId = "league2", "League:", choices = sort(unique(md$League))),
                         pickerInput(inputId = "team2", "Team:", choices = ""),
                         pickerInput(inputId = "player2", "Player:", choices = ""),
                         valueBoxOutput("age2", width = 2.4),
                         valueBoxOutput("height2", width = 2.4),
                         valueBoxOutput("weight2", width = 2.4),
                         valueBoxOutput("overall2", width = 2.4),
                         valueBoxOutput("value2", width = 2.4),
                         valueBoxOutput("wage2", width = 2.4),
                         valueBoxOutput("preferredleg2", width = 2.4),
                         valueBoxOutput("position2", width = 2.4),
                         valueBoxOutput("class2", width = 2.4),
                         valueBoxOutput("contract2", width = 2.4),
                         valueBoxOutput("nationality2", width = 2.4)
                         
                         
                     )
                 )),
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
