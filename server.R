  function(input, output, session) {
  ########## Compare Players
  observeEvent(input$league1, {
    clubs1 <- md %>%
      filter(League == input$league1) %>%
      pull(Club)
    
    
    clubs_updated1 = sort(unique(clubs1))
    
    
    updatePickerInput(
      session,
      inputId = "team1",
      choices =  clubs_updated1,
      selected = clubs_updated1[1]
    )
  })
  observeEvent(input$league2, {
    clubs2 <- md %>%
      filter(League == input$league2) %>%
      pull(Club)
    
    
    clubs_updated2 = sort(unique(clubs2))
    
    
    updatePickerInput(
      session,
      inputId = "team2",
      choices =  clubs_updated2,
      selected = clubs_updated2[1]
    )
  })
  
  observeEvent(input$team1, {
    clubs1 <- md %>%
      filter(League == input$league1 & Club == input$team1) %>%
      pull(Name)
    
    
    players1 = sort(unique(clubs1))
    
    
    updatePickerInput(
      session,
      inputId = "player1",
      choices =  players1,
      selected = players1[1]
    )
    
  })
  
  observeEvent(input$team2, {
    clubs2 <- md %>%
      filter(League == input$league2 & Club == input$team2) %>%
      pull(Name)
    
    
    players2 = sort(unique(clubs2))
    
    
    updatePickerInput(
      session,
      inputId = "player2",
      choices =  players2,
      selected = players2[2]
    )
    
  })
  
  output$age1 <- renderValueBox({
    valueBox("Age", md$Age[md$Name == input$player1])
  })
  output$age2 <- renderValueBox({
    valueBox("Age", md$Age[md$Name == input$player2])
  })
  output$height1 <- renderValueBox({
    valueBox("Height cm", md$Height[md$Name == input$player1])
  })
  output$height2 <- renderValueBox({
    valueBox("Height cm", md$Height[md$Name == input$player2])
  })
  output$weight1 <- renderValueBox({
    valueBox("Weight kg", md$Weight[md$Name == input$player1])
  })
  output$weight2 <- renderValueBox({
    valueBox("Weight kg", md$Weight[md$Name == input$player2])
  })
  output$nationality1 <- renderValueBox({
    valueBox("Nationality", md$Nationality[md$Name == input$player1])
  })
  output$nationality2 <- renderValueBox({
    valueBox("Nationality", md$Nationality[md$Name == input$player2])
  })
  output$overall1 <- renderValueBox({
    valueBox("Overall", md$Overall[md$Name == input$player1])
  })
  output$overall2 <- renderValueBox({
    valueBox("Overall", md$Overall[md$Name == input$player2])
  })
  output$value1 <- renderValueBox({
    valueBox("Value", md$value_currency[md$Name == input$player1])
  })
  output$value2 <- renderValueBox({
    valueBox("Value", md$value_currency[md$Name == input$player2])
  })
  output$wage1 <- renderValueBox({
    valueBox("Wage", md$wage_currency[md$Name == input$player1])
  })
  output$wage2 <- renderValueBox({
    valueBox("Wage", md$wage_currency[md$Name == input$player2])
  })
  output$preferredleg1 <- renderValueBox({
    valueBox("Preferred Foot", md$Preferred.Foot[md$Name == input$player1])
  })
  output$preferredleg2 <- renderValueBox({
    valueBox("Preferred Foot", md$Preferred.Foot[md$Name == input$player2])
  })
  output$position1 <- renderValueBox({
    valueBox("Position", md$Position[md$Name == input$player1])
  })
  output$position2 <- renderValueBox({
    valueBox("Position", md$Position[md$Name == input$player2])
  })
  output$class1 <- renderValueBox({
    valueBox("Class", md$Class[md$Name == input$player1])
  })
  output$class2 <- renderValueBox({
    valueBox("Class", md$Class[md$Name == input$player2])
  })
  output$contract1 <- renderValueBox({
    valueBox("Contract", md$Contract.Valid.Until[md$Name == input$player1])
  })
  output$contract2 <- renderValueBox({
    valueBox("Contract", md$Contract.Valid.Until[md$Name == input$player2])
  })
  
  
  output$radarplayers <- renderPlotly({
    radar <- md %>%
      transmute(
        Name,
        League,
        Club,
        Speed = (SprintSpeed + Agility + Acceleration) / 3,
        Power = (Strength + Stamina + Balance) / 3,
        Technic = (BallControl + Dribbling + Vision) / 3,
        Attack = (Finishing + ShotPower + LongShots + Curve) / 4,
        Defence = (Marking + StandingTackle + SlidingTackle) / 3
      )
    radarP1 <- radar %>% filter(Name %in% input$player1)
    radarP2 <- radar %>% filter(Name %in% input$player2)
    
    plot_ly(type = 'scatterpolar',
            fill = 'toself') %>%
      add_trace(
        r = c(
          radarP1 %>% pull(Speed),
          radarP1 %>% pull(Power),
          radarP1 %>% pull(Technic),
          radarP1 %>% pull(Attack),
          radarP1 %>% pull(Defence)
        ),
        theta = c('Speed', 'Power', 'Technic', 'Attack', 'Defence'),
        name = input$player1
      ) %>%
      add_trace(
        r = c(
          radarP2 %>% pull(Speed),
          radarP2 %>% pull(Power),
          radarP2 %>% pull(Technic),
          radarP2 %>% pull(Attack),
          radarP2 %>% pull(Defence)
        ),
        theta = c('Speed', 'Power', 'Technic', 'Attack', 'Defence'),
        name = input$player2
      ) %>%
      layout(polar = list(radialaxis = list(
        visible = T,
        range = c(0, 100)
      )))
    
  })
  
  

  
  
  output$histogramplayer1 <- renderPlotly({
    ds_p1 <- md %>% filter(Name %in% input$player1)
    facetReactiveBar(ds_p1, fill_variable = "#dd4b39", fill_strip = "firebrick")
    
    
  })
  output$histogramplayer2 <- renderPlotly({
    ds_p2 <- md %>% filter(Name %in% input$player2)
    facetReactiveBar(ds_p2, fill_variable = "royalblue", fill_strip = "navy")
    
  })
  
  ### League Stats
  observeEvent(input$leagues,
               {
                 output$values <- renderValueBox({
                   valueBox(
                     "Total value of league",
                     value = md %>% filter(League %in% input$leagues) %>%
                       summarise(total = paste("€", round(sum(Value)/1000, digits = 2), "B")) %>% pull(total)
                   )
                   
                 })
                 output$numofplayers <- renderValueBox({
                   valueBox(
                     "Total Number of Players",
                     value = md %>% filter(League %in% input$leagues) %>% select(Club) %>% nrow()
                   )
                 })
                 output$teams <- renderValueBox({
                   valueBox(
                     "Total Number of Clubs",
                     value = md %>% filter(League %in% input$leagues) %>% select(Club) %>% distinct() %>% nrow()
                   )
                 })
                 
                 

                 

                 output$best_team <- renderTable({
                   bestLeague <-
                     md %>% filter(League %in% input$leagues)
                   
                   
                   leagueTeam <- best_team(bestLeague, input = input$leaguetactic)
                   
                   leagueTeam %>%
                     add_row(
                       Name = "Average Overall:",
                       Overall = round(mean(leagueTeam$Overall), digits = 2),
                       Position = "ALL"
                     ) %>%
                     mutate(
                       Jersey.Number = as.character(Jersey.Number),
                       Club = as.character(Club),
                       Jersey.Number = if_else(is.na(Jersey.Number), "", Jersey.Number),
                       Club = if_else(is.na(Club), "", Club)
                     ) %>%
                     rename_all(funs(gsub("[[:punct:]]", " ", .)))
                 })
                 
                 
                 output$league_besttactic <- renderPlot(width = "auto", {
                   bestLeague <-
                     md %>% filter(League %in% input$leagues)
                     leagueTeam <- best_team(bestLeague, input = input$leaguetactic)
                     
                     
                     pitch_custom <- list(
                       length = 150,
                       width = 100,
                       penalty_box_length = 20,
                       penalty_box_width = 50,
                       six_yard_box_length = 8,
                       six_yard_box_width = 26,
                       penalty_spot_distance = 16,
                       goal_width = 12,
                       origin_x = 0,
                       origin_y = 0
                     )
       
                     if (input$leaguetactic == "4-2-3-1"){
                       ggplot() +
                         annotate_pitch(fill = "#038253", colour = "white", dimensions = pitch_custom) +
                         annotate("text", x = 4, y = 50, label = leagueTeam$Name[leagueTeam$Position == "GK"])+
                         annotate("text", x = 30, y = 12.5, label = leagueTeam$Name[leagueTeam$Position == "RB"])+
                         annotate("text", x = 25, y = 34, label = leagueTeam$Name[leagueTeam$Position == "CB"][1])+
                         annotate("text", x = 25, y = 67, label = leagueTeam$Name[leagueTeam$Position == "CB"][2])+
                         annotate("text", x = 30, y = 87.5, label = leagueTeam$Name[leagueTeam$Position == "LB"])+
                         annotate("text", x = 60, y = 65, label = leagueTeam$Name[leagueTeam$Position == "CM"][1])+
                         annotate("text", x = 60, y = 35, label = leagueTeam$Name[leagueTeam$Position == "CM"][2])+
                         annotate("text", x = 100, y = 50, label = leagueTeam$Name[leagueTeam$Position == "CAM"])+
                         annotate("text", x = 110, y = 87.5, label = leagueTeam$Name[leagueTeam$Position == "LW"])+
                         annotate("text", x = 110, y = 12.5, label = leagueTeam$Name[leagueTeam$Position == "RW"])+
                         annotate("text", x = 125, y = 50, label = leagueTeam$Name[leagueTeam$Position == "CF"])+
                         theme_pitch()
                     } 
                     else if(input$leaguetactic == "3-5-2"){
                       ggplot() +
                         annotate_pitch(fill = "#038253", colour = "white", dimensions = pitch_custom) +
                         annotate("text", x = 4, y = 50, label = leagueTeam$Name[leagueTeam$Position == "GK"])+
                         annotate("text", x = 25, y = 25, label = leagueTeam$Name[leagueTeam$Position == "CB"][1])+
                         annotate("text", x = 25, y = 50, label = leagueTeam$Name[leagueTeam$Position == "CB"][2])+
                         annotate("text", x = 25, y = 75, label = leagueTeam$Name[leagueTeam$Position == "CB"][3])+
                         annotate("text", x = 85, y = 87.5, label = leagueTeam$Name[leagueTeam$Position == "LM"])+
                         annotate("text", x = 70, y = 75, label = leagueTeam$Name[leagueTeam$Position == "CM"][1])+
                         annotate("text", x = 70, y = 50, label = leagueTeam$Name[leagueTeam$Position == "CM"][2])+
                         annotate("text", x = 70, y = 25, label = leagueTeam$Name[leagueTeam$Position == "CM"][3])+
                         annotate("text", x = 85, y = 12.5, label = leagueTeam$Name[leagueTeam$Position == "RM"])+
                         annotate("text", x = 135, y = 45, label = leagueTeam$Name[leagueTeam$Position == "ST"][1])+
                         annotate("text", x = 135, y = 55, label = leagueTeam$Name[leagueTeam$Position == "ST"][2])+
                         theme_pitch()
                     }
                     else{
                       ggplot() +
                         annotate_pitch(fill = "#038253", colour = "white", dimensions = pitch_custom) +
                         annotate("text", x = 4, y = 50, label = leagueTeam$Name[leagueTeam$Position == "GK"])+
                         annotate("text", x = 30, y = 12.5, label = leagueTeam$Name[leagueTeam$Position =="RB"])+
                         annotate("text", x = 25, y = 34, label = leagueTeam$Name[leagueTeam$Position=="CB"][1])+
                         annotate("text", x = 25, y = 67, label = leagueTeam$Name[leagueTeam$Position=="CB"][2])+
                         annotate("text", x = 30, y = 87.5, label = leagueTeam$Name[leagueTeam$Position=="LB"])+
                         annotate("text", x = 88, y = 65, label = leagueTeam$Name[leagueTeam$Position=="CM"][1])+
                         annotate("text", x = 88, y = 37, label = leagueTeam$Name[leagueTeam$Position=="CM"][2])+
                         annotate("text", x = 55, y = 50, label = leagueTeam$Name[leagueTeam$Position=="CDM"])+
                         annotate("text", x = 125, y = 87.5, label = leagueTeam$Name[leagueTeam$Position=="LW"])+
                         annotate("text", x = 125, y = 12.5, label = leagueTeam$Name[leagueTeam$Position=="RW"])+
                         annotate("text", x = 135, y = 50, label = leagueTeam$Name[leagueTeam$Position=="ST"])+
                         theme_pitch()
                     }
              

                 })
                 
                 output$league_nat <-  renderPlotly({
                   world_map <- map_data("world")
                   
                   numofplayers <- world_map %>%
                     mutate(region = as.character(region)) %>%
                     rename(Country = region) %>%
                     left_join((
                       md %>% mutate(
                         Nationality = as.character(Nationality),
                         Nationality = if_else(Nationality %in% "England", "UK", Nationality)
                       ) %>%
                         filter(League == input$leagues) %>%
                         count(Nationality, name = "Number of Player") %>%
                         rename(Country = Nationality) %>%
                         mutate(Country = as.character(Country))
                     ),
                     by = "Country")
                   
                   ggplotly(
                     ggplot(numofplayers, aes(long, lat, group = group)) +
                       geom_polygon(
                         aes(label = `Country`, fill = `Number of Player`),
                         color = "white",
                         show.legend = FALSE
                       ) +
                       scale_fill_viridis_c(option = "C") +
                       theme_void() +
                       labs(fill = "Number of Players:",
                            title = "Nationality of The Players in The League")
                   )
                   
                 })
                 
  
                 output$league_players <- renderPlotly({
                   
                   
                   md_league_players <-md %>%
                     filter(League %in% input$leagues & Class %in% input$leagueclass) %>%
                     mutate(Name = reorder(Name, Value)) %>%
                     group_by(Class) %>% 
                     top_n(n = 20, wt = Value)
                   
                   ggplotly(
                       ggplot(md_league_players, aes(Value,
                                  Name,
                                  size = Overall,
                                  color = Position)) +
                       geom_point() +
                       facet_wrap(Class ~ ., scales = "free") +
                       theme(
                         strip.background = element_rect(fill = "gray"),
                         strip.text.x = element_text(
                           size = 10,
                           colour = "white",
                           face = "bold.italic"
                         )
                       ) +
                       theme_minimal() + labs(title = "Most Valuable Players", x = "Million €", y = NULL
                       )
                   )
                   
                 })
                 
               })
  
                output$league_comp <- renderPlotly({
                  if(input$comp_league == "League"){
                    ggplotly(
                      md %>% 
                        group_by(League) %>% 
                        summarise(Total.Value = sum(as.double(Value), na.rm = TRUE),
                                  Average.Overall = mean(as.integer(Overall), na.rm = TRUE)) %>% 
                        ggplot(aes(reorder(League, Total.Value), Total.Value, fill = Average.Overall))+
                        geom_col(show.legend = F)+
                        coord_flip()+
                        theme_minimal()+ 
                        labs(x = NULL, y = "League Total Value")+
                        scale_y_continuous(labels = c("0 €", "2 Billion €", "4 Billion €", "6 Billion €"))
                    )
                  }
                  else if(input$comp_league == "Team Total Value"){
                    
        
                    ggplotly(
                      md %>%
                        filter(League == input$leagues) %>% 
                        group_by(Club) %>% 
                        summarise(Total.Value = sum(as.double(Value), na.rm = TRUE),
                                  Average.Overall = mean(as.integer(Overall), na.rm = TRUE)) %>% 
                        ggplot(aes(reorder(Club, Total.Value), Total.Value, fill = Average.Overall))+
                        geom_col(show.legend = F)+
                        coord_flip()+
                        theme_minimal()+
                        labs(x = NULL, y = "Team Total Value") +
                        scale_y_continuous(breaks = c(0,200,400,600,800),
                                           labels = c("0 €", "200 Million €", "400 Million €", "600 Million €", "800 Million €"))
                    )

                  }
                  else if(input$comp_league == "Position"){
                    ggplotly(
                      md %>%
                        filter(League %in% input$leagues) %>% 
                        group_by(Class) %>%
                        summarise(Total.Value = sum(as.double(Value), na.rm = TRUE),
                                  Average.Overall = mean(as.integer(Overall), na.rm = TRUE)) %>% 
                        ggplot(aes(reorder(Class, Total.Value), Total.Value, fill = Average.Overall))+
                        geom_col(show.legend = F)+
                        coord_flip()+
                        theme_minimal()+
                        labs(x = NULL, y = "Position Total Value") + 
                        scale_y_continuous(breaks = c(0,500,1000,1500,2000),
                          labels = c("0 €", "0.5 Billion €", "1 Billion €", "1.5 Billion €", "2 Billion €"))
                    )
                  }
                  else{
                    ggplotly(
                      md %>% 
                        filter(League %in% input$leagues) %>%
                        group_by(Club, Class) %>% 
                        summarise(mean = mean(Overall)) %>% 
                        ungroup() %>% 
                        filter(Club %in% md$Club) %>% 
                        ggplot(aes(reorder(Club, mean), mean, fill = Class))+
                        geom_col(position = "fill")+
                        geom_text(aes(label = round(mean,digits = 2)), position = position_fill(0.5))+
                        coord_flip()+
                        theme_minimal()+
                        theme(legend.position = "top")+
                        labs(x = NULL, y = NULL, title = "Team Overall for every position class")
                    )
                  }
                })
                output$table = DT::renderDataTable({
                  if(input$Dataset == "Original"){
                    fifa19
                  }
                  else{
                    md %>% 
                      select(-1)
                  }
                })
  
  
}
