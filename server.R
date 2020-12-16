function(input, output, session) {
 
    observeEvent(input$tp_league, {
        
        clubs <- md %>%
            filter(League == input$tp_league)%>%
            pull(Club) 
        
        
        clubs_updated = sort(unique(clubs))
        
        
        updatePickerInput(session,
                          inputId = "tp_team",
                          choices =  clubs_updated,
                          selected = clubs_updated[1])
    })
    
    observeEvent(input$tp_team, {
        
        clubs <- md %>%
            filter(League == input$tp_league & Club == input$tp_team)%>%
            pull(Name) 
        
        
        players = sort(unique(clubs))
        
        
        updatePickerInput(session,
                          inputId = "tp_player",
                          choices =  players,
                          selected = players[1])
        
    })
    output$tp_age <- renderValueBox({
        valueBox("Age", md$Age[md$Name == input$tp_player])
    })
    output$tp_nationality <- renderValueBox({
        valueBox("Nationality", md$Nationality[md$Name == input$tp_player])
    })
    output$tp_overall <- renderValueBox({
        valueBox("Overall", md$Overall[md$Name == input$tp_player])
    })
    output$tp_value <- renderValueBox({
        valueBox("Value", md$value_currency[md$Name == input$tp_player])
    })
    output$tp_contract <- renderValueBox({
        valueBox("Contract", md$Contract.Valid.Until[md$Name == input$tp_player])
    })
    
    
    output$tp_radar <- renderPlotly({
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
            ) %>%
            filter(md$Name == input$tp_player)
        
        
        plot_ly(type = 'scatterpolar',
                fill = 'toself') %>%
            add_trace(
                r = c(
                    radar %>% pull(Speed),
                    radar %>% pull(Power),
                    radar %>% pull(Technic),
                    radar %>% pull(Attack),
                    radar %>% pull(Defence)
                ),
                theta = c('Speed', 'Power', 'Technic', 'Attack', 'Defence'),
                name = input$tp_player
            ) %>%
            layout(polar = list(radialaxis = list(
                visible = T,
                range = c(0, 100)
            )))
        
        
    })
    
    
    
    
}
