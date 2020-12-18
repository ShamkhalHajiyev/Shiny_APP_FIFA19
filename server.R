function(input, output, session) {
    ########## Compare Players
    observeEvent(input$league1, {
        
        clubs1 <- md %>%
            filter(League == input$league1)%>%
            pull(Club) 
        
        
        clubs_updated1 = sort(unique(clubs1))
        
        
        updatePickerInput(session,
                          inputId = "team1",
                          choices =  clubs_updated1,
                          selected = clubs_updated1[1])
    })
    observeEvent(input$league2, {
        
        clubs2 <- md %>%
            filter(League == input$league2)%>%
            pull(Club) 
        
        
        clubs_updated2 = sort(unique(clubs2))
        
        
        updatePickerInput(session,
                          inputId = "team2",
                          choices =  clubs_updated2,
                          selected = clubs_updated2[1])
    })
    
    observeEvent(input$team1, {
        
        clubs1 <- md %>%
            filter(League == input$league1 & Club == input$team1)%>%
            pull(Name) 
        
        
        players1 = sort(unique(clubs1))
        
        
        updatePickerInput(session,
                          inputId = "player1",
                          choices =  players1,
                          selected = players1[1])
        
    })
    
    observeEvent(input$team2, {
        
        clubs2 <- md %>%
            filter(League == input$league2 & Club == input$team2)%>%
            pull(Name) 
        
        
        players2 = sort(unique(clubs2))
        
        
        updatePickerInput(session,
                          inputId = "player2",
                          choices =  players2,
                          selected = players2[2])
        
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
                fill = 'toself')%>%
            add_trace(
                r = c(radarP1 %>% pull(Speed),radarP1 %>% pull(Power), radarP1 %>% pull(Technic), radarP1 %>% pull(Attack), radarP1 %>% pull(Defence)),
                theta = c('Speed', 'Power', 'Technic', 'Attack', 'Defence'),
                name = input$player1
            ) %>%
            add_trace(
                r = c(radarP2 %>% pull(Speed),radarP2 %>% pull(Power), radarP2 %>% pull(Technic), radarP2 %>% pull(Attack), radarP2 %>% pull(Defence)),
                theta = c('Speed','Power','Technic', 'Attack', 'Defence'),
                name = input$player2
            ) %>%
            layout(polar = list(radialaxis = list(
                visible = T,
                range = c(0, 100)
            )))
        
    })

  
    facetReactiveBar = function(df, fill_variable, fill_strip){
        
        res <- NULL
        
        if(missing("df") | missing("fill_variable") | missing("fill_strip")) return(res)
        if(is.null(df) | is.null(fill_variable) | is.null("fill_strip")) return(res)
        
        
        res <- df %>% 
            select(Name, Crossing:SlidingTackle) %>% 
            rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
            gather(Exp, Skill, Crossing:`SlidingTackle`, -`Name`) %>% 
            ggplot(aes(Exp, Skill))+
            geom_col(fill = fill_variable)+
            facet_wrap(~(df %>% pull(`Name`)))+
            labs(x = NULL, y = NULL)+
            theme(strip.background =element_rect(fill=fill_strip,color = "black"),
                  strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"),
                  axis.text.x=element_text(angle=90))
        
        return(res)
        
    }    

    
    output$histogramplayer1 <- renderPlotly({
        ds_p1 <- md %>% filter(Name %in% input$player1)
        facetReactiveBar(ds_p1, fill_variable = "#dd4b39", fill_strip = "firebrick")
        
        
    })
    output$histogramplayer2 <- renderPlotly({
        ds_p2 <- md %>% filter(Name %in% input$player2)
        facetReactiveBar(ds_p2, fill_variable = "royalblue", fill_strip = "navy")
        
    })
    
    ### Compare League

    
    
    
}