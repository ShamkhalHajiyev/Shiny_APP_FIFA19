best_team <- function(df, input) {
  team <- NULL
  team <- tibble()
  team_copy <-
    df %>% select(Jersey.Number, Name, Overall, Position, Club) %>% arrange(-Overall)
  
  tac4231 <-
    c("GK",
      "RB",
      "CB",
      "CB",
      "LB",
      "CM",
      "CM",
      "AM",
      "LW",
      "RW",
      "CF")
  tac352 <-
    c("GK",
      "CB",
      "CB",
      "CB",
      "RM",
      "CM",
      "CM",
      "CM",
      "LM",
      "ST",
      "ST")
  tac433 <-
    c("GK",
      "RB",
      "CB",
      "CB",
      "LB",
      "CM",
      "CDM",
      "CM",
      "LW",
      "RW",
      "ST")
  
  tactic <- if (input == "4-2-3-1") {
    tac4231
  } else if (input == "3-5-2") {
    tac352
  } else{
    tac433
  }
  
  for (i in tactic) {
    team %<>%  bind_rows(team_copy %>% filter(Position %in% i) %>% head(1))
    team_copy %<>% filter(!Name %in% (team %>% pull(Name)))
    
  }
  
  return(team)
  
}

facetReactiveBar = function(df, fill_variable, fill_strip) {
  res <- NULL
  
  if (missing("df") |
      missing("fill_variable") | missing("fill_strip"))
    return(res)
  if (is.null(df) |
      is.null(fill_variable) | is.null("fill_strip"))
    return(res)
  
  
  res <- df %>%
    select(Name, Crossing:SlidingTackle) %>%
    rename_all(funs(gsub("[[:punct:]]", " ", .))) %>%
    gather(Exp, Skill, Crossing:`SlidingTackle`,-`Name`) %>%
    ggplot(aes(Exp, Skill)) +
    geom_col(fill = fill_variable) +
    facet_wrap( ~ (df %>% pull(`Name`))) +
    labs(x = NULL, y = NULL) +
    theme(
      strip.background = element_rect(fill = fill_strip, color = "black"),
      strip.text.x = element_text(
        size = 10,
        colour = "white",
        face = "bold.italic"
      ),
      axis.text.x = element_text(angle = 90)
    )
  
  return(res)
  
}