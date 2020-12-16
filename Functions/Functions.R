#####Functions

vbox_players <- function(md, variable = c('Age', 'Overall', 'Nationality', 'Value', 'Contract.Valid.Until')){
  
  res <- NULL
  
  if(missing("md") | missing("variable") | missing("color")) return(res)
  if(is.null(md) | is.null(variable) | is.null(color)) return(res)
  
  if(variable == 'Age'){
    icon_vb <- "street-view"
  }else if(variable == 'Overall'){
    icon_vb <- "battery-three-quarters"
  }else if(variable == 'Nationality'){
    icon_vb <- "flag"
  }else if(variable == 'Value'){
    icon_vb <- "wallet"
  }else if(variable == 'Contract.Valid.Until'){
    md <- md %>% rename(Contract = Contract.Valid.Until)
    variable <- "Contract"
    icon_vb <- "file-signature"
  }else{
    return(NULL)
  }
  
  res <- valueBox(
    value = tags$p(md %>% pull(!!rlang::parse_expr(variable)), style = "font-size: 80%;"),
    subtitle = variable,
    icon = icon(icon_vb)
  )
  
  return(res)
  
}