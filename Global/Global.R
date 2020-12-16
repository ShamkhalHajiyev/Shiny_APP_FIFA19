###Packages 

pacotes = c("shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders","tidyverse",
            "scales", "knitr", "kableExtra", "ggfortify","dplyr","plotly","FNN")

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically

package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})



library(tidyverse) 
library(magrittr)
library(maps)
library(plotly)
library(DT)
library(tidytext)
library(gridExtra)
library(factoextra)

###Data 
fifa19 <- read.csv2("data/fifa2019.csv", sep  = ",")


head(fifa19)

sort(colSums(is.na(fifa19)))

row.has.na <- apply(fifa19, 1, function(x){any(is.na(x))})

sum(row.has.na)

fifa19_updated <- fifa19[!row.has.na,]

sort(colSums(is.na(fifa19_updated)))

### Creating the Leagues

bundesliga <- c(
  "1. FC Nürnberg", "1. FSV Mainz 05", "Bayer 04 Leverkusen", "FC Bayern München",
  "Borussia Dortmund", "Borussia Mönchengladbach", "Eintracht Frankfurt",
  "FC Augsburg", "FC Schalke 04", "Fortuna Düsseldorf", "Hannover 96",
  "Hertha BSC", "RB Leipzig", "SC Freiburg", "TSG 1899 Hoffenheim",
  "VfB Stuttgart", "VfL Wolfsburg", "SV Werder Bremen"
)

premierLeague <- c(
  "Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley",
  "Cardiff City", "Chelsea", "Crystal Palace", "Everton", "Fulham",
  "Huddersfield Town", "Leicester City", "Liverpool", "Manchester City",
  "Manchester United", "Newcastle United", "Southampton", 
  "Tottenham Hotspur", "Watford", "West Ham United", "Wolverhampton Wanderers"
  
)

laliga <- c(
  "Athletic Club de Bilbao", "Atlético Madrid", "CD Leganés",
  "Deportivo Alavés", "FC Barcelona", "Getafe CF", "Girona FC", 
  "Levante UD", "Rayo Vallecano", "RC Celta", "RCD Espanyol", 
  "Real Betis", "Real Madrid", "Real Sociedad", "Real Valladolid CF",
  "SD Eibar", "SD Huesca", "Sevilla FC", "Valencia CF", "Villarreal CF"
)

seriea <- c(
  "Atalanta","Bologna","Cagliari","Chievo Verona","Empoli", "Fiorentina","Frosinone","Genoa",
  "Inter","Juventus","Lazio","Milan","Napoli","Parma","Roma","Sampdoria","Sassuolo","SPAL",
  "Torino","Udinese"
  
)


ligue1 <- c(
  "Amiens SC", "Angers SCO", "AS Monaco", "AS Saint-Étienne", "Dijon FCO", "En Avant de Guingamp",
  "FC Nantes", "FC Girondins de Bordeaux", "LOSC Lille", "Montpellier HSC", "Nîmes Olympique", 
  "OGC Nice", "Olympique Lyonnais","Olympique de Marseille", "Paris Saint-Germain", 
  "RC Strasbourg Alsace", "Stade Malherbe Caen", "Stade de Reims", "Stade Rennais FC", "Toulouse Football Club"
)



fifa19_updated %<>% mutate(
  League = case_when(
    Club %in% bundesliga ~ "Bundesliga",
    Club %in% premierLeague ~ "Premier League",
    Club %in% laliga ~ "La Liga",
    Club %in% seriea ~ "Serie A",
    Club %in% ligue1 ~ "Ligue 1",
  ),
  Country = case_when(
    League == "Bundesliga" ~ "Germany",
    League == "Premier League" ~ "UK",
    League == "La Liga" ~ "Spain",
    League == "Serie A" ~ "Italy",
    League == "Ligue 1" ~ "France",
  )
) %>% filter(!is.na(League)) %>% mutate_if(is.factor, as.character)


rm(bundesliga, premierLeague, laliga, seriea, ligue1)



str(fifa19_updated)

### columns to drop == Photo, Flag, Club.Logo, Loaned.From, Work.Rate

fifa19_updated %<>% select(-ID, -Body.Type, -Real.Face, -Joined, -Loaned.From, -Photo, -Flag, -Special, -Work.Rate, -Club.Logo, -Release.Clause)

### columns to be factor == Preferred.Foot, Work.Rate


### Height and Weight variables convert cm and kg units.

fifa19_updated %<>%
  mutate(Height = round((as.numeric(str_sub(Height, start=1,end = 1))*30.48) + (as.numeric(str_sub(Height, start = 3, end = 5))* 2.54)),
         Weight = round(as.numeric(str_sub(Weight, start = 1, end = 3)) / 2.204623))

### Correction of the Preferred Foot Variable.

fifa19_updated %<>% filter(Preferred.Foot %in% c("Left", "Right")) 
fifa19_updated$Preferred.Foot <- as.factor(as.character(fifa19_updated$Preferred.Foot))
fifa19_updated <- fifa19_updated[!grepl("K", fifa19_updated$Value),]



head(fifa19_updated$Value)
fifa19_updated$Value <- as.numeric(unlist(regmatches(fifa19_updated$Value, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",fifa19_updated$Value)))) 
fifa19_updated <- fifa19_updated[fifa19_updated$Value >= 1, ]


head(fifa19_updated$Wage)
fifa19_updated$Wage <- as.numeric(unlist(regmatches(fifa19_updated$Wage, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",fifa19_updated$Wage)))) 



str(fifa19_updated)

