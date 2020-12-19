
###Data
fifa19 <- read.csv2("C:/Users/hajiy/Desktop/Fifa_19_Project/Data/fifa2019.csv", sep  = ",")
row.has.na <- apply(fifa19, 1, function(x){any(is.na(x))})
md <- fifa19[!row.has.na,]


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



md %<>% mutate(
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


rm(bundesliga, premierLeague, laliga, seriea, ligue1, row.has.na)



### columns to drop 

md %<>% select(-ID, -Body.Type, -Real.Face, -Joined, -Loaned.From, -Photo, -Flag, -Special, -Work.Rate, -Club.Logo, -Release.Clause)




### Height and Weight variables convert cm and kg units.

md %<>%
  mutate(Height = round((as.numeric(str_sub(Height, start=1,end = 1))*30.48) + (as.numeric(str_sub(Height, start = 3, end = 5))* 2.54)),
         Weight = round(as.numeric(str_sub(Weight, start = 1, end = 3)) / 2.204623))

### Correction of the Preferred Foot Variable.

md %<>% filter(Preferred.Foot %in% c("Left", "Right")) 
md$Preferred.Foot <- as.factor(as.character(md$Preferred.Foot))
md <- md[!grepl("K", md$Value),]
md$Value <- as.numeric(unlist(regmatches(md$Value, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",md$Value)))) 
md <- md[md$Value >= 1, ]
md$Wage <- as.numeric(unlist(regmatches(md$Wage, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",md$Wage)))) 


# Create Position Class #
defence <- c("CB", "RB", "LB", "LWB", "RWB", "LCB", "RCB")
midfielder <- c("CM", "CDM","CAM","LM","RM", "LAM", "RAM", "LCM", "RCM", "LDM", "RDM")

md %<>% mutate(Class = if_else(Position %in% "GK", "Goal Keeper",
                                 if_else(Position %in% defence, "Defender",
                                         if_else(Position %in% midfielder, "Midfielder", "Forward"))))

rm(defence, midfielder)

md$value_currency <- paste("€", md$Value, "M")
md$wage_currency <- paste("€", md$Wage, "K")

write.csv(md,'.csv')


