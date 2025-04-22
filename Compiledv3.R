library(tidyverse)
library(nbastatR)
library(dplyr)
library(flexdashboard)
library(parallel)
library(rvest)
library(stringr)
library(caret)
library(ggplot2)
library(ggpubr)
library(knitr)
library(data.table)
library(broom)
library(grid)
library(gridExtra)
library(grDevices)
library(reactablefmtr)
library(formattable)
library(markdown)
library(magick)
library(highcharter)
library(extrafont)
library(cowplot)
library(fmsb)
library(shiny)
library(fontawesome)
library(bslib)

library(plotly)
library(ggbreak)


library(RSelenium)
library(httr)
library(rjson)
library(jsonlite)
library(stringi)
library(sparkline)
library(rlist)
library(htmlwidgets)
library(webshot)
library(webshot2)
library(foreach)

Sys.sleep(1)




rosters <- read.csv("C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/rosters.csv")
rosters_id <- rosters %>% filter(Include == "Y") %>% pull(idPlayer) 
rosters_names <- rosters %>% filter(Include == "Y") %>% pull(namePlayer)
rosters_teams <- rosters %>% filter(Include == "Y") %>% pull(idTeam)

##Date used for schedule

scheduleDate <- "2024-10-01"

#Season Identifier

current_season <- "2024-25"
last_season <- "2023-24"
season_current <- 2025
season_previous <- 2024

color_set <- viridis::magma(5)




Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

gamedata <- game_logs(seasons = season_previous, result_types = "team", season_types = c("Regular Season","Playoffs"))
gamedata_current <- game_logs(seasons = season_current, result_types = "team", season_types = c("Regular Season","Playoffs"))

gamedata <- bind_rows(gamedata,gamedata_current)


all_rosters <- seasons_rosters(seasons = season_current, return_message = FALSE)

playerdata <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Regular Season"))

playerdata_playoffs <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Playoffs"))

playerdata <- bind_rows(playerdata, playerdata_playoffs)


player_name <- function(x){
  
  playerdata %>% filter(idPlayer == x) %>% 
    select(dateGame, idGame, slugOpponent, idPlayer, namePlayer, fgm,fga,pts,treb,ast,stl,blk,fg3m,fg3a,tov,
           plusminus,minutes,locationGame, countDaysRestPlayer, isB2B, isB2BFirst, isB2BSecond) %>% group_by(namePlayer) %>% summarize(n = n()) %>% pull(namePlayer)
  
}

player_name <- lapply(rosters_id,player_name)

players <- player_name

players <- unlist(players)




gameids<- playerdata %>% group_by(idGame) %>% summarize(n = n()) %>% pull(idGame)

numcores <- parallelly::availableCores()

cl <- makeCluster(numcores)

my_function <- function(x) {
  library(tidyverse)
  library(nbastatR)
  
  play_play <- play_by_play_v2(game_ids = x,nest_data = F)
  
}

results <- parLapply(cl,gameids,my_function)
play_play <- bind_rows(results)


stopCluster(cl)

#Schedule

teams <- nba_teams(league = "NBA")

teams <- teams %>% filter(idConference != 0) %>% 
  select(cityTeam, slugTeam, idTeam, nameTeam, urlThumbnailTeam) %>% rename(Opponent = cityTeam) %>% 
  mutate(urlThumbnailTeam = ifelse(slugTeam == "GSW", "https://cdn.nba.com/logos/nba/1610612744/primary/L/logo.svg",urlThumbnailTeam))


slugteams <- teams %>% select(slugTeam)

slugteams_list <- slugteams %>% mutate(slugTeam = tolower(slugTeam)) %>% 
  mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                           ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)

## Regular Season Schedule

# schedule <- lapply(slugteams_list, function(x){
#   
#   testurl <- paste0("https://www.espn.com/nba/team/schedule/_/name/",x,"/seasontype/2")
#   
#   h <- read_html(testurl)
#   
#   tab <- h |> html_nodes("table")
#   
#   tab <- tab[[1]] |> html_table()
#   
#   tab <- tab |> setNames(c("Date", "Opponenet", "Time", "TV","Tickets","Tickets_dup","Unused1","Unused2")) 
#   
#   tab <- tab[-(1:2),]
#   
#   tab <- tab %>% mutate(location = ifelse(str_detect(Opponenet,"@"),"Away","Home")) %>% 
#     mutate(Opponent = ifelse(location == "Home", str_sub(Opponenet,3),str_sub(Opponenet,2))) %>% 
#     mutate(Date = str_extract(Date, '\\b[^,]+$'))
#   
#   tab <- tab %>% left_join(teams, by = "Opponent") %>% mutate(Team = toupper(x)) %>% 
#     mutate(Team = ifelse(Team == "UTAH","UTA",ifelse(Team == "NO","NOP",Team))) %>% 
#     mutate(game_number = 1:n())
#   
# })
# 
# schedule <- bind_rows(schedule)
# 
# schedule <- schedule %>% filter(!is.na(Date)) %>% mutate(Date = ifelse(substr(Date,1,3) %in% c("Oct","Nov","Dec"),paste(Date,season_previous),paste(Date,season_current))) %>% 
#   mutate(Date = as.Date(Date, "%b%d%Y")) %>% select(Date,location,Opponent,slugTeam,idTeam,nameTeam,urlThumbnailTeam,Team,game_number,TV)

## Play In Schedule

standings <- standings(seasons = season_current, season_types = "Regular Season", return_message = F)

play_off_teams_list <- standings %>% filter(slugPlayoffClinch != "- o") %>% left_join(gamedata %>% group_by(slugTeam,idTeam) %>% summarize(n = n()), by = "idTeam") %>% 
  select(nameTeam,slugTeam.y,idTeam,nameConference,ClinchedPlayIn,ClinchedPostSeason,slugPlayoffClinch) %>% rename(slugTeam = slugTeam.y) %>% 
  mutate(slugTeam = tolower(slugTeam)) %>% 
  mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                           ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)



schedule <- lapply(play_off_teams_list, function(x){
  
  testurl <- paste0("https://www.espn.com/nba/team/schedule/_/name/",x,"/seasontype/3")
  
  h <- read_html(testurl)
  
  tab <- h |> html_nodes("table")
  
  tab <- tab[[1]] |> html_table()
  
  tab <- tab |> setNames(c("Date", "Opponenet", "Time", "TV","Tickets","Tickets_dup","Unused1","Unused2")) 
  
  tab <- tab[-(1:1),]
  
  tab <- tab %>% mutate(location = ifelse(str_detect(Opponenet,"@"),"Away","Home")) %>% 
    mutate(Opponent = ifelse(location == "Home", str_sub(Opponenet,3),str_sub(Opponenet,2))) %>% 
    mutate(Date = str_extract(Date, '\\b[^,]+$'))
  
  tab <- tab %>% left_join(teams, by = "Opponent") %>% mutate(Team = toupper(x)) %>% 
    mutate(Team = ifelse(Team == "UTAH","UTA",ifelse(Team == "NO","NOP",Team))) %>% 
    mutate(game_number = 1:n())
  
})

schedule <- bind_rows(schedule)

schedule <- schedule %>% filter(!is.na(Date)) %>% mutate(Date = ifelse(substr(Date,1,3) %in% c("Oct","Nov","Dec"),paste(Date,season_previous),paste(Date,season_current))) %>% 
  mutate(Date = as.Date(Date, "%b%d%Y")) %>% select(Date,location,Opponent,slugTeam,idTeam,nameTeam,urlThumbnailTeam,Team,game_number,TV,Time)

conference <- unnest(bref_teams_stats(seasons = season_current))

conference <- gamedata %>% group_by(nameTeam,slugTeam) %>% summarize(n = n()) %>% 
  left_join(conference %>% mutate(nameTeam = ifelse(nameTeam == "Los Angeles Clippers","LA Clippers",nameTeam)) %>% 
              group_by(nameTeam,nameConference) %>% summarize(n = n()), by = "nameTeam")



all_players <- as.data.frame(players) %>% rename(namePlayer = players) %>% 
  left_join(playerdata %>% filter(slugSeason == current_season) %>% group_by(namePlayer,idPlayer,slugTeam) %>% summarize(n = n()), by = "namePlayer") %>% 
  select(namePlayer,idPlayer,n) %>% ungroup()


all_players_previous_batch <- rosters %>% left_join(playerdata %>% filter(dateGame == playerdata %>% arrange(desc(dateGame)) %>% head(1) %>% 
                                                                            pull(dateGame)) %>% group_by(idPlayer) %>% summarize(n = n()), by = "idPlayer") %>% 
  filter(!is.na(n), Include == "Y") %>% select(idPlayer,namePlayer)

wix_jobs <- write.csv(as.data.frame(players) %>% rename(namePlayer = players) %>% 
                        left_join(playerdata %>% filter(slugSeason == current_season) %>% group_by(namePlayer,idPlayer) %>% 
                                    summarize(n = n()), by = "namePlayer") %>% 
                        left_join(all_rosters %>% select(numberJersey,countYearsExperience,weightLBS,heightInches,agePlayer,groupPosition,idPlayer,slugTeam) %>%
                                    mutate(heightInches = paste0(floor(heightInches/12),"'",round(((heightInches/12)-floor(heightInches/12))*12,0),"\"")), by = "idPlayer") %>% 
                        left_join(playerdata %>% group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% left_join(teams, by = "slugTeam")  %>% 
                        select(namePlayer, idPlayer, nameTeam,urlPlayerHeadshot,numberJersey,countYearsExperience,weightLBS,heightInches,agePlayer,groupPosition) %>% 
                        mutate(urlPlayerHeadshot = paste0("{\"nodes\":[{\"type\":\"PARAGRAPH\",\"id\":\"hlkd5116\",\"nodes\":[],\"paragraphData\":{}},{\"type\":\"HTML\",\"id\":\"cps99115\",\"nodes\":[],\"htmlData\":{\"containerData\":{\"textWrap\":true,\"height\":{\"custom\":\"219\"},\"spoiler\":{},\"alignment\":\"CENTER\",\"width\":{\"custom\":\"265\"}},\"url\":\"",urlPlayerHeadshot,"\"",",\"source\":\"HTML\"}},{\"type\":\"PARAGRAPH\",\"id\":\"zuhhp117\",\"nodes\":[],\"paragraphData\":{}}],\"metadata\":{\"version\":1,\"createdTimestamp\":\"2022-12-19T17:56:18.279Z\",\"updatedTimestamp\":\"2022-12-19T17:56:18.279Z\",\"id\":\"857a69b7-4e6b-4821-973a-064010acd74b\"},\"documentStyle\":{}}")) %>% mutate(HTML = paste0("{\"nodes\":[{\"type\":\"PARAGRAPH\",\"id\":\"hlkd5116\",\"nodes\":[],\"paragraphData\":{}},{\"type\":\"HTML\",\"id\":\"cps99115\",\"nodes\":[],\"htmlData\":{\"containerData\":{\"textWrap\":true,\"height\":{\"custom\":\"1000\"},\"spoiler\":{},\"alignment\":\"CENTER\",\"width\":{\"custom\":\"1800\"}},\"url\":\"https://raw.githack.com/backironanalytics/my-site-test/main/sheets/",idPlayer,substr(idPlayer,start = 1, stop =3),".html\",\"source\":\"HTML\"}},{\"type\":\"PARAGRAPH\",\"id\":\"zuhhp117\",\"nodes\":[],\"paragraphData\":{}}],\"metadata\":{\"version\":1,\"createdTimestamp\":\"2022-12-19T17:56:18.279Z\",\"updatedTimestamp\":\"2022-12-19T17:56:18.279Z\",\"id\":\"857a69b7-4e6b-4821-973a-064010acd74b\"},\"documentStyle\":{}}"))%>% mutate(numberJersey = paste0("Number: ",numberJersey), agePlayer = paste0("Age: ",agePlayer),groupPosition = paste0("Position: ",groupPosition), countYearsExperience = paste0("Experience: ",countYearsExperience," Years"), heightInches = paste0("Height: ",heightInches), weightLBS = paste0("Weight: ",weightLBS," lbs") ) %>% relocate(urlPlayerHeadshot, .after = HTML) %>% select(-idPlayer),"C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/jobs_test2.csv") 






#Matrix


#Next Game

next_team_batch_date <- schedule %>% 
  arrange(Date) %>% filter(!str_detect(Time,"Postponed"),!str_detect(Time,"-"),!str_detect(TV,"-"), !str_detect(TV,"Postponed"), !is.na(Date)) %>% 
  head(1) %>% pull(Date)

next_game_date_teams <- schedule %>% filter(Date == next_team_batch_date) %>% pull(slugTeam)


next_team_batch <- all_rosters %>% filter(slugTeam %in% next_game_date_teams) %>% select(idPlayer,namePlayer)


## Standings Data



play_off_teams <- standings %>% filter(slugPlayoffClinch != "- o") %>% select(nameTeam,slugTeam,idTeam,nameConference,ClinchedPlayIn,ClinchedPostSeason,slugPlayoffClinch) %>% pull(idTeam)

all_players_previous_batch_play_off <- rosters %>% left_join(playerdata %>% filter(dateGame == playerdata %>% arrange(desc(dateGame)) %>% head(1) %>% 
                                                                                     pull(dateGame)) %>% group_by(idPlayer) %>% summarize(n = n()), by = "idPlayer") %>% 
  filter(!is.na(n), Include == "Y", idTeam %in% play_off_teams) %>% select(idPlayer,namePlayer)

##Matchup

matchup <-  schedule %>% filter(Date == next_team_batch_date) %>% mutate(matchup = ifelse(location == "Away",paste("vs.",Team),paste("@",Team)))


## Test

test_names <- c("fg3a","ftm","pts_reb_ast","fgm","pts_reb","ast_reb","pts_ast","stl_blk","fg3m","stl","blk","tov","pts","ast","treb")

cl <- makeCluster(numcores)


test <- function(z){
  
  library(tidyverse)
  library(nbastatR)
  library(rvest)
  
  Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
  
  season_previous <- 2024
  season_current <- 2025
  
  gamedata <- game_logs(seasons = season_previous, result_types = "team", season_types = c("Regular Season","Playoffs"))
  gamedata_current <- game_logs(seasons = season_current, result_types = "team", season_types = c("Regular Season","Playoffs"))
  
  gamedata <- bind_rows(gamedata,gamedata_current)
  
  playerdata <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Regular Season"))
  
  playerdata_playoffs <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Playoffs"))
  
  playerdata <- bind_rows(playerdata, playerdata_playoffs)
  
  standings <- standings(seasons = season_current, season_types = "Regular Season", return_message = F)
  
  play_off_teams_list <- standings %>% filter(slugPlayoffClinch != "- o") %>% left_join(gamedata %>% group_by(slugTeam,idTeam) %>% summarize(n = n()), by = "idTeam") %>% 
    select(nameTeam,slugTeam.y,idTeam,nameConference,ClinchedPlayIn,ClinchedPostSeason,slugPlayoffClinch) %>% rename(slugTeam = slugTeam.y) %>% 
    mutate(slugTeam = tolower(slugTeam)) %>% 
    mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                             ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)
  
  teams <- nba_teams(league = "NBA")
  
  teams <- teams %>% filter(idConference != 0) %>% 
    select(cityTeam, slugTeam, idTeam, nameTeam, urlThumbnailTeam) %>% rename(Opponent = cityTeam) %>% 
    mutate(urlThumbnailTeam = ifelse(slugTeam == "GSW", "https://cdn.nba.com/logos/nba/1610612744/primary/L/logo.svg",urlThumbnailTeam))
  
  
  slugteams <- teams %>% select(slugTeam)
  
  slugteams_list <- slugteams %>% mutate(slugTeam = tolower(slugTeam)) %>% 
    mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                             ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)
  
  
  
  schedule <- lapply(play_off_teams_list, function(x){
    
    testurl <- paste0("https://www.espn.com/nba/team/schedule/_/name/",x,"/seasontype/3")
    
    h <- read_html(testurl)
    
    tab <- h |> html_nodes("table")
    
    tab <- tab[[1]] |> html_table()
    
    tab <- tab |> setNames(c("Date", "Opponenet", "Time", "TV","Tickets","Tickets_dup","Unused1","Unused2")) 
    
    tab <- tab[-(1:1),]
    
    tab <- tab %>% mutate(location = ifelse(str_detect(Opponenet,"@"),"Away","Home")) %>% 
      mutate(Opponent = ifelse(location == "Home", str_sub(Opponenet,3),str_sub(Opponenet,2))) %>% 
      mutate(Date = str_extract(Date, '\\b[^,]+$'))
    
    tab <- tab %>% left_join(teams, by = "Opponent") %>% mutate(Team = toupper(x)) %>% 
      mutate(Team = ifelse(Team == "UTAH","UTA",ifelse(Team == "NO","NOP",Team))) %>% 
      mutate(game_number = 1:n())
    
  })
  
  schedule <- bind_rows(schedule)
  
  schedule <- schedule %>% filter(!is.na(Date)) %>% mutate(Date = ifelse(substr(Date,1,3) %in% c("Oct","Nov","Dec"),paste(Date,season_previous),paste(Date,season_current))) %>% 
    mutate(Date = as.Date(Date, "%b%d%Y")) %>% select(Date,location,Opponent,slugTeam,idTeam,nameTeam,urlThumbnailTeam,Team,game_number,TV,Time)
  
  all_rosters <- seasons_rosters(seasons = season_current, return_message = FALSE)
  
  next_team_batch_date <- schedule %>% arrange(Date) %>% 
    filter(!str_detect(Time,"Postponed"),!str_detect(Time,"-"),!str_detect(TV,"-"), !str_detect(TV,"Postponed"), !is.na(Date)) %>% head(1) %>% pull(Date)
  
  next_game_date_teams <- schedule %>% filter(Date == next_team_batch_date) %>% pull(slugTeam)
  
  next_team_batch <- all_rosters %>% filter(slugTeam %in% next_game_date_teams) %>% select(idPlayer,namePlayer)
  
  output <- lapply(next_team_batch$idPlayer, function(x){
    
    slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
    
    hit_rate <- seq(0.5,60.5,1)
    
    df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
      mutate(pts_reb_ast = pts+treb+ast, pts_reb = pts+treb,ast_reb = ast+treb, pts_ast = pts+ast,stl_blk = stl+blk, metric = z) %>% 
      select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,metric,z) %>% 
      rename(amount = z)
    
    hit_rate_above <- lapply(hit_rate, function(x){
      
      df %>% mutate(test = mean(amount > x), OU = x) %>% group_by(namePlayer, idPlayer, OU, metric) %>% summarize(test = min(test), .groups = 'drop') %>% 
        ungroup() %>% mutate(slugTeam = slug_team)
      
    })
    
    bind_rows(hit_rate_above)
    
  })
  
  bind_rows(output)
  
}

test <- parLapply(cl,test_names,test)



#Test Home


test_home <- function(z){
  
  library(tidyverse)
  library(nbastatR)
  library(rvest)
  
  Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
  
  season_previous <- 2024
  season_current <- 2025
  
  gamedata <- game_logs(seasons = season_previous, result_types = "team", season_types = c("Regular Season","Playoffs"))
  gamedata_current <- game_logs(seasons = season_current, result_types = "team", season_types = c("Regular Season","Playoffs"))
  
  gamedata <- bind_rows(gamedata,gamedata_current)
  
  playerdata <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Regular Season"))
  
  playerdata_playoffs <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Playoffs"))
  
  playerdata <- bind_rows(playerdata, playerdata_playoffs)
  
  standings <- standings(seasons = season_current, season_types = "Regular Season", return_message = F)
  
  play_off_teams_list <- standings %>% filter(slugPlayoffClinch != "- o") %>% left_join(gamedata %>% group_by(slugTeam,idTeam) %>% summarize(n = n()), by = "idTeam") %>% 
    select(nameTeam,slugTeam.y,idTeam,nameConference,ClinchedPlayIn,ClinchedPostSeason,slugPlayoffClinch) %>% rename(slugTeam = slugTeam.y) %>% 
    mutate(slugTeam = tolower(slugTeam)) %>% 
    mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                             ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)
  
  teams <- nba_teams(league = "NBA")
  
  teams <- teams %>% filter(idConference != 0) %>% 
    select(cityTeam, slugTeam, idTeam, nameTeam, urlThumbnailTeam) %>% rename(Opponent = cityTeam) %>% 
    mutate(urlThumbnailTeam = ifelse(slugTeam == "GSW", "https://cdn.nba.com/logos/nba/1610612744/primary/L/logo.svg",urlThumbnailTeam))
  
  
  slugteams <- teams %>% select(slugTeam)
  
  slugteams_list <- slugteams %>% mutate(slugTeam = tolower(slugTeam)) %>% 
    mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                             ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)
  
  
  
  schedule <- lapply(play_off_teams_list, function(x){
    
    testurl <- paste0("https://www.espn.com/nba/team/schedule/_/name/",x,"/seasontype/3")
    
    h <- read_html(testurl)
    
    tab <- h |> html_nodes("table")
    
    tab <- tab[[1]] |> html_table()
    
    tab <- tab |> setNames(c("Date", "Opponenet", "Time", "TV","Tickets","Tickets_dup","Unused1","Unused2")) 
    
    tab <- tab[-(1:1),]
    
    tab <- tab %>% mutate(location = ifelse(str_detect(Opponenet,"@"),"Away","Home")) %>% 
      mutate(Opponent = ifelse(location == "Home", str_sub(Opponenet,3),str_sub(Opponenet,2))) %>% 
      mutate(Date = str_extract(Date, '\\b[^,]+$'))
    
    tab <- tab %>% left_join(teams, by = "Opponent") %>% mutate(Team = toupper(x)) %>% 
      mutate(Team = ifelse(Team == "UTAH","UTA",ifelse(Team == "NO","NOP",Team))) %>% 
      mutate(game_number = 1:n())
    
  })
  
  schedule <- bind_rows(schedule)
  
  schedule <- schedule %>% filter(!is.na(Date)) %>% mutate(Date = ifelse(substr(Date,1,3) %in% c("Oct","Nov","Dec"),paste(Date,season_previous),paste(Date,season_current))) %>% 
    mutate(Date = as.Date(Date, "%b%d%Y")) %>% select(Date,location,Opponent,slugTeam,idTeam,nameTeam,urlThumbnailTeam,Team,game_number,TV,Time)
  
  all_rosters <- seasons_rosters(seasons = season_current, return_message = FALSE)
  
  next_team_batch_date <- schedule %>% arrange(Date) %>% 
    filter(!str_detect(Time,"Postponed"),!str_detect(Time,"-"),!str_detect(TV,"-"), !str_detect(TV,"Postponed"), !is.na(Date)) %>% head(1) %>% pull(Date)
  
  next_game_date_teams <- schedule %>% filter(Date == next_team_batch_date) %>% pull(slugTeam)
  
  next_team_batch <- all_rosters %>% filter(slugTeam %in% next_game_date_teams) %>% select(idPlayer,namePlayer)
  
  output <- lapply(next_team_batch$idPlayer, function(x){
    
    slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
    
    hit_rate <- seq(0.5,60.5,1)
    
    df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
      mutate(pts_reb_ast = pts+treb+ast, pts_reb = pts+treb,ast_reb = ast+treb, pts_ast = pts+ast,stl_blk = stl+blk, metric = z) %>% 
      select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,metric,z) %>% 
      rename(amount = z)
    
    hit_rate_above <- lapply(hit_rate, function(x){
      
      df %>% mutate(test = mean(amount > x), OU = x) %>% group_by(namePlayer, idPlayer, OU, metric) %>% summarize(test = min(test), .groups = 'drop') %>% 
        ungroup() %>% mutate(slugTeam = slug_team)
      
    })
    
    bind_rows(hit_rate_above)
    
  })
  
  bind_rows(output)
  
}

test_home <- parLapply(cl,test_names,test_home)


#Test Away


test_away <- function(z){
  
  library(tidyverse)
  library(nbastatR)
  library(rvest)
  
  Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
  
  season_previous <- 2024
  season_current <- 2025
  
  gamedata <- game_logs(seasons = season_previous, result_types = "team", season_types = c("Regular Season","Playoffs"))
  gamedata_current <- game_logs(seasons = season_current, result_types = "team", season_types = c("Regular Season","Playoffs"))
  
  gamedata <- bind_rows(gamedata,gamedata_current)
  
  playerdata <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Regular Season"))
  
  playerdata_playoffs <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Playoffs"))
  
  playerdata <- bind_rows(playerdata, playerdata_playoffs)
  
  standings <- standings(seasons = season_current, season_types = "Regular Season", return_message = F)
  
  play_off_teams_list <- standings %>% filter(slugPlayoffClinch != "- o") %>% left_join(gamedata %>% group_by(slugTeam,idTeam) %>% summarize(n = n()), by = "idTeam") %>% 
    select(nameTeam,slugTeam.y,idTeam,nameConference,ClinchedPlayIn,ClinchedPostSeason,slugPlayoffClinch) %>% rename(slugTeam = slugTeam.y) %>% 
    mutate(slugTeam = tolower(slugTeam)) %>% 
    mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                             ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)
  
  teams <- nba_teams(league = "NBA")
  
  teams <- teams %>% filter(idConference != 0) %>% 
    select(cityTeam, slugTeam, idTeam, nameTeam, urlThumbnailTeam) %>% rename(Opponent = cityTeam) %>% 
    mutate(urlThumbnailTeam = ifelse(slugTeam == "GSW", "https://cdn.nba.com/logos/nba/1610612744/primary/L/logo.svg",urlThumbnailTeam))
  
  
  slugteams <- teams %>% select(slugTeam)
  
  slugteams_list <- slugteams %>% mutate(slugTeam = tolower(slugTeam)) %>% 
    mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                             ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)
  
  
  
  schedule <- lapply(play_off_teams_list, function(x){
    
    testurl <- paste0("https://www.espn.com/nba/team/schedule/_/name/",x,"/seasontype/3")
    
    h <- read_html(testurl)
    
    tab <- h |> html_nodes("table")
    
    tab <- tab[[1]] |> html_table()
    
    tab <- tab |> setNames(c("Date", "Opponenet", "Time", "TV","Tickets","Tickets_dup","Unused1","Unused2")) 
    
    tab <- tab[-(1:1),]
    
    tab <- tab %>% mutate(location = ifelse(str_detect(Opponenet,"@"),"Away","Home")) %>% 
      mutate(Opponent = ifelse(location == "Home", str_sub(Opponenet,3),str_sub(Opponenet,2))) %>% 
      mutate(Date = str_extract(Date, '\\b[^,]+$'))
    
    tab <- tab %>% left_join(teams, by = "Opponent") %>% mutate(Team = toupper(x)) %>% 
      mutate(Team = ifelse(Team == "UTAH","UTA",ifelse(Team == "NO","NOP",Team))) %>% 
      mutate(game_number = 1:n())
    
  })
  
  schedule <- bind_rows(schedule)
  
  schedule <- schedule %>% filter(!is.na(Date)) %>% mutate(Date = ifelse(substr(Date,1,3) %in% c("Oct","Nov","Dec"),paste(Date,season_previous),paste(Date,season_current))) %>% 
    mutate(Date = as.Date(Date, "%b%d%Y")) %>% select(Date,location,Opponent,slugTeam,idTeam,nameTeam,urlThumbnailTeam,Team,game_number,TV,Time)
  
  all_rosters <- seasons_rosters(seasons = season_current, return_message = FALSE)
  
  next_team_batch_date <- schedule %>% arrange(Date) %>% 
    filter(!str_detect(Time,"Postponed"),!str_detect(Time,"-"),!str_detect(TV,"-"), !str_detect(TV,"Postponed"), !is.na(Date)) %>% head(1) %>% pull(Date)
  
  next_game_date_teams <- schedule %>% filter(Date == next_team_batch_date) %>% pull(slugTeam)
  
  next_team_batch <- all_rosters %>% filter(slugTeam %in% next_game_date_teams) %>% select(idPlayer,namePlayer)
  
  output <- lapply(next_team_batch$idPlayer, function(x){
    
    slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
    
    hit_rate <- seq(0.5,60.5,1)
    
    df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
      mutate(pts_reb_ast = pts+treb+ast, pts_reb = pts+treb,ast_reb = ast+treb, pts_ast = pts+ast,stl_blk = stl+blk, metric = z) %>% 
      select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,metric,z) %>% 
      rename(amount = z)
    
    hit_rate_above <- lapply(hit_rate, function(x){
      
      df %>% mutate(test = mean(amount > x), OU = x) %>% group_by(namePlayer, idPlayer, OU, metric) %>% summarize(test = min(test), .groups = 'drop') %>% 
        ungroup() %>% mutate(slugTeam = slug_team)
      
    })
    
    bind_rows(hit_rate_above)
    
  })
  
  bind_rows(output)
  
}

test_away <- parLapply(cl,test_names,test_away)


#Test Last 5


test_five <- function(z){
  
  library(tidyverse)
  library(nbastatR)
  library(rvest)
  
  Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
  
  season_previous <- 2024
  season_current <- 2025
  
  gamedata <- game_logs(seasons = season_previous, result_types = "team", season_types = c("Regular Season","Playoffs"))
  gamedata_current <- game_logs(seasons = season_current, result_types = "team", season_types = c("Regular Season","Playoffs"))
  
  gamedata <- bind_rows(gamedata,gamedata_current)
  
  playerdata <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Regular Season"))
  
  playerdata_playoffs <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Playoffs"))
  
  playerdata <- bind_rows(playerdata, playerdata_playoffs)
  
  standings <- standings(seasons = season_current, season_types = "Regular Season", return_message = F)
  
  play_off_teams_list <- standings %>% filter(slugPlayoffClinch != "- o") %>% left_join(gamedata %>% group_by(slugTeam,idTeam) %>% summarize(n = n()), by = "idTeam") %>% 
    select(nameTeam,slugTeam.y,idTeam,nameConference,ClinchedPlayIn,ClinchedPostSeason,slugPlayoffClinch) %>% rename(slugTeam = slugTeam.y) %>% 
    mutate(slugTeam = tolower(slugTeam)) %>% 
    mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                             ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)
  
  teams <- nba_teams(league = "NBA")
  
  teams <- teams %>% filter(idConference != 0) %>% 
    select(cityTeam, slugTeam, idTeam, nameTeam, urlThumbnailTeam) %>% rename(Opponent = cityTeam) %>% 
    mutate(urlThumbnailTeam = ifelse(slugTeam == "GSW", "https://cdn.nba.com/logos/nba/1610612744/primary/L/logo.svg",urlThumbnailTeam))
  
  
  slugteams <- teams %>% select(slugTeam)
  
  slugteams_list <- slugteams %>% mutate(slugTeam = tolower(slugTeam)) %>% 
    mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                             ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)
  
  
  
  schedule <- lapply(play_off_teams_list, function(x){
    
    testurl <- paste0("https://www.espn.com/nba/team/schedule/_/name/",x,"/seasontype/3")
    
    h <- read_html(testurl)
    
    tab <- h |> html_nodes("table")
    
    tab <- tab[[1]] |> html_table()
    
    tab <- tab |> setNames(c("Date", "Opponenet", "Time", "TV","Tickets","Tickets_dup","Unused1","Unused2")) 
    
    tab <- tab[-(1:1),]
    
    tab <- tab %>% mutate(location = ifelse(str_detect(Opponenet,"@"),"Away","Home")) %>% 
      mutate(Opponent = ifelse(location == "Home", str_sub(Opponenet,3),str_sub(Opponenet,2))) %>% 
      mutate(Date = str_extract(Date, '\\b[^,]+$'))
    
    tab <- tab %>% left_join(teams, by = "Opponent") %>% mutate(Team = toupper(x)) %>% 
      mutate(Team = ifelse(Team == "UTAH","UTA",ifelse(Team == "NO","NOP",Team))) %>% 
      mutate(game_number = 1:n())
    
  })
  
  schedule <- bind_rows(schedule)
  
  schedule <- schedule %>% filter(!is.na(Date)) %>% mutate(Date = ifelse(substr(Date,1,3) %in% c("Oct","Nov","Dec"),paste(Date,season_previous),paste(Date,season_current))) %>% 
    mutate(Date = as.Date(Date, "%b%d%Y")) %>% select(Date,location,Opponent,slugTeam,idTeam,nameTeam,urlThumbnailTeam,Team,game_number,TV,Time)
  
  all_rosters <- seasons_rosters(seasons = season_current, return_message = FALSE)
  
  next_team_batch_date <- schedule %>% arrange(Date) %>% 
    filter(!str_detect(Time,"Postponed"),!str_detect(Time,"-"),!str_detect(TV,"-"), !str_detect(TV,"Postponed"), !is.na(Date)) %>% head(1) %>% pull(Date)
  
  next_game_date_teams <- schedule %>% filter(Date == next_team_batch_date) %>% pull(slugTeam)
  
  next_team_batch <- all_rosters %>% filter(slugTeam %in% next_game_date_teams) %>% select(idPlayer,namePlayer)
  
  output <- lapply(next_team_batch$idPlayer, function(x){
    
    slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
    
    hit_rate <- seq(0.5,60.5,1)
    
    df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
      mutate(pts_reb_ast = pts+treb+ast, pts_reb = pts+treb,ast_reb = ast+treb, pts_ast = pts+ast,stl_blk = stl+blk, metric = z) %>% 
      select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,metric,z) %>% arrange(desc(dateGame)) %>% head(5) %>% 
      rename(amount = z)
    
    hit_rate_above <- lapply(hit_rate, function(x){
      
      df %>% mutate(test = mean(amount > x), OU = x) %>% group_by(namePlayer, idPlayer, OU, metric) %>% summarize(test = min(test), .groups = 'drop') %>% 
        ungroup() %>% mutate(slugTeam = slug_team)
      
    })
    
    bind_rows(hit_rate_above)
    
  })
  
  bind_rows(output)
  
}

test_five <- parLapply(cl,test_names,test_five)


#Test Last 10


test_ten <- function(z){
  
  library(tidyverse)
  library(nbastatR)
  library(rvest)
  
  Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
  
  season_previous <- 2024
  season_current <- 2025
  
  gamedata <- game_logs(seasons = season_previous, result_types = "team", season_types = c("Regular Season","Playoffs"))
  gamedata_current <- game_logs(seasons = season_current, result_types = "team", season_types = c("Regular Season","Playoffs"))
  
  gamedata <- bind_rows(gamedata,gamedata_current)
  
  playerdata <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Regular Season"))
  
  playerdata_playoffs <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Playoffs"))
  
  playerdata <- bind_rows(playerdata, playerdata_playoffs)
  
  standings <- standings(seasons = season_current, season_types = "Regular Season", return_message = F)
  
  play_off_teams_list <- standings %>% filter(slugPlayoffClinch != "- o") %>% left_join(gamedata %>% group_by(slugTeam,idTeam) %>% summarize(n = n()), by = "idTeam") %>% 
    select(nameTeam,slugTeam.y,idTeam,nameConference,ClinchedPlayIn,ClinchedPostSeason,slugPlayoffClinch) %>% rename(slugTeam = slugTeam.y) %>% 
    mutate(slugTeam = tolower(slugTeam)) %>% 
    mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                             ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)
  
  teams <- nba_teams(league = "NBA")
  
  teams <- teams %>% filter(idConference != 0) %>% 
    select(cityTeam, slugTeam, idTeam, nameTeam, urlThumbnailTeam) %>% rename(Opponent = cityTeam) %>% 
    mutate(urlThumbnailTeam = ifelse(slugTeam == "GSW", "https://cdn.nba.com/logos/nba/1610612744/primary/L/logo.svg",urlThumbnailTeam))
  
  
  slugteams <- teams %>% select(slugTeam)
  
  slugteams_list <- slugteams %>% mutate(slugTeam = tolower(slugTeam)) %>% 
    mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                             ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)
  
  
  
  schedule <- lapply(play_off_teams_list, function(x){
    
    testurl <- paste0("https://www.espn.com/nba/team/schedule/_/name/",x,"/seasontype/3")
    
    h <- read_html(testurl)
    
    tab <- h |> html_nodes("table")
    
    tab <- tab[[1]] |> html_table()
    
    tab <- tab |> setNames(c("Date", "Opponenet", "Time", "TV","Tickets","Tickets_dup","Unused1","Unused2")) 
    
    tab <- tab[-(1:1),]
    
    tab <- tab %>% mutate(location = ifelse(str_detect(Opponenet,"@"),"Away","Home")) %>% 
      mutate(Opponent = ifelse(location == "Home", str_sub(Opponenet,3),str_sub(Opponenet,2))) %>% 
      mutate(Date = str_extract(Date, '\\b[^,]+$'))
    
    tab <- tab %>% left_join(teams, by = "Opponent") %>% mutate(Team = toupper(x)) %>% 
      mutate(Team = ifelse(Team == "UTAH","UTA",ifelse(Team == "NO","NOP",Team))) %>% 
      mutate(game_number = 1:n())
    
  })
  
  schedule <- bind_rows(schedule)
  
  schedule <- schedule %>% filter(!is.na(Date)) %>% mutate(Date = ifelse(substr(Date,1,3) %in% c("Oct","Nov","Dec"),paste(Date,season_previous),paste(Date,season_current))) %>% 
    mutate(Date = as.Date(Date, "%b%d%Y")) %>% select(Date,location,Opponent,slugTeam,idTeam,nameTeam,urlThumbnailTeam,Team,game_number,TV,Time)
  
  all_rosters <- seasons_rosters(seasons = season_current, return_message = FALSE)
  
  next_team_batch_date <- schedule %>% arrange(Date) %>% 
    filter(!str_detect(Time,"Postponed"),!str_detect(Time,"-"),!str_detect(TV,"-"), !str_detect(TV,"Postponed"), !is.na(Date)) %>% head(1) %>% pull(Date)
  
  next_game_date_teams <- schedule %>% filter(Date == next_team_batch_date) %>% pull(slugTeam)
  
  next_team_batch <- all_rosters %>% filter(slugTeam %in% next_game_date_teams) %>% select(idPlayer,namePlayer)
  
  output <- lapply(next_team_batch$idPlayer, function(x){
    
    slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
    
    hit_rate <- seq(0.5,60.5,1)
    
    df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
      mutate(pts_reb_ast = pts+treb+ast, pts_reb = pts+treb,ast_reb = ast+treb, pts_ast = pts+ast,stl_blk = stl+blk, metric = z) %>% 
      select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,metric,z) %>% arrange(desc(dateGame)) %>% head(10) %>% 
      rename(amount = z)
    
    hit_rate_above <- lapply(hit_rate, function(x){
      
      df %>% mutate(test = mean(amount > x), OU = x) %>% group_by(namePlayer, idPlayer, OU, metric) %>% summarize(test = min(test), .groups = 'drop') %>% 
        ungroup() %>% mutate(slugTeam = slug_team)
      
    })
    
    bind_rows(hit_rate_above)
    
  })
  
  bind_rows(output)
  
}

test_ten <- parLapply(cl,test_names,test_ten)




stopCluster(cl)




test <- bind_rows(test)
test_2 <- test %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

fg3a <- test_2 %>% filter(metric == "fg3a") %>% select(!metric)
ftm <- test_2 %>% filter(metric == "ftm") %>% select(!metric)
fgm <- test_2 %>% filter(metric == "fgm") %>% select(!metric)
ptrebast_1 <- test_2 %>% filter(metric == "pts_reb_ast") %>% select(!metric)
pt_reb_1 <- test_2 %>% filter(metric == "pts_reb") %>% select(!metric)
ast_reb_1 <- test_2 %>% filter(metric == "ast_reb") %>% select(!metric)
pt_ast_1 <- test_2 %>% filter(metric == "pts_ast") %>% select(!metric)
stl_blk_1 <- test_2 %>% filter(metric == "stl_blk") %>% select(!metric)
fg3m_1 <- test_2 %>% filter(metric == "fg3m") %>% select(!metric)
stl_1 <- test_2 %>% filter(metric == "stl") %>% select(!metric)
blk_1 <- test_2 %>% filter(metric == "blk") %>% select(!metric)
tov_1 <- test_2 %>% filter(metric == "tov") %>% select(!metric)
pt <- test_2 %>% filter(metric == "pts") %>% select(!metric)
ast_1 <- test_2 %>% filter(metric == "ast") %>% select(!metric)
reb <- test_2 %>% filter(metric == "treb") %>% select(!metric)

fg3a_pivoted <- fg3a %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                     summarize(GP = n()), by = "idPlayer")   %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

ftm_pivoted <- ftm %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                   summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

fgm_pivoted <- fgm %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                   summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

ptrebast_pivoted <- ptrebast_1 %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                               summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)   %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

ptrebast <- test %>% filter(metric == "pts_reb_ast") %>% mutate(Type = "Regular Season")

pt_reb_pivoted <- pt_reb_1 %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                           summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

pt_reb <- test %>% filter(metric == "pts_reb") %>% mutate(Type = "Regular Season")

ast_reb_pivoted <- ast_reb_1 %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                             summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

ast_reb <- test %>% filter(metric == "ast_reb") %>% mutate(Type = "Regular Season")


pt_ast_pivoted <- pt_ast_1 %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                           summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

pt_ast <- test %>% filter(metric == "pts_ast") %>% mutate(Type = "Regular Season")


stl_blk_pivoted <- stl_blk_1 %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                             summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)   %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

stl_blk <- test %>% filter(metric == "stl_blk") %>% mutate(Type = "Regular Season")

fg3m_pivoted <- fg3m_1 %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                       summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam)

fg3m <- test %>% filter(metric == "fg3m")%>% mutate(Type = "Regular Season")


stl_pivoted <- stl_1 %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                     summarize(GP = n()), by = "idPlayer")   %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

stl <- test %>% filter(metric == "stl") %>% mutate(Type = "Regular Season")


blk_pivoted <- blk_1 %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                     summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

blk <- test %>% filter(metric == "blk") %>% mutate(Type = "Regular Season")

tov_pivoted <- tov_1 %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                     summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

tov <- test %>% filter(metric == "tov") %>% mutate(Type = "Regular Season")


pt_pivoted <- pt %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                 summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam")   %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

pts <- test %>% filter(metric == "pts") %>% mutate(Type = "Regular Season")

ast_pivoted <- ast_1 %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                     summarize(GP = n()), by = "idPlayer")   %>% 
  left_join(teams, by = "slugTeam")   %>% rename(Player = namePlayer, Team = slugTeam)   %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

ast <- test %>% filter(metric == "ast") %>% mutate(Type = "Regular Season")


reb_pivoted <- reb %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                   summarize(GP = n()), by = "idPlayer")   %>% 
  left_join(teams, by = "slugTeam")   %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

treb <- test %>% filter(metric == "treb") %>% mutate(Type = "Regular Season")



test_home <- bind_rows(test_home)
test_2_home <- test_home %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

fg3a_home <- test_home %>% filter(metric == "fg3a") %>% select(!metric) %>% mutate(Type = "Home Games")
ftm_home <- test_home %>% filter(metric == "ftm") %>% select(!metric) %>% mutate(Type = "Home Games")
fgm_home <- test_home %>% filter(metric == "fgm") %>% select(!metric) %>% mutate(Type = "Home Games")
ptrebast_home <- test_home %>% filter(metric == "pts_reb_ast") %>% select(!metric) %>% mutate(Type = "Home Games")
pt_reb_home <- test_home %>% filter(metric == "pts_reb") %>% select(!metric) %>% mutate(Type = "Home Games")
ast_reb_home <- test_home %>% filter(metric == "ast_reb") %>% select(!metric) %>% mutate(Type = "Home Games")
pt_ast_home <- test_home %>% filter(metric == "pts_ast") %>% select(!metric) %>% mutate(Type = "Home Games")
stl_blk_home <- test_home %>% filter(metric == "stl_blk") %>% select(!metric) %>% mutate(Type = "Home Games")
fg3m_home <- test_home %>% filter(metric == "fg3m") %>% select(!metric) %>% mutate(Type = "Home Games")
stl_home <- test_home %>% filter(metric == "stl") %>% select(!metric) %>% mutate(Type = "Home Games")
blk_home <- test_home %>% filter(metric == "blk") %>% select(!metric) %>% mutate(Type = "Home Games")
tov_home <- test_home %>% filter(metric == "tov") %>% select(!metric) %>% mutate(Type = "Home Games")
pts_home <- test_home %>% filter(metric == "pts") %>% select(!metric) %>% mutate(Type = "Home Games")
ast_home <- test_home %>% filter(metric == "ast") %>% select(!metric) %>% mutate(Type = "Home Games")
treb_home <- test_home %>% filter(metric == "treb") %>% select(!metric) %>% mutate(Type = "Home Games")



test_away <- bind_rows(test_away)
test_2_away <- test_away %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

fg3a_away <- test_away %>% filter(metric == "fg3a") %>% select(!metric) %>% mutate(Type = "Away Games")
ftm_away <- test_away %>% filter(metric == "ftm") %>% select(!metric) %>% mutate(Type = "Away Games")
fgm_away <- test_away %>% filter(metric == "fgm") %>% select(!metric) %>% mutate(Type = "Away Games")
ptrebast_away <- test_away %>% filter(metric == "pts_reb_ast") %>% select(!metric) %>% mutate(Type = "Away Games")
pt_reb_away <- test_away %>% filter(metric == "pts_reb") %>% select(!metric) %>% mutate(Type = "Away Games")
ast_reb_away <- test_away %>% filter(metric == "ast_reb") %>% select(!metric) %>% mutate(Type = "Away Games")
pt_ast_away <- test_away %>% filter(metric == "pts_ast") %>% select(!metric) %>% mutate(Type = "Away Games")
stl_blk_away <- test_away %>% filter(metric == "stl_blk") %>% select(!metric) %>% mutate(Type = "Away Games")
fg3m_away <- test_away %>% filter(metric == "fg3m") %>% select(!metric) %>% mutate(Type = "Away Games")
stl_away <- test_away %>% filter(metric == "stl") %>% select(!metric) %>% mutate(Type = "Away Games")
blk_away <- test_away %>% filter(metric == "blk") %>% select(!metric) %>% mutate(Type = "Away Games")
tov_away <- test_away %>% filter(metric == "tov") %>% select(!metric) %>% mutate(Type = "Away Games")
pts_away <- test_away %>% filter(metric == "pts") %>% select(!metric) %>% mutate(Type = "Away Games")
ast_away <- test_away %>% filter(metric == "ast") %>% select(!metric) %>% mutate(Type = "Away Games")
treb_away <- test_away %>% filter(metric == "treb") %>% select(!metric) %>% mutate(Type = "Away Games")




test_five <- bind_rows(test_five)
test_2_five <- test_five %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

fg3a_five <- test_five %>% filter(metric == "fg3a") %>% select(!metric) %>% mutate(Type = "Last 5")
ftm_five <- test_five %>% filter(metric == "ftm") %>% select(!metric) %>% mutate(Type = "Last 5")
fgm_five <- test_five %>% filter(metric == "fgm") %>% select(!metric) %>% mutate(Type = "Last 5")
ptrebast_five <- test_five %>% filter(metric == "pts_reb_ast") %>% select(!metric) %>% mutate(Type = "Last 5")
pt_reb_five <- test_five %>% filter(metric == "pts_reb") %>% select(!metric) %>% mutate(Type = "Last 5")
ast_reb_five <- test_five %>% filter(metric == "ast_reb") %>% select(!metric) %>% mutate(Type = "Last 5")
pt_ast_five <- test_five %>% filter(metric == "pts_ast") %>% select(!metric) %>% mutate(Type = "Last 5")
stl_blk_five <- test_five %>% filter(metric == "stl_blk") %>% select(!metric) %>% mutate(Type = "Last 5")
fg3m_five <- test_five %>% filter(metric == "fg3m") %>% select(!metric) %>% mutate(Type = "Last 5")
stl_five <- test_five %>% filter(metric == "stl") %>% select(!metric) %>% mutate(Type = "Last 5")
blk_five <- test_five %>% filter(metric == "blk") %>% select(!metric) %>% mutate(Type = "Last 5")
tov_five <- test_five %>% filter(metric == "tov") %>% select(!metric) %>% mutate(Type = "Last 5")
pts_five <- test_five %>% filter(metric == "pts") %>% select(!metric) %>% mutate(Type = "Last 5")
ast_five <- test_five %>% filter(metric == "ast") %>% select(!metric) %>% mutate(Type = "Last 5")
treb_five <- test_five %>% filter(metric == "treb") %>% select(!metric) %>% mutate(Type = "Last 5")


test_ten <- bind_rows(test_ten)
test_2_ten <- test_ten %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

fg3a_ten <- test_ten %>% filter(metric == "fg3a") %>% select(!metric) %>% mutate(Type = "Last 10")
ftm_ten <- test_ten %>% filter(metric == "ftm") %>% select(!metric) %>% mutate(Type = "Last 10")
fgm_ten <- test_ten %>% filter(metric == "fgm") %>% select(!metric) %>% mutate(Type = "Last 10")
ptrebast_ten <- test_ten %>% filter(metric == "pts_reb_ast") %>% select(!metric) %>% mutate(Type = "Last 10")
pt_reb_ten <- test_ten %>% filter(metric == "pts_reb") %>% select(!metric) %>% mutate(Type = "Last 10")
ast_reb_ten <- test_ten %>% filter(metric == "ast_reb") %>% select(!metric) %>% mutate(Type = "Last 10")
pt_ast_ten <- test_ten %>% filter(metric == "pts_ast") %>% select(!metric) %>% mutate(Type = "Last 10")
stl_blk_ten <- test_ten %>% filter(metric == "stl_blk") %>% select(!metric) %>% mutate(Type = "Last 10")
fg3m_ten <- test_ten %>% filter(metric == "fg3m") %>% select(!metric) %>% mutate(Type = "Last 10")
stl_ten <- test_ten %>% filter(metric == "stl") %>% select(!metric) %>% mutate(Type = "Last 10")
blk_ten <- test_ten %>% filter(metric == "blk") %>% select(!metric) %>% mutate(Type = "Last 10")
tov_ten <- test_ten %>% filter(metric == "tov") %>% select(!metric) %>% mutate(Type = "Last 10")
pts_ten <- test_ten %>% filter(metric == "pts") %>% select(!metric) %>% mutate(Type = "Last 10")
ast_ten <- test_ten %>% filter(metric == "ast") %>% select(!metric) %>% mutate(Type = "Last 10")
treb_ten <- test_ten %>% filter(metric == "treb") %>% select(!metric) %>% mutate(Type = "Last 10")


ptreb_ast_df <- bind_rows(ptrebast,ptrebast_away,ptrebast_home,ptrebast_five,ptrebast_ten)

ptreb_ast_df$namePlayer <- stri_trans_general(str = ptreb_ast_df$namePlayer, id = "Latin-ASCII")

pt_reb_df <- bind_rows(pt_reb,pt_reb_away,pt_reb_home,pt_reb_five,pt_reb_ten)

pt_reb_df$namePlayer <- stri_trans_general(str = pt_reb_df$namePlayer, id = "Latin-ASCII")

ast_reb_df <- bind_rows(ast_reb,ast_reb_away,ast_reb_home,ast_reb_five,ast_reb_ten)

ast_reb_df$namePlayer <- stri_trans_general(str = ast_reb_df$namePlayer, id = "Latin-ASCII")

pt_ast_df <- bind_rows(pt_ast,pt_ast_away,pt_ast_home,pt_ast_five,pt_ast_ten)

pt_ast_df$namePlayer <- stri_trans_general(str = pt_ast_df$namePlayer, id = "Latin-ASCII")

stl_blk_df <- bind_rows(stl_blk,stl_blk_away,stl_blk_home,stl_blk_five,stl_blk_ten)

stl_blk_df$namePlayer <- stri_trans_general(str = stl_blk_df$namePlayer, id = "Latin-ASCII")

fg3m_df <- bind_rows(fg3m,fg3m_away,fg3m_home,fg3m_five,fg3m_ten)

fg3m_df$namePlayer <- stri_trans_general(str = fg3m_df$namePlayer, id = "Latin-ASCII")

stl_df <- bind_rows(stl,stl_away,stl_home,stl_five,stl_ten)

stl_df$namePlayer <- stri_trans_general(str = stl_df$namePlayer, id = "Latin-ASCII")

blk_df <- bind_rows(blk,blk_away,blk_home,blk_five,blk_ten)

blk_df$namePlayer <- stri_trans_general(str = blk_df$namePlayer, id = "Latin-ASCII")

tov_df <- bind_rows(tov,tov_away,tov_home,tov_five,tov_ten)

tov_df$namePlayer <- stri_trans_general(str = tov_df$namePlayer, id = "Latin-ASCII")

pts_df <- bind_rows(pts,pts_away,pts_home,pts_five,pts_ten)

pts_df$namePlayer <- stri_trans_general(str = pts_df$namePlayer, id = "Latin-ASCII")

ast_df <- bind_rows(ast,ast_away,ast_home,ast_five,ast_ten)

ast_df$namePlayer <- stri_trans_general(str = ast_df$namePlayer, id = "Latin-ASCII")

treb_df <- bind_rows(treb,treb_away,treb_home,treb_five,treb_ten)

treb_df$namePlayer <- stri_trans_general(str = treb_df$namePlayer, id = "Latin-ASCII")

##Minutes Last 10


min_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    arrange(desc(dateGame)) %>% head(10)
  
})

min_ten <- bind_rows(min_ten) 


 

##1Q Points


firstqpoints <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,10.5,1)
  
  
  
  play_play_player <- play_play %>% filter(idPlayerNBA1 == x, !is.na(slugScore))
  
  play_play_makes <- play_play_player %>% 
    select(idGame,numberPeriod, descriptionPlayHome,descriptionPlayNeutral,descriptionPlayVisitor, namePlayer1,slugScore,scoreHome,scoreAway,slugTeamPlayer1) %>% 
    rename(slugTeam = slugTeamPlayer1) %>% mutate(description = ifelse(is.na(descriptionPlayHome),ifelse(is.na(descriptionPlayNeutral),descriptionPlayVisitor,descriptionPlayNeutral),descriptionPlayHome)) %>% mutate(free_throw_flag = str_detect(description,"Free Throw"), three_point_flag = str_detect(description,"3PT")) %>% mutate(two_point_flag = ifelse(free_throw_flag == FALSE & three_point_flag == FALSE, TRUE,FALSE)) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam")) %>% mutate(pts = ifelse(free_throw_flag == TRUE,1,ifelse(two_point_flag== TRUE,2,3)),score_type = ifelse(free_throw_flag == TRUE,"Free Throw",ifelse(two_point_flag== TRUE,"2 pt","3 pt")))
  
  firstq_makes <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer,slugTeam) %>% summarize(n = n()) %>% left_join(play_play_makes %>% filter(numberPeriod == 1) %>% group_by(dateGame) %>% summarize(pts = sum(pts)), by = "dateGame") %>% mutate(pts = replace_na(pts,0))
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_makes %>% ungroup() %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

firstqpoints <- bind_rows(firstqpoints) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

firstqpoints_pivoted <- firstqpoints %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                                     summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)   %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 


##1Q Assists


firstqassists <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_assists <- play_play %>% filter(idPlayerNBA2 == x, str_detect(descriptionPlayHome,"AST") | str_detect(descriptionPlayVisitor,"AST"))
  
  play_play_assists <- play_play_player_assists %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,namePlayer2,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam"))
  
  firstq_assists <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason, namePlayer,idPlayer,slugTeam) %>% summarize(n = n()) %>% left_join(play_play_assists %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% summarize(assists = n()), by = "dateGame") %>% mutate(assists = replace_na(assists,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_assists %>% ungroup() %>% mutate(test = mean(assists > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

firstqassists <- bind_rows(firstqassists) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

firstqassists_pivoted <- firstqassists %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                                       summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)   %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

##1Q Rebounds


firstqrebounds <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_rebounds <- play_play %>% filter(is.na(slugScore),idPlayerNBA1 == x, str_detect(descriptionPlayHome,"REBOUND") | str_detect(descriptionPlayVisitor,"REBOUND"))
  
  play_play_rebounds <- play_play_player_rebounds %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,slugTeamPlayer1) %>% 
    rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam")) 
  
  firstq_rebounds <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25")  %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer,slugTeam) %>% summarize(n = n()) %>% 
    left_join(play_play_rebounds %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% summarize(rebounds = n()), by = "dateGame") %>%
    mutate(rebounds = replace_na(rebounds,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_rebounds %>% ungroup() %>% mutate(test = mean(rebounds > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

firstqrebounds <- bind_rows(firstqrebounds) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

firstqrebounds_pivoted <- firstqrebounds %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                                         summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)   %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 


library(reactablefmtr)

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")


rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Matrix_Testv2.Rmd',
                  output_file = "prop_bet_matrix.html",
                  output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Matrix'))


# Data Sheet Output



Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")


# cl <- makeCluster(2)
# 
# doParallel::registerDoParallel(cl)
# 
# foreach(j = all_players_previous_batch$idPlayer,.packages = c("flexdashboard",
#                                                                                                                "tidyverse",
#                                                                                                                "dplyr",
#                                                                                                                "knitr",
#                                                                                                                "ggplot2",
#                                                                                                                "stringr",
#                                                                                                                "caret",
#                                                                                                                "reactablefmtr",
#                                                                                                                "formattable",
#                                                                                                                "markdown",
#                                                                                                                "magick",
#                                                                                                                "highcharter",
#                                                                                                                "cowplot",
#                                                                                                                "extrafont",
#                                                                                                                "data.table",
#                                                                                                                "broom",
#                                                                                                                "grid",
#                                                                                                                "gridExtra",
#                                                                                                                "grDevices",
#                                                                                                                "fmsb",
#                                                                                                                "fontawesome",
#                                                                                                                "bslib",
#                                                                                                                "plotly",
#                                                                                                                "ggbreak",
#                                                                                                                "nbastatR",
#                                                                                                                "rvest",
#                                                                                                                "ggpubr"
# ),.export = ls(globalenv())) %dopar% {
#   
#   
#   rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/ML_Parlay_TBRv17_2025.Rmd',
#                     output_file = paste0(j,substr(j,start = 1,stop=3),".html"),
#                     output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/sheets'),
#                     params = list(id = j))
# }
# 
# stopCluster(cl)
# 
# 
# 
# test2 <- function(x) {
#   
#   file.info(paste0("C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/sheets/",x,substr(x,start = 1,stop=3),".html"))
#   
# }
# 
# test2 <- lapply(all_players_previous_batch$idPlayer, test2)
# 
# test2 <-bind_rows(test2)
# 
# test2 <- test2%>% rownames_to_column('File')
# 
# test2 %>% select(File,ctime)


#One Off


foreach(j = all_players_previous_batch_play_off$idPlayer) %do% {
  
  rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/ML_Parlay_TBRv17_2025.Rmd',
                    output_file = paste0(j,substr(j,start = 1,stop=3),".html"),
                    output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/sheets'),
                    params = list(id = j))
}



