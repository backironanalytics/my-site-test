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

error_log <- file("C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/error_log.R", open="wt")
sink(error_log, type="message")

try({

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




Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

gamedata <- game_logs(seasons = season_previous, result_types = "team", season_types = c("Regular Season","Playoffs"))
gamedata_current <- game_logs(seasons = season_current, result_types = "team", season_types = c("Regular Season"))



gamedata <- bind_rows(gamedata,gamedata_current)

all_rosters <- seasons_rosters(seasons = season_current, return_message = FALSE)

playerdata <- game_logs(seasons = season_previous:season_current, result_types = "player", season_types = c("Regular Season"))

playerdata_playoffs <- game_logs(seasons = season_previous, result_types = "player", season_types = c("Playoffs"))

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

teams <- teams %>% filter(idLeague == 2, idConference != 0) %>% select(cityTeam, slugTeam, idTeam, nameTeam, urlThumbnailTeam) %>% rename(Opponent = cityTeam)


slugteams <- teams %>% select(slugTeam)

slugteams_list <- slugteams %>% mutate(slugTeam = tolower(slugTeam)) %>% 
  mutate(slugTeam = ifelse(slugTeam == "uta","utah",
                           ifelse(slugTeam == "nop","no",slugTeam))) %>% pull(slugTeam)

schedule <- lapply(slugteams_list, function(x){
  
  testurl <- paste0("https://www.espn.com/nba/team/schedule/_/name/",x,"/seasontype/2")
  
  h <- read_html(testurl)
  
  tab <- h |> html_nodes("table")
  
  tab <- tab[[1]] |> html_table()
  
  tab <- tab |> setNames(c("Date", "Opponenet", "Time", "TV","Tickets","Tickets_dup","Unused1","Unused2")) 
  
  tab <- tab[-(1:2),]
  
  tab <- tab %>% mutate(location = ifelse(str_detect(Opponenet,"@"),"Away","Home")) %>% 
    mutate(Opponent = ifelse(location == "Home", str_sub(Opponenet,3),str_sub(Opponenet,2))) %>% 
    mutate(Date = str_extract(Date, '\\b[^,]+$')) %>% mutate(Date = as.Date(Date, "%b%d")) %>% 
    mutate(Date = if_else(Date < scheduleDate,Date %m+% years(1),Date)) %>% select(Date,location,Opponent)
  
  tab <- tab %>% left_join(teams, by = "Opponent") %>% mutate(Team = toupper(x)) %>% 
    mutate(Team = ifelse(Team == "UTAH","UTA",ifelse(Team == "NO","NOP",Team))) %>% mutate(next_game = ifelse(Date >= Sys.Date()+1,TRUE,FALSE)) %>% 
    mutate(game_number = 1:n())
  
})

schedule <- bind_rows(schedule)

schedule <- schedule %>% filter(!is.na(Date))

conference <- unnest(bref_teams_stats(seasons = season_current))

conference <- gamedata %>% group_by(nameTeam,slugTeam) %>% summarize(n = n()) %>% 
  left_join(conference %>% mutate(nameTeam = ifelse(nameTeam == "Los Angeles Clippers","LA Clippers",nameTeam)) %>% 
              group_by(nameTeam,nameConference) %>% summarize(n = n()), by = "nameTeam")



all_players <- as.data.frame(players) %>% rename(namePlayer = players) %>% 
  left_join(playerdata %>% filter(slugSeason == current_season) %>% group_by(namePlayer,idPlayer,slugTeam) %>% summarize(n = n()), by = "namePlayer") %>% 
  select(namePlayer,idPlayer,n) %>% ungroup()

all_players_previous <- schedule %>% filter(Date == schedule %>% filter(next_game == TRUE) %>% pull(Date) %>% min -1) %>% pull(slugTeam)

all_players_previous_batch <- lapply(all_players_previous, function(x){
  
  rosters %>% filter(slugTeam == x) %>% filter(Include == "Y") %>% select(idPlayer,namePlayer)
  
})

all_players_previous_batch <- bind_rows(all_players_previous_batch)

all_players_previous_batch <- all_players_previous_batch %>% 
  left_join(playerdata %>% filter(slugSeason == current_season) %>% group_by(namePlayer,idPlayer,slugTeam) %>% summarize(n = n()), by = "idPlayer") %>% select(idPlayer,namePlayer.y) %>% 
  rename(namePlayer = namePlayer.y)

wix_jobs <- write.csv(as.data.frame(players) %>% rename(namePlayer = players) %>% 
                        left_join(playerdata %>% filter(slugSeason == current_season) %>% group_by(namePlayer,idPlayer) %>% 
                                    summarize(n = n()), by = "namePlayer") %>% 
                        left_join(all_rosters %>% select(numberJersey,countYearsExperience,weightLBS,heightInches,agePlayer,groupPosition,idPlayer,slugTeam) %>%
                                    mutate(heightInches = paste0(floor(heightInches/12),"'",round(((heightInches/12)-floor(heightInches/12))*12,0),"\"")), by = "idPlayer") %>% 
                        left_join(playerdata %>% group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% left_join(teams, by = "slugTeam")  %>% 
                        select(namePlayer, idPlayer, nameTeam,urlPlayerHeadshot,numberJersey,countYearsExperience,weightLBS,heightInches,agePlayer,groupPosition) %>% 
                        mutate(urlPlayerHeadshot = paste0("{\"nodes\":[{\"type\":\"PARAGRAPH\",\"id\":\"hlkd5116\",\"nodes\":[],\"paragraphData\":{}},{\"type\":\"HTML\",\"id\":\"cps99115\",\"nodes\":[],\"htmlData\":{\"containerData\":{\"textWrap\":true,\"height\":{\"custom\":\"219\"},\"spoiler\":{},\"alignment\":\"CENTER\",\"width\":{\"custom\":\"265\"}},\"url\":\"",urlPlayerHeadshot,"\"",",\"source\":\"HTML\"}},{\"type\":\"PARAGRAPH\",\"id\":\"zuhhp117\",\"nodes\":[],\"paragraphData\":{}}],\"metadata\":{\"version\":1,\"createdTimestamp\":\"2022-12-19T17:56:18.279Z\",\"updatedTimestamp\":\"2022-12-19T17:56:18.279Z\",\"id\":\"857a69b7-4e6b-4821-973a-064010acd74b\"},\"documentStyle\":{}}")) %>% mutate(HTML = paste0("{\"nodes\":[{\"type\":\"PARAGRAPH\",\"id\":\"hlkd5116\",\"nodes\":[],\"paragraphData\":{}},{\"type\":\"HTML\",\"id\":\"cps99115\",\"nodes\":[],\"htmlData\":{\"containerData\":{\"textWrap\":true,\"height\":{\"custom\":\"1000\"},\"spoiler\":{},\"alignment\":\"CENTER\",\"width\":{\"custom\":\"1800\"}},\"url\":\"https://raw.githack.com/backironanalytics/my-site-test/main/sheets/",idPlayer,substr(idPlayer,start = 1, stop =3),".html\",\"source\":\"HTML\"}},{\"type\":\"PARAGRAPH\",\"id\":\"zuhhp117\",\"nodes\":[],\"paragraphData\":{}}],\"metadata\":{\"version\":1,\"createdTimestamp\":\"2022-12-19T17:56:18.279Z\",\"updatedTimestamp\":\"2022-12-19T17:56:18.279Z\",\"id\":\"857a69b7-4e6b-4821-973a-064010acd74b\"},\"documentStyle\":{}}"))%>% mutate(numberJersey = paste0("Number: ",numberJersey), agePlayer = paste0("Age: ",agePlayer),groupPosition = paste0("Position: ",groupPosition), countYearsExperience = paste0("Experience: ",countYearsExperience," Years"), heightInches = paste0("Height: ",heightInches), weightLBS = paste0("Weight: ",weightLBS," lbs") ) %>% relocate(urlPlayerHeadshot, .after = HTML) %>% select(-idPlayer),"C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/jobs_test2.csv") 



library(foreach)

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")

foreach(i = all_players_previous_batch$namePlayer, 
        j = all_players_previous_batch$idPlayer) %do% {
  
  rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/ML_Parlay_TBRv15_2025.Rmd',
                    output_file = paste0(j,substr(j,start = 1,stop=3),".html"),
                    output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/sheets'),
                    params = list(id = j))
        }

})
