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

rosters <- read.csv("rosters.csv")
rosters_id <- rosters %>% filter(Include == "Y") %>% pull(idPlayer) 
rosters_names <- rosters %>% filter(Include == "Y") %>% pull(namePlayer)
rosters_teams <- rosters %>% filter(Include == "Y") %>% pull(idTeam)




Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

gamedata <- game_logs(seasons = 2024, result_types = "team", season_types = c("Regular Season","Playoffs"))
gamedata_current <- game_logs(seasons = 2025, result_types = "team", season_types = c("Regular Season"))

current_season <- "2024-25"
last_season <- "2023-24"

gamedata <- bind_rows(gamedata,gamedata_current)

all_rosters <- seasons_rosters(seasons = 2025, return_message = FALSE)

playerdata <- game_logs(seasons = 2024:2025, result_types = "player", season_types = c("Regular Season"))

playerdata_playoffs <- game_logs(seasons = 2024, result_types = "player", season_types = c("Playoffs"))

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
    mutate(Date = if_else(Date < "2024-10-01",Date %m+% years(1),Date)) %>% select(Date,location,Opponent)
  
  tab <- tab %>% left_join(teams, by = "Opponent") %>% mutate(Team = toupper(x)) %>% 
    mutate(Team = ifelse(Team == "UTAH","UTA",ifelse(Team == "NO","NOP",Team))) %>% mutate(next_game = ifelse(Date >= Sys.Date()+1,TRUE,FALSE)) %>% 
    mutate(game_number = 1:n())
  
})

schedule <- bind_rows(schedule)

schedule <- schedule %>% filter(!is.na(Date))

conference <- unnest(bref_teams_stats(seasons = 2025))

conference <- gamedata %>% group_by(nameTeam,slugTeam) %>% summarize(n = n()) %>% 
  left_join(conference %>% mutate(nameTeam = ifelse(nameTeam == "Los Angeles Clippers","LA Clippers",nameTeam)) %>% 
              group_by(nameTeam,nameConference) %>% summarize(n = n()), by = "nameTeam")



all_players <- as.data.frame(players) %>% rename(namePlayer = players) %>% 
  left_join(playerdata %>% filter(slugSeason == "2024-25") %>% group_by(namePlayer,idPlayer,slugTeam) %>% summarize(n = n()), by = "namePlayer") %>% 
  select(namePlayer,idPlayer,n) %>% ungroup()

all_players_previous <- schedule %>% filter(Date == schedule %>% filter(next_game == TRUE) %>% pull(Date) %>% min -1) %>% pull(slugTeam)

all_players_previous_batch <- lapply(all_players_previous, function(x){
  
  rosters %>% filter(slugTeam == x) %>% filter(Include == "Y") %>% select(idPlayer,namePlayer)
  
})

all_players_previous_batch <- bind_rows(all_players_previous_batch)

all_players_previous_batch <- all_players_previous_batch %>% 
  left_join(playerdata %>% filter(slugSeason == "2024-25") %>% group_by(namePlayer,idPlayer,slugTeam) %>% summarize(n = n()), by = "idPlayer") %>% select(idPlayer,namePlayer.y) %>% 
  rename(namePlayer = namePlayer.y)

library(foreach)

foreach(i = all_players_previous_batch$namePlayer[1:(length(all_players_previous_batch$namePlayer)/2)], 
        j = all_players_previous_batch$idPlayer[1:(length(all_players_previous_batch$namePlayer)/2)]) %do% {
  
  rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/ML_Parlay_TBRv15_2025.Rmd',
                    output_file = paste0(j,substr(j,start = 1,stop=3),".html"),
                    output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/sheets'),
                    params = list(id = j))
}
