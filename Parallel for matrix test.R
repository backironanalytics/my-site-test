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

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

numcores <- parallelly::availableCores()


cl <- makeCluster(numcores)

test_names <- c("fg3a","ftm","pts_reb_ast","fgm","pts_reb","ast_reb","pts_ast","stl_blk","fg3m","stl","blk","tov","pts","ast","treb")


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
stopCluster(cl)