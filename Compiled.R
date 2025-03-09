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


Sys.sleep(10800)


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

teams <- teams %>% filter(idLeague == 2, idConference != 0) %>% 
  select(cityTeam, slugTeam, idTeam, nameTeam, urlThumbnailTeam) %>% rename(Opponent = cityTeam) %>% 
  mutate(urlThumbnailTeam = ifelse(slugTeam == "GSW", "https://cdn.nba.com/logos/nba/1610612744/primary/L/logo.svg",urlThumbnailTeam))


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
  
  rosters %>% left_join(all_rosters %>% select(slugTeam,idPlayer), by = "idPlayer") %>% filter(slugTeam.y == x) %>% filter(Include == "Y") %>% select(idPlayer,namePlayer)
  
})

all_players_previous_batch <- bind_rows(all_players_previous_batch)

all_players_previous_batch <- all_players_previous_batch %>% 
  left_join(playerdata %>% filter(slugSeason == current_season) %>% group_by(namePlayer,idPlayer,slugTeam) %>% summarize(n = n()), by = "idPlayer") %>% select(idPlayer,namePlayer.y) %>% 
  rename(namePlayer = namePlayer.y) %>% group_by(idPlayer,namePlayer) %>% summarize(n = n()) %>% as.data.frame()

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


cl <- makeCluster(2)

doParallel::registerDoParallel(cl)

foreach(j = all_players_previous_batch$idPlayer[1:(length(all_players_previous_batch$idPlayer))],.packages = c("flexdashboard",
                                                                                                               "tidyverse",
                                                                                                               "dplyr",
                                                                                                               "knitr",
                                                                                                               "ggplot2",
                                                                                                               "stringr",
                                                                                                               "caret",
                                                                                                               "reactablefmtr",
                                                                                                               "formattable",
                                                                                                               "markdown",
                                                                                                               "magick",
                                                                                                               "highcharter",
                                                                                                               "cowplot",
                                                                                                               "extrafont",
                                                                                                               "data.table",
                                                                                                               "broom",
                                                                                                               "grid",
                                                                                                               "gridExtra",
                                                                                                               "grDevices",
                                                                                                               "fmsb",
                                                                                                               "fontawesome",
                                                                                                               "bslib",
                                                                                                               "plotly",
                                                                                                               "ggbreak",
                                                                                                               "nbastatR",
                                                                                                               "rvest",
                                                                                                               "ggpubr"
),.export = ls(globalenv())) %dopar% {
  
  
  rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/ML_Parlay_TBRv16_2025.Rmd',
                    output_file = paste0(j,substr(j,start = 1,stop=3),".html"),
                    output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/sheets'),
                    params = list(id = j))
}

stopCluster(cl)



test2 <- function(x) {
  
  file.info(paste0("C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/sheets/",x,substr(x,start = 1,stop=3),".html"))
  
}

test2 <- lapply(all_players_previous_batch$idPlayer, test2)

test2 <-bind_rows(test2)

test2 <- test2%>% rownames_to_column('File')

test2 %>% select(File,ctime)


#Matrix


#Next Game

next_game_date_teams <- schedule %>% filter(Date == schedule %>% filter(next_game == TRUE) %>% pull(Date) %>% min) %>% pull(slugTeam)

next_team_batch <- all_rosters %>% filter(slugTeam %in% next_game_date_teams) %>% select(idPlayer,namePlayer)

next_team_batch_date <- schedule %>% filter(next_game == TRUE) %>% pull(Date) %>% min

##Matchup

matchup <- schedule %>% filter(Date == schedule %>% filter(next_game == TRUE) %>% 
                                 pull(Date) %>% min) %>% mutate(matchup = ifelse(location == "Away",paste("vs.",Team),paste("@",Team)))



##Pts Reb Ast


ptrebast <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb_ast)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

ptrebast <- bind_rows(ptrebast) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

ptrebast_pivoted <- ptrebast %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                             summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)   %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 





##Pts


pt <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(3.5,40.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

pt <- bind_rows(pt) %>% pivot_wider(names_from = OU, values_from = test) %>% unnest(cols = everything())

pt_pivoted <- pt %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                 summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam")   %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

##Ast


ast <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,13.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

ast <- bind_rows(ast) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

ast_pivoted <- ast %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                   summarize(GP = n()), by = "idPlayer")   %>% 
  left_join(teams, by = "slugTeam")   %>% rename(Player = namePlayer, Team = slugTeam)   %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 


##Reb


reb <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,17.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,treb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(treb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

reb <- bind_rows(reb) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

reb_pivoted <- reb %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                   summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam")   %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 


##Three Pointers Made


fg3m <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

fg3m <- bind_rows(fg3m) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

fg3m_pivoted <- fg3m %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                     summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam)



##Three Pointers Attempted


fg3a <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,9.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3a)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3a > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

fg3a <- bind_rows(fg3a) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

fg3a_pivoted <- fg3a %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                     summarize(GP = n()), by = "idPlayer")   %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

##Steals


stl <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

stl <- bind_rows(stl) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

stl_pivoted <- stl %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                   summarize(GP = n()), by = "idPlayer")   %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 



##Blocks


blk <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

blk <- bind_rows(blk) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

blk_pivoted <- blk %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                   summarize(GP = n()), by = "idPlayer")   %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 



##Turnovers


tov <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,tov)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(tov > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

tov <- bind_rows(tov) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

tov_pivoted <- tov %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                   summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

##Free Throws Made


ftm <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ftm)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ftm > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

ftm <- bind_rows(ftm) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

ftm_pivoted <- ftm %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                   summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

##Field Goals Made


fgm <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,11.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fgm)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fgm > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

fgm <- bind_rows(fgm) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

fgm_pivoted <- fgm %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                   summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam) %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 



##Points + Assists


pt_ast <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_ast = pts+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_ast)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_ast > x), OU = x) %>% group_by(namePlayer, idPlayer,  OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

pt_ast <- bind_rows(pt_ast) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

pt_ast_pivoted <- pt_ast %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                         summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 

##Points + Rebounds


pt_reb <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb = pts+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb > x), OU = x) %>% group_by(namePlayer, idPlayer,  OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

pt_reb <- bind_rows(pt_reb) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

pt_reb_pivoted <- pt_reb %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                         summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 



##Assists + Rebounds


ast_reb <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(5.5,25.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer,  OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

ast_reb <- bind_rows(ast_reb) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

ast_reb_pivoted <- ast_reb %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                           summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)  %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 


##Steals + Blocks


stl_blk <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,7.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer,  OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

stl_blk <- bind_rows(stl_blk) %>% pivot_wider(names_from = OU, values_from = test)%>% unnest(cols = everything())

stl_blk_pivoted <- stl_blk %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
                                           summarize(GP = n()), by = "idPlayer")  %>% 
  left_join(teams, by = "slugTeam") %>% rename(Player = namePlayer, Team = slugTeam)   %>% select(!c(idPlayer,Team,Opponent,idTeam,nameTeam)) %>% 
  relocate(urlThumbnailTeam, .after = Player) %>% relocate(GP, .after = urlThumbnailTeam) 



##1Q Points

gamedata <- game_logs(seasons = 2024, result_types = "team", season_types = c("Regular Season","Playoffs"))
gamedata_current <- game_logs(seasons = 2025, result_types = "team", season_types = c("Regular Season"))

current_season <- "2024-25"
last_season <- "2023-24"

gamedata <- bind_rows(gamedata,gamedata_current)

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


rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Matrix_Test.Rmd',
                  output_file = "prop_bet_matrix.html",
                  output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Matrix'))


## New Product

color_set <- viridis::magma(5)


##Pts Reb Ast


ptrebast <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% select(idPlayer,slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% mutate(pts_reb_ast = pts+treb+ast) %>%
             pull(pts_reb_ast))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,dateGame,locationGame,pts_reb_ast,urlPlayerHeadshot) %>% 
    left_join(slug_team, by = "idPlayer")
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% 
      summarize(test = min(test),average = mean(pts_reb_ast), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above 
  
})

ptrebast <- bind_rows(ptrebast) %>% mutate(Type = "Regular Season")

##Pts Reb Ast Home Games


ptrebast_home <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% select(idPlayer,slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
             mutate(pts_reb_ast = pts+treb+ast) %>%
             pull(pts_reb_ast))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,dateGame,locationGame,pts_reb_ast) %>% left_join(slug_team, by = "idPlayer")
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% 
      summarize(test = min(test),average = mean(pts_reb_ast),sd = sd, .groups = 'drop') %>% ungroup()
    
  })
  
  hit_rate_above
  
})

ptrebast_home <- bind_rows(ptrebast_home) %>% mutate(Type = "Home Games")



##Pts Reb Ast Away Games


ptrebast_away <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% select(idPlayer,slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
             mutate(pts_reb_ast = pts+treb+ast) %>%
             pull(pts_reb_ast))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,dateGame,locationGame,pts_reb_ast) %>% left_join(slug_team, by = "idPlayer")
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% 
      summarize(test = min(test),average = mean(pts_reb_ast),sd = sd, .groups = 'drop') %>% ungroup()
    
  })
  
  hit_rate_above
  
})

ptrebast_away <- bind_rows(ptrebast_away) %>% mutate(Type = "Away Games")




##Pts Reb Ast Last 5


ptrebast_five <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% select(idPlayer,slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
             mutate(pts_reb_ast = pts+treb+ast) %>% arrange(desc(dateGame)) %>% head(5) %>%
             pull(pts_reb_ast))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,dateGame,locationGame,pts_reb_ast) %>% arrange(desc(dateGame)) %>% head(5) %>% left_join(slug_team, by = "idPlayer")
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>%
      summarize(test = min(test),average = mean(pts_reb_ast),sd = sd, .groups = 'drop') %>% ungroup()
    
  })
  
  hit_rate_above
  
})

ptrebast_five <- bind_rows(ptrebast_five) %>% mutate(Type = "Last 5")




##Pts Reb Ast Last 10


ptrebast_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% select(idPlayer,slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
             mutate(pts_reb_ast = pts+treb+ast) %>% arrange(desc(dateGame)) %>% head(10) %>%
             pull(pts_reb_ast))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,dateGame,locationGame,pts_reb_ast) %>% arrange(desc(dateGame)) %>% head(10) %>% left_join(slug_team, by = "idPlayer")
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% 
      summarize(test = min(test),average = mean(pts_reb_ast),sd = sd, .groups = 'drop') %>% ungroup()
    
  })
  
  hit_rate_above
  
})

ptrebast_ten <- bind_rows(ptrebast_ten) %>% mutate(Type = "Last 10")

##Minutes Last 10


min_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    arrange(desc(dateGame)) %>% head(10)
  
})

min_ten <- bind_rows(min_ten) 

ptreb_ast_df <- bind_rows(ptrebast,ptrebast_away,ptrebast_home,ptrebast_five,ptrebast_ten)

ptreb_ast_df$namePlayer <- stri_trans_general(str = ptreb_ast_df$namePlayer, id = "Latin-ASCII")


##Points + Rebounds


pt_reb <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(10.5,60.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
             mutate(pts_reb = pts+treb) %>%
             pull(pts_reb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb = pts+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup()
    
  })
  
  hit_rate_above
  
})

pt_reb <- bind_rows(pt_reb) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")



##Points + Rebounds Home Games


pt_reb_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(10.5,60.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
             mutate(pts_reb = pts+treb) %>%
             pull(pts_reb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb = pts+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% 
      summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup()
    
  })
  
  hit_rate_above
  
})

pt_reb_home <- bind_rows(pt_reb_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")



##Points + Rebounds Away Games


pt_reb_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(10.5,60.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
             mutate(pts_reb = pts+treb) %>%
             pull(pts_reb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb = pts+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup()
    
  })
  
  hit_rate_above
  
})

pt_reb_away <- bind_rows(pt_reb_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Points + Rebounds Last 10


pt_reb_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(10.5,60.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
             mutate(pts_reb = pts+treb) %>% arrange(desc(dateGame)) %>% head(10) %>%
             pull(pts_reb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb = pts+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup()
    
  })
  
  hit_rate_above
  
})

pt_reb_ten <- bind_rows(pt_reb_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")


##Points + Rebounds Last 5


pt_reb_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(10.5,60.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
             mutate(pts_reb = pts+treb) %>% arrange(desc(dateGame)) %>% head(5) %>%
             pull(pts_reb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb = pts+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup()
    
  })
  
  hit_rate_above
  
})

pt_reb_five <- bind_rows(pt_reb_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")



pt_reb_df <- bind_rows(pt_reb,pt_reb_away,pt_reb_home,pt_reb_five,pt_reb_ten)

pt_reb_df$namePlayer <- stri_trans_general(str = pt_reb_df$namePlayer, id = "Latin-ASCII")




##Assists + Rebounds


ast_reb <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(5.5,25.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% mutate(ast_reb = ast+treb) %>%
             pull(ast_reb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_reb <- bind_rows(ast_reb) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Assists + Rebounds Home Games


ast_reb_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(5.5,25.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
             mutate(ast_reb = ast+treb) %>%
             pull(ast_reb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_reb_home <- bind_rows(ast_reb_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Assists + Rebounds Away Games


ast_reb_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(5.5,25.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
             mutate(ast_reb = ast+treb) %>%
             pull(ast_reb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_reb_away <- bind_rows(ast_reb_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")


##Assists + Rebounds Last 10


ast_reb_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(5.5,25.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
             mutate(ast_reb = ast+treb) %>% arrange(desc(dateGame)) %>% head(10) %>%
             pull(ast_reb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_reb_ten <- bind_rows(ast_reb_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Assists + Rebounds Last 5


ast_reb_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(5.5,25.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
             mutate(ast_reb = ast+treb) %>% arrange(desc(dateGame)) %>% head(5) %>%
             pull(ast_reb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_reb_five <- bind_rows(ast_reb_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")


ast_reb_df <- bind_rows(ast_reb,ast_reb_away,ast_reb_home,ast_reb_five,ast_reb_ten)

ast_reb_df$namePlayer <- stri_trans_general(str = ast_reb_df$namePlayer, id = "Latin-ASCII")


##Steals + Blocks


stl_blk <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,7.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% mutate(stl_blk = stl+blk) %>%
             pull(stl_blk))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd =sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_blk <- bind_rows(stl_blk) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")



##Steals + Blocks Home Games


stl_blk_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,7.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
             mutate(stl_blk = stl+blk) %>%
             pull(stl_blk))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd= sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_blk_home <- bind_rows(stl_blk_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Steals + Blocks Away Games


stl_blk_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,7.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
             mutate(stl_blk = stl+blk) %>%
             pull(stl_blk))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_blk_away <- bind_rows(stl_blk_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Steals + Blocks Last 10


stl_blk_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,7.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
             mutate(stl_blk = stl+blk) %>% arrange(desc(dateGame)) %>% head(10) %>%
             pull(stl_blk))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd= sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_blk_ten <- bind_rows(stl_blk_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Steals + Blocks Last 5


stl_blk_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,7.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
             mutate(stl_blk = stl+blk) %>% arrange(desc(dateGame)) %>% head(5) %>%
             pull(stl_blk))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_blk_five <- bind_rows(stl_blk_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")


stl_blk_df <- bind_rows(stl_blk,stl_blk_away,stl_blk_home,stl_blk_five,stl_blk_ten)

stl_blk_df$namePlayer <- stri_trans_general(str = stl_blk_df$namePlayer, id = "Latin-ASCII")


##Three Pointers Made


fg3m <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,6.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25")  %>%
             pull(fg3m))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

fg3m <- bind_rows(fg3m) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Three Pointers Made Home Games


fg3m_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,6.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H")  %>%
             pull(fg3m))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

fg3m_home <- bind_rows(fg3m_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Three Pointers Made Away Games


fg3m_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,6.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A")  %>%
             pull(fg3m))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

fg3m_away <- bind_rows(fg3m_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Three Pointers Made Last 10


fg3m_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,6.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(10) %>%
             pull(fg3m))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

fg3m_ten <- bind_rows(fg3m_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Three Pointers Made Last 5


fg3m_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,6.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(5) %>%
             pull(fg3m))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

fg3m_five <- bind_rows(fg3m_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")



fg3m_df <- bind_rows(fg3m,fg3m_away,fg3m_home,fg3m_five,fg3m_ten)

fg3m_df$namePlayer <- stri_trans_general(str = fg3m_df$namePlayer, id = "Latin-ASCII")


##Steals


stl <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25")  %>%
             pull(stl))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl <- bind_rows(stl) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Steals Home Games


stl_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H")  %>%
             pull(stl))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_home <- bind_rows(stl_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Steals Away Games


stl_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A")  %>%
             pull(stl))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_away <- bind_rows(stl_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Steals Last 10


stl_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(10) %>%
             pull(stl))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_ten <- bind_rows(stl_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Steals Last 5


stl_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(5) %>%
             pull(stl))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_five <- bind_rows(stl_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")



stl_df <- bind_rows(stl,stl_away,stl_home,stl_five,stl_ten)

stl_df$namePlayer <- stri_trans_general(str = stl_df$namePlayer, id = "Latin-ASCII")


##Blocks


blk <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25")  %>%
             pull(blk))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

blk <- bind_rows(blk) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Blocks Home Games


blk_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H")  %>%
             pull(blk))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

blk_home <- bind_rows(blk_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Blocks Away Games


blk_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A")  %>%
             pull(blk))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

blk_away <- bind_rows(blk_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Blocks Last 10


blk_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(10) %>%
             pull(blk))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,blk) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

blk_ten <- bind_rows(blk_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Blocks Last 5


blk_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(5) %>%
             pull(blk))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,blk) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

blk_five <- bind_rows(blk_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")



blk_df <- bind_rows(blk,blk_away,blk_home,blk_five,blk_ten)

blk_df$namePlayer <- stri_trans_general(str = blk_df$namePlayer, id = "Latin-ASCII")


##Turnovers


tov <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25")  %>%
             pull(tov))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,tov)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(tov > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

tov <- bind_rows(tov) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Turnovers Home Games


tov_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H")  %>%
             pull(tov))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,tov)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(tov > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

tov_home <- bind_rows(tov_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Turnovers Away Games


tov_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A")  %>%
             pull(tov))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,tov)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(tov > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

tov_away <- bind_rows(tov_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Turnovers Last 10


tov_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(10) %>%
             pull(tov))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,tov) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(tov > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

tov_ten <- bind_rows(tov_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Turnovers Last 5


tov_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(5) %>%
             pull(tov))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,tov) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(tov > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

tov_five <- bind_rows(tov_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")



tov_df <- bind_rows(tov,tov_away,tov_home,tov_five,tov_ten)

tov_df$namePlayer <- stri_trans_general(str = tov_df$namePlayer, id = "Latin-ASCII")


##Points


pts <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(3.5,40.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25")  %>%
             pull(pts))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

pts <- bind_rows(pts) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Points Home Games


pts_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(3.5,40.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H")  %>%
             pull(pts))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

pts_home <- bind_rows(pts_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Points Away Games


pts_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(3.5,40.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A")  %>%
             pull(pts))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

pts_away <- bind_rows(pts_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Points Last 10


pts_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(3.5,40.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(10) %>%
             pull(pts))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

pts_ten <- bind_rows(pts_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Points Last 5


pts_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(3.5,40.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(5) %>%
             pull(pts))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

pts_five <- bind_rows(pts_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")



pts_df <- bind_rows(pts,pts_away,pts_home,pts_five,pts_ten)

pts_df$namePlayer <- stri_trans_general(str = pts_df$namePlayer, id = "Latin-ASCII")



##Assists


ast <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,13.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25")  %>%
             pull(ast))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(ast_reb_ast = ast+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast <- bind_rows(ast) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Assists Home Games


ast_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,13.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H")  %>%
             pull(ast))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(ast_reb_ast = ast+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_home <- bind_rows(ast_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Assists Away Games


ast_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,13.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A")  %>%
             pull(ast))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(ast_reb_ast = ast+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_away <- bind_rows(ast_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Assists Last 10


ast_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,13.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(10) %>%
             pull(ast))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(ast_reb_ast = ast+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_ten <- bind_rows(ast_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Assists Last 5


ast_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,13.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(5) %>%
             pull(ast))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(ast_reb_ast = ast+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_five <- bind_rows(ast_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")



ast_df <- bind_rows(ast,ast_away,ast_home,ast_five,ast_ten)

ast_df$namePlayer <- stri_trans_general(str = ast_df$namePlayer, id = "Latin-ASCII")


##Rebounds


treb <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,17.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25")  %>%
             pull(treb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(treb_reb_ast = treb+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,treb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(treb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

treb <- bind_rows(treb) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Rebounds Home Games


treb_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,17.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H")  %>%
             pull(treb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(treb_reb_ast = treb+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,treb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(treb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

treb_home <- bind_rows(treb_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Rebounds Away Games


treb_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,17.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A")  %>%
             pull(treb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(treb_reb_ast = treb+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,treb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(treb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

treb_away <- bind_rows(treb_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Rebounds Last 10


treb_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,17.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(10) %>%
             pull(treb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(treb_reb_ast = treb+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,treb) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(treb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test),sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

treb_ten <- bind_rows(treb_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Rebounds Last 5


treb_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,17.5,1)
  
  sd <- sd(playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% arrange(desc(dateGame)) %>% head(5) %>%
             pull(treb))
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(treb_reb_ast = treb+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,treb) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(treb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), sd = sd, .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

treb_five <- bind_rows(treb_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")



treb_df <- bind_rows(treb,treb_away,treb_home,treb_five,treb_ten)

treb_df$namePlayer <- stri_trans_general(str = treb_df$namePlayer, id = "Latin-ASCII")




