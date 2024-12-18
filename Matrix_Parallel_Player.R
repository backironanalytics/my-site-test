library(tidyverse)
library(nbastatR)
library(dplyr)
library(flexdashboard)
library(parallel)
library(rvest)
library(reactablefmtr)

rosters <- read.csv("rosters.csv")
rosters_id <- rosters %>% filter(Include == "Y") %>% pull(idPlayer) 
rosters_names <- rosters %>% filter(Include == "Y") %>% pull(namePlayer)
rosters_teams <- rosters %>% filter(Include == "Y") %>% pull(idTeam)

color_set <- c("#f7c844","#429460","#2e6d9e")


Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

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




#Schedule

teams <- nba_teams(league = "NBA")

teams <- teams %>% filter(idLeague == 2, idConference != 0) %>% select(cityTeam, slugTeam, idTeam, nameTeam, urlThumbnailTeam) %>% rename(Opponent = cityTeam) %>% 
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
    mutate(Date = str_extract(Date, '\\b[^,]+$')) %>% mutate(Date = as.Date(Date, "%b%d")) %>% select(Date,location,Opponent)
  
  tab <- tab %>% left_join(teams, by = "Opponent") %>% mutate(Team = toupper(x)) %>% 
    mutate(Team = ifelse(Team == "UTAH","UTA",ifelse(Team == "NO","NOP",Team))) %>% mutate(next_game = ifelse(Date >= Sys.Date()+1,TRUE,FALSE)) %>% 
    mutate(game_number = 1:n())
  
})

schedule <- bind_rows(schedule)

schedule <- schedule %>% filter(!is.na(Date))



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






#Next Game

next_game_date_teams <- schedule %>% filter(Date == schedule %>% filter(next_game == TRUE) %>% pull(Date) %>% min) %>% pull(slugTeam)

next_team_batch <- lapply(next_game_date_teams, function(x){
  
  playerdata %>% filter(slugTeam == x , typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer,namePlayer) %>% summarize(n = n())
  
})

next_team_batch <- bind_rows(next_team_batch)

next_team_batch_date <- schedule %>% filter(next_game == TRUE) %>% pull(Date) %>% min


# Play by Play

gameids<- playerdata %>% filter(slugTeam %in% next_game_date_teams) %>% group_by(idGame) %>% summarize(n = n()) %>% pull(idGame)

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

##Pts Reb Ast


ptrebast <- lapply(next_team_batch$idPlayer, function(x){
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb_ast)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test), .groups = 'drop') %>% ungroup()
    
  })
  
  hit_rate_above
  
})



ptrebast <- bind_rows(ptrebast)

pt_reb_ast_player <- ptrebast %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer,urlPlayerHeadshot) %>% 
      summarize(GP = n()), by = "idPlayer") %>% filter(namePlayer %in% c("Damian Lillard","Shai Gilgeous-Alexander","Giannis Antetokounmpo")) %>% group_by(namePlayer,OU,test) %>% 
  summarize(n = n()) %>% pivot_wider(names_from = namePlayer, values_from = test) %>% select(-n) %>% filter(OU >= 33.5, OU <= 43.5)


reactable(highlight = TRUE, striped = TRUE,pt_reb_ast_player, 
          columns = list(
  `Damian Lillard` = colDef(format = colFormat(suffix = "%"), cell = color_tiles(pt_reb_ast_player, number_fmt = scales::percent,colors = color_set, box_shadow = TRUE)),
  `Shai Gilgeous-Alexander` = colDef(format = colFormat(suffix = "%"), cell = color_tiles(pt_reb_ast_player, number_fmt = scales::percent,colors = color_set, box_shadow = TRUE)),
  `Giannis Antetokounmpo` = colDef(format = colFormat(suffix = "%"), cell = color_tiles(pt_reb_ast_player, number_fmt = scales::percent,colors = color_set, box_shadow = TRUE))),
          theme = fivethirtyeight(), defaultPageSize = 50, fullWidth = TRUE) %>% add_title("Points+Rebounds+Assists", align = "center") %>% 
  add_subtitle("How Often The Over Has Hit So Far This Season", align = "center", font_style = "italic")



##Three Pointers Made


fg3m <- lapply(next_team_batch$idPlayer, function(x){
  
  hit_rate <- seq(0.5,6.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test), .groups = 'drop') %>% ungroup()
    
  })
  
  hit_rate_above
  
})


fg3m <- bind_rows(fg3m) %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer,urlPlayerHeadshot) %>% 
                                        summarize(GP = n()), by = "idPlayer") %>% filter(namePlayer %in% c("Damian Lillard","Shai Gilgeous-Alexander","Giannis Antetokounmpo")) %>% group_by(namePlayer,OU,test) %>% 
  summarize(n = n()) %>% pivot_wider(names_from = namePlayer, values_from = test) %>% select(-n)




 reactable(highlight = TRUE, striped = TRUE,fg3m, 
          columns = list(
            `Damian Lillard` = colDef(format = colFormat(suffix = "%"), cell = color_tiles(fg3m, number_fmt = scales::percent,colors = color_set, box_shadow = TRUE)),
            `Shai Gilgeous-Alexander` = colDef(format = colFormat(suffix = "%"), cell = color_tiles(fg3m, number_fmt = scales::percent,colors = color_set, box_shadow = TRUE)),
            `Giannis Antetokounmpo` = colDef(format = colFormat(suffix = "%"), cell = color_tiles(fg3m, number_fmt = scales::percent,colors = color_set, box_shadow = TRUE))),
          theme = fivethirtyeight(), defaultPageSize = 50, fullWidth = TRUE) %>% add_title("Made Three Pointers", align = "center") %>% 
  add_subtitle("How Often The Over Has Hit So Far This Season", align = "center", font_style = "italic")



##Steals + Blocks


stl_blk <- lapply(next_team_batch$idPlayer, function(x){
  
  hit_rate <- seq(0.5,7.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test), .groups = 'drop') %>% ungroup()
    
  })
  
  hit_rate_above
  
})

stl_blk <- bind_rows(stl_blk) %>% left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer,urlPlayerHeadshot) %>% 
                                              summarize(GP = n()), by = "idPlayer") %>% filter(namePlayer %in% c("Damian Lillard","Shai Gilgeous-Alexander","Giannis Antetokounmpo")) %>% group_by(namePlayer,OU,test) %>% 
  summarize(n = n()) %>% pivot_wider(names_from = namePlayer, values_from = test) %>% select(-n)


p<- reactable(highlight = TRUE, striped = TRUE,stl_blk, 
          columns = list(
            `Damian Lillard` = colDef(format = colFormat(suffix = "%"), cell = color_tiles(stl_blk, number_fmt = scales::percent,colors = color_set, box_shadow = TRUE)),
            `Shai Gilgeous-Alexander` = colDef(format = colFormat(suffix = "%"), cell = color_tiles(stl_blk, number_fmt = scales::percent,colors = color_set, box_shadow = TRUE)),
            `Giannis Antetokounmpo` = colDef(format = colFormat(suffix = "%"), cell = color_tiles(stl_blk, number_fmt = scales::percent,colors = color_set, box_shadow = TRUE))),
          theme = fivethirtyeight(), defaultPageSize = 50, fullWidth = TRUE) %>% add_title("Steals + Blocks", align = "center") %>% 
  add_subtitle("How Often The Over Has Hit So Far This Season", align = "center", font_style = "italic")



