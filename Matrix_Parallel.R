library(tidyverse)
library(nbastatR)
library(dplyr)
library(flexdashboard)
library(parallel)
library(rvest)

rosters <- read.csv("C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/rosters.csv")
rosters_id <- rosters %>% filter(Include == "Y") %>% pull(idPlayer) 
rosters_names <- rosters %>% filter(Include == "Y") %>% pull(namePlayer)
rosters_teams <- rosters %>% filter(Include == "Y") %>% pull(idTeam)

##Date used for schedule

scheduleDate <- "2024-10-01"




Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

all_rosters <- seasons_rosters(seasons = 2025, return_message = FALSE)

playerdata <- game_logs(seasons = 2024:2025, result_types = "player", season_types = c("Regular Season"))

playerdata_playoffs <- game_logs(seasons = 2024, result_types = "player", season_types = c("Playoffs"))

playerdata <- bind_rows(playerdata, playerdata_playoffs)

##DELETE##

#playerdata <- playerdata %>% filter(idPlayer == 203471)




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
    mutate(Date = str_extract(Date, '\\b[^,]+$')) %>% mutate(Date = as.Date(Date, "%b%d")) %>% 
    mutate(Date = if_else(Date < scheduleDate,Date %m+% years(1),Date)) %>% select(Date,location,Opponent)
  
  tab <- tab %>% left_join(teams, by = "Opponent") %>% mutate(Team = toupper(x)) %>% 
    mutate(Team = ifelse(Team == "UTAH","UTA",ifelse(Team == "NO","NOP",Team))) %>% mutate(next_game = ifelse(Date >= Sys.Date()+1,TRUE,FALSE)) %>% 
    mutate(game_number = 1:n())
  
})

schedule <- bind_rows(schedule)

schedule <- schedule %>% filter(!is.na(Date))










#Next Game

next_game_date_teams <- schedule %>% filter(Date == schedule %>% filter(next_game == TRUE) %>% pull(Date) %>% min) %>% pull(slugTeam)

next_team_batch <- lapply(next_game_date_teams, function(x){
  
  playerdata %>% filter(slugTeam == x , typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer,namePlayer) %>% summarize(n = n())
  
})

next_team_batch <- bind_rows(next_team_batch)

next_team_batch_date <- schedule %>% filter(next_game == TRUE) %>% pull(Date) %>% min

##Filter out players who have played this season but no longer on roster

on_roster_filter <- all_rosters %>% filter(idPlayer %in% next_team_batch$idPlayer) %>% group_by(namePlayer,idPlayer,slugTeam) %>% summarize(n = n())

next_team_batch <- on_roster_filter %>% select(idPlayer,namePlayer,n)


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