library(tidyverse)
library(rvest)
library(RSelenium)
library(httr)
library(rjson)
library(nbastatR)
library(jsonlite)
library(dplyr)
library(parallel)
library(stringi)
library(reactablefmtr)
library(sparkline)
library(rlist)
library(htmlwidgets)
library(webshot)
library(webshot2)

color_set <- viridis::magma(5)



rosters <- read.csv("C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/rosters.csv")
rosters_id <- rosters %>% filter(Include == "Y") %>% pull(idPlayer) 
rosters_names <- rosters %>% filter(Include == "Y") %>% pull(namePlayer)
rosters_teams <- rosters %>% filter(Include == "Y") %>% pull(idTeam)


#1st Quarter Points


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1215/subcategories/16551"
json_data <- fromJSON(txt=json_file)

dk_firstqpts <- json_data[5]

dk_firstqpts <- as.data.frame(dk_firstqpts)

dk_firstqpts <- unnest(dk_firstqpts)

dk_firstqpts <- dk_firstqpts %>% filter(selections.tags == "SGP") %>% select(seoIdentifier,selections.label,american) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american) 

dk_firstqpts <- dk_firstqpts %>% 
  mutate(label = ifelse(label == "3+",2.5,ifelse(label == "4+",3.5,ifelse(label == "5+",4.5,
                                                                          ifelse(label == "6+",5.5,ifelse(label == "7+",6.5,
                                                                                                          ifelse(label == "8+",7.5,ifelse(label == "9+",8.5,ifelse(label == "9+",8.5,ifelse(label == "10+",9.5,label)))))))))) %>%
  mutate(odds = as.numeric(str_replace(odds,"−","-")))

# Pts Reb Ast O/U

json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/583/subcategories/5001"
json_data <- fromJSON(txt=json_file)

dk_ptrebast <- json_data[5]

dk_ptrebast <- as.data.frame(dk_ptrebast)

dk_ptrebast <- unnest(dk_ptrebast)

dk_ptrebast <- dk_ptrebast %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)

#Pts Ast O/U


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/583/subcategories/9973"
json_data <- fromJSON(txt=json_file)

test <- json_data[5]

test <- as.data.frame(test)

#Pts Reb O/U


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/583/subcategories/9976"
json_data <- fromJSON(txt=json_file)

dk_ptreb <- json_data[5]

dk_ptreb <- as.data.frame(dk_ptreb)

dk_ptreb <- unnest(dk_ptreb)

dk_ptreb <- dk_ptreb %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)


#Ast Reb O/U


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/583/subcategories/9974"
json_data <- fromJSON(txt=json_file)

dk_astreb  <- json_data[5]

dk_astreb <- as.data.frame(dk_astreb)

dk_astreb <- unnest(dk_astreb)

dk_astreb <- dk_astreb %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)

#Total Reb O/U


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1216/subcategories/12492"
json_data <- fromJSON(txt=json_file)

test <- json_data[5]

test <- as.data.frame(test)

# Steals Blocks O/U

json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1293/subcategories/13781"
json_data <- fromJSON(txt=json_file)

dk_stlblk  <- json_data[5]

dk_stlblk <- as.data.frame(dk_stlblk)

dk_stlblk <- unnest(dk_stlblk)

dk_stlblk <- dk_stlblk %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)


# Threes Made

json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1218/subcategories/12497"
json_data <- fromJSON(txt=json_file)

dk_fg3m  <- json_data[5]

dk_fg3m <- as.data.frame(dk_fg3m)

dk_fg3m <- unnest(dk_fg3m)

dk_fg3m <- dk_fg3m %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)

# First Quarter Assists


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1217/subcategories/16552"
json_data <- fromJSON(txt=json_file)

dk_firstqast <- json_data[5]

dk_firstqast <- as.data.frame(dk_firstqast)

dk_firstqast <- unnest(dk_firstqast)

dk_firstqast <- dk_firstqast %>% filter(selections.tags == "SGP") %>% select(seoIdentifier,selections.label,american) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american) 

dk_firstqast <- dk_firstqast %>% 
  mutate(label = ifelse(label == "3+",2.5,ifelse(label == "4+",3.5,ifelse(label == "5+",4.5,
                                                                          ifelse(label == "6+",5.5,ifelse(label == "7+",6.5,
                                                                                                          ifelse(label == "8+",7.5,ifelse(label == "9+",8.5,ifelse(label == "9+",8.5,ifelse(label == "10+",9.5,ifelse(label == "2+",1.5,label))))))))))) %>%
  mutate(odds = as.numeric(str_replace(odds,"−","-")))


# First Quarter Rebounds


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1216/subcategories/16553"
json_data <- fromJSON(txt=json_file)

dk_firstqreb <- json_data[5]

dk_firstqreb <- as.data.frame(dk_firstqreb)

dk_firstqreb <- unnest(dk_firstqreb)

dk_firstqreb <- dk_firstqreb %>% filter(selections.tags == "SGP") %>% select(seoIdentifier,selections.label,american) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american) 

dk_firstqreb <- dk_firstqreb %>% 
  mutate(label = ifelse(label == "3+",2.5,ifelse(label == "4+",3.5,ifelse(label == "5+",4.5,
                                                                          ifelse(label == "6+",5.5,ifelse(label == "7+",6.5,
                                                                                                          ifelse(label == "8+",7.5,ifelse(label == "9+",8.5,ifelse(label == "9+",8.5,ifelse(label == "10+",9.5,ifelse(label == "2+",1.5,label)))))))))))


# Steals


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1293/subcategories/13508"
json_data <- fromJSON(txt=json_file)

dk_stl <- json_data[5]

dk_stl <- as.data.frame(dk_stl)

dk_stl <- unnest(dk_stl)

dk_stl <- dk_stl %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)


# Blocks


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1293/subcategories/13780"
json_data <- fromJSON(txt=json_file)

dk_blk <- json_data[5]

dk_blk <- as.data.frame(dk_blk)

dk_blk <- unnest(dk_blk)

dk_blk <- dk_blk %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)

# Turnovers


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1293/subcategories/13782"
json_data <- fromJSON(txt=json_file)

dk_tov <- json_data[5]

dk_tov <- as.data.frame(dk_tov)

dk_tov <- unnest(dk_tov)

dk_tov <- dk_tov %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)


# Points


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1215/subcategories/12488"
json_data <- fromJSON(txt=json_file)

dk_pts <- json_data[5]

dk_pts <- as.data.frame(dk_pts)

dk_pts <- unnest(dk_pts)

dk_pts <- dk_pts %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)

# Assists


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1217/subcategories/12495"
json_data <- fromJSON(txt=json_file)

dk_ast <- json_data[5]

dk_ast <- as.data.frame(dk_ast)

dk_ast <- unnest(dk_ast)

dk_ast <- dk_ast %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)

# Rebounds


json_file <- "https://sportsbook-nash.draftkings.com/api/sportscontent/dkusil/v1/leagues/42648/categories/1216/subcategories/12492"
json_data <- fromJSON(txt=json_file)

dk_reb <- json_data[5]

dk_reb <- as.data.frame(dk_reb)

dk_reb <- unnest(dk_reb)

dk_reb <- dk_reb %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)





  
  
  
  
##Date used for schedule
  
scheduleDate = "2024-10-01"




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

next_game_date_teams <- schedule %>% filter(Date == schedule %>% filter(next_game == TRUE) %>% pull(Date) %>% min-1) %>% pull(slugTeam)

next_team_batch <- lapply(next_game_date_teams, function(x){
  
  playerdata %>% filter(slugTeam == x , typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer,namePlayer) %>% summarize(n = n())
  
})

next_team_batch <- bind_rows(next_team_batch)

next_team_batch_date <- schedule %>% filter(next_game == TRUE) %>% pull(Date) %>% min -1

##Filter out players who have played this season but no longer on roster

on_roster_filter <- all_rosters %>% filter(idPlayer %in% next_team_batch$idPlayer) %>% group_by(namePlayer,idPlayer,slugTeam) %>% summarize(n = n())

next_team_batch <- on_roster_filter %>% select(idPlayer,namePlayer,n)

##Matchup

matchup <- schedule %>% filter(Date == schedule %>% filter(next_game == TRUE) %>% 
                                 pull(Date) %>% min-1) %>% mutate(matchup = ifelse(location == "Away",paste("vs.",Team),paste("@",Team)))


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


ptreb_ast_df <- ptreb_ast_df %>% left_join(dk_ptrebast, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = pts+treb+ast) %>% group_by(idPlayer) %>% summarize(avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(ptreb_ast_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer") %>%
  left_join(ptreb_ast_df %>% filter(Type == "Home Games") %>% group_by(idPlayer) %>% summarize(variation_home = mean(sd)), by = "idPlayer") %>%
  left_join(ptreb_ast_df %>% filter(Type == "Away Games") %>% group_by(idPlayer) %>% summarize(variation_away = mean(sd)), by = "idPlayer") %>%
  left_join(ptreb_ast_df %>% filter(Type == "Last 10") %>% group_by(idPlayer) %>% summarize(variation_ten = mean(sd)), by = "idPlayer") %>%
  left_join(ptreb_ast_df %>% filter(Type == "Last 5") %>% group_by(idPlayer) %>% summarize(variation_five = mean(sd)), by = "idPlayer")

ptreb_ast_df_join <- ptreb_ast_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Last 10", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

ptrebast_picks <- ptreb_ast_df %>% left_join(ptreb_ast_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten %>% 
              mutate(pts_reb_ast = pts+treb+ast) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(pts_reb_ast = list(pts_reb_ast)), by = "idPlayer") %>% 
  group_by(namePlayer, OU,Under, slugTeam,Type, avg = round(avg,1),
                                  variation_regular = round(variation_regular,1),
                                  variation_home = as.character(round(variation_home,1)),
                                  variation_away = as.character(round(variation_away,1)),
                                  variation_five = as.character(round(variation_five,1)),
                                  variation_ten = as.character(round(variation_ten,1)),
           minutes, pts_reb_ast, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(pts_reb_ast, .after = minutes) %>% relocate(GP, .after = pts_reb_ast) %>%
  select(!c(slugTeam,variation_home,variation_away,variation_ten,variation_five,variation_regular))

# reactable(highlight = TRUE, ptrebast_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
#                                                                                style = cell_style(font_weight = "bold")),
#                                                                           urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
#                                                            avg = colDef(name = "Season Avg"),
#                                                            GP = colDef(name = "Games Played"),
#                                                            Under = colDef(name = "Odds"),
#           OU = colDef(name = "Total Points, Rebounds, Assists O/U",width = 110),
#           minutes = colDef(name = "Minutes Last 10 GP",cell = function(value,index) {
#             sparkline(ptrebast_picks$minutes[[index]], height = 50)
#           }),
#           pts_reb_ast = colDef(name = "Total Points, Rebounds, Assists Last 10 GP",cell = function(value,index) {
#             sparkline(ptrebast_picks$pts_reb_ast[[index]], type = "bar", height = 50)
#           }),
#           `Away Games` = colDef(cell = data_bars(ptrebast_picks, 
#                                                  fill_color = color_set, 
#                                                  background = '#F1F1F1', 
#                                                  min_value = 0, 
#                                                  max_value = 1, 
#                                                  text_position = 'outside-end',
#                                                  number_fmt = scales::percent,
#                                                  bold_text = TRUE)),
#           `Home Games` = colDef(cell = data_bars(ptrebast_picks, 
#                                                  fill_color = color_set, 
#                                                  background = '#F1F1F1', 
#                                                  min_value = 0, 
#                                                  max_value = 1, 
#                                                  text_position = 'outside-end',
#                                                  number_fmt = scales::percent,
#                                                  bold_text = TRUE)),
#           `Last 10` = colDef(cell = data_bars(ptrebast_picks, 
#                                               fill_color = color_set, 
#                                               background = '#F1F1F1', 
#                                               min_value = 0, 
#                                               max_value = 1, 
#                                               text_position = 'outside-end',
#                                               number_fmt = scales::percent,
#                                               bold_text = TRUE)),
#           `Last 5` = colDef(cell = data_bars(ptrebast_picks, 
#                                              fill_color = color_set, 
#                                              background = '#F1F1F1', 
#                                              min_value = 0, 
#                                              max_value = 1, 
#                                              text_position = 'outside-end',
#                                              number_fmt = scales::percent,
#                                              bold_text = TRUE)),
#           `Regular Season` = colDef(cell = data_bars(ptrebast_picks, 
#                                                      fill_color = color_set, 
#                                                      background = '#F1F1F1', 
#                                                      min_value = 0, 
#                                                      max_value = 1, 
#                                                      text_position = 'outside-end',
#                                                      number_fmt = scales::percent,
#                                                      bold_text = TRUE))),
#           theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"),
#           paginationType = "simple")
# 




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


pt_reb_df <- pt_reb_df %>% left_join(dk_ptreb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = pts+treb) %>% group_by(idPlayer) %>% summarize(avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(pt_reb_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

pt_reb_df_join <- pt_reb_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

pt_reb_picks <- pt_reb_df %>% left_join(pt_reb_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten %>% mutate(pts_reb = pts+treb) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(pts_reb = list(pts_reb)), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, OU,Under, Type, avg = round(avg,1),
                                  variation_regular = round(variation_regular,1), minutes, pts_reb, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>% pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% 
  relocate(matchup, .after = urlThumbnailTeam) %>% relocate(minutes, .after = Under) %>% relocate(pts_reb, .after = minutes) %>% relocate(GP, .after = pts_reb) %>% 
  select(-c(slugTeam,idPlayer, variation_regular))



# reactable(highlight = TRUE, striped = TRUE,pt_reb_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
#                                                                                             style = cell_style(font_weight = "bold")),
#                                                                           urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
#                                                                         avg = colDef(name = "Season Avg"),
#                                                                         Under = colDef(name = "Odds"),
#                                                                         GP = colDef(name = "Games Played"),
#                                                                         minutes = colDef(name = "Minutes Last 10 GP",cell = function(value,index) {
#                                                                           sparkline(pt_reb_picks$minutes[[index]], height = 50)
#                                                                         }),
#                                                                         pts_reb = colDef(name = "Points, Rebounds Last 10 GP",cell = function(value,index) {
#                                                                           sparkline(pt_reb_picks$pts_reb[[index]], type = "bar", height = 50)
#                                                                         }),
#                                                                           OU = colDef(name = "Total Points & Rebounds O/U",width = 110),
#                                                                           `Away Games` = colDef(cell = data_bars(pt_reb_picks, 
#                                                                                                                  fill_color = color_set, 
#                                                                                                                  background = '#F1F1F1', 
#                                                                                                                  min_value = 0, 
#                                                                                                                  max_value = 1, 
#                                                                                                                  text_position = 'outside-end',
#                                                                                                                  number_fmt = scales::percent)),
#                                                                           `Home Games` = colDef(cell = data_bars(pt_reb_picks, 
#                                                                                                                  fill_color = color_set, 
#                                                                                                                  background = '#F1F1F1', 
#                                                                                                                  min_value = 0, 
#                                                                                                                  max_value = 1, 
#                                                                                                                  text_position = 'outside-end',
#                                                                                                                  number_fmt = scales::percent)),
#                                                                           `Last 10` = colDef(cell = data_bars(pt_reb_picks, 
#                                                                                                               fill_color = color_set, 
#                                                                                                               background = '#F1F1F1', 
#                                                                                                               min_value = 0, 
#                                                                                                               max_value = 1, 
#                                                                                                               text_position = 'outside-end',
#                                                                                                               number_fmt = scales::percent)),
#                                                                           `Last 5` = colDef(cell = data_bars(pt_reb_picks, 
#                                                                                                              fill_color = color_set, 
#                                                                                                              background = '#F1F1F1', 
#                                                                                                              min_value = 0, 
#                                                                                                              max_value = 1, 
#                                                                                                              text_position = 'outside-end',
#                                                                                                              number_fmt = scales::percent)),
#                                                                           `Regular Season` = colDef(cell = data_bars(pt_reb_picks, 
#                                                                                                                      fill_color = color_set, 
#                                                                                                                      background = '#F1F1F1', 
#                                                                                                                      min_value = 0, 
#                                                                                                                      max_value = 1, 
#                                                                                                                      text_position = 'outside-end',
#                                                                                                                      number_fmt = scales::percent))),
#           theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"),
#           paginationType = "simple") 
# 
# 

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


ast_reb_df <- ast_reb_df %>% left_join(dk_astreb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = ast+treb) %>% group_by(idPlayer) %>% summarize(avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(ast_reb_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

ast_reb_df_join <- ast_reb_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Last 5", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

ast_reb_picks <- ast_reb_df %>% left_join(ast_reb_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten %>% mutate(ast_reb = ast+treb) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(ast_reb = list(ast_reb)), by = "idPlayer")  %>% 
  group_by(namePlayer,idPlayer, OU, Under, Type, avg = round(avg,1),
                                  variation_regular = round(variation_regular,1), minutes, ast_reb, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%
  relocate(minutes, .after = Under) %>% relocate(ast_reb, .after = minutes) %>% relocate(GP, .after = ast_reb) %>% select(-c(slugTeam,idPlayer, variation_regular))


# reactable(highlight = TRUE, striped = TRUE,ast_reb_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
#                                                                                              style = cell_style(font_weight = "bold")),
#                                                                         urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
#                                                                         avg = colDef(name = "Season Avg"),
#                                                                         GP = colDef(name = "Games Played"),
#                                                                         minutes = colDef(name = "Minutes Last 10 GP",cell = function(value,index) {
#                                                                           sparkline(ast_reb_picks$minutes[[index]], height = 50)
#                                                                         }),
#                                                                         ast_reb = colDef(name = "Total Assists & Rebounds Last 10 GP",cell = function(value,index) {
#                                                                           sparkline(ast_reb_picks$ast_reb[[index]], type = "bar", height = 50)
#                                                                         }),
#                                                                         Under = colDef(name = "Odds"),
#                                                                         OU = colDef(name = "Total Assists & Rebounds O/U",width = 110),
#                                                                         `Away Games` = colDef(cell = data_bars(ast_reb_picks, 
#                                                                                                                fill_color = color_set, 
#                                                                                                                background = '#F1F1F1', 
#                                                                                                                min_value = 0, 
#                                                                                                                max_value = 1, 
#                                                                                                                text_position = 'outside-end',
#                                                                                                                number_fmt = scales::percent)),
#                                                                         `Home Games` = colDef(cell = data_bars(ast_reb_picks, 
#                                                                                                                fill_color = color_set, 
#                                                                                                                background = '#F1F1F1', 
#                                                                                                                min_value = 0, 
#                                                                                                                max_value = 1, 
#                                                                                                                text_position = 'outside-end',
#                                                                                                                number_fmt = scales::percent)),
#                                                                         `Last 10` = colDef(cell = data_bars(ast_reb_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                         `Last 5` = colDef(cell = data_bars(ast_reb_picks, 
#                                                                                                            fill_color = color_set, 
#                                                                                                            background = '#F1F1F1', 
#                                                                                                            min_value = 0, 
#                                                                                                            max_value = 1, 
#                                                                                                            text_position = 'outside-end',
#                                                                                                            number_fmt = scales::percent)),
#                                                                         `Regular Season` = colDef(cell = data_bars(ast_reb_picks, 
#                                                                                                                    fill_color = color_set, 
#                                                                                                                    background = '#F1F1F1', 
#                                                                                                                    min_value = 0, 
#                                                                                                                    max_value = 1, 
#                                                                                                                    text_position = 'outside-end',
#                                                                                                                    number_fmt = scales::percent))),
#           theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"),
#           paginationType = "simple") 
# 



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


stl_blk_df <- stl_blk_df %>% left_join(dk_stlblk, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = stl+blk) %>% group_by(idPlayer) %>% summarize(avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(stl_blk_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% 
              summarize(variation_regular = mean(sd)), by = "idPlayer")

stl_blk_df_join <- stl_blk_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

stl_blk_picks <- stl_blk_df %>% left_join(stl_blk_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten %>% mutate(stl_blk = stl+blk) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(stl_blk = list(stl_blk)), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, OU, Under, Type, avg = round(avg,1),
                                  variation_regular = round(variation_regular,1), minutes, stl_blk, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%
  relocate(minutes, .after = Under) %>% relocate(stl_blk, .after = minutes) %>% relocate(GP, .after = stl_blk)  %>% select(-c(slugTeam,idPlayer, variation_regular))




# reactable(highlight = TRUE, striped = TRUE,stl_blk_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
#                                                                                              style = cell_style(font_weight = "bold")),
#                                                                          urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
#                                                                          avg = colDef(name = "Season Avg"),
#                                                                          GP = colDef(name = "Games Played"),
#                                                                          Under = colDef(name = "Odds"),
#                                                                          minutes = colDef(name = "Minutes Last 10 GP",cell = function(value,index) {
#                                                                            sparkline(stl_blk_picks$minutes[[index]], height = 50)
#                                                                          }),
#                                                                          stl_blk = colDef(name = "Total Steals & Blocks Last 10 GP",cell = function(value,index) {
#                                                                            sparkline(stl_blk_picks$stl_blk[[index]], type = "bar", height = 50)
#                                                                          }),
#                                                                          OU = colDef(name = "Total Steals & Blocks O/U",width = 110),
#                                                                          `Away Games` = colDef(cell = data_bars(stl_blk_picks, 
#                                                                                                                 fill_color = color_set, 
#                                                                                                                 background = '#F1F1F1', 
#                                                                                                                 min_value = 0, 
#                                                                                                                 max_value = 1, 
#                                                                                                                 text_position = 'outside-end',
#                                                                                                                 number_fmt = scales::percent)),
#                                                                          `Home Games` = colDef(cell = data_bars(stl_blk_picks, 
#                                                                                                                 fill_color = color_set, 
#                                                                                                                 background = '#F1F1F1', 
#                                                                                                                 min_value = 0, 
#                                                                                                                 max_value = 1, 
#                                                                                                                 text_position = 'outside-end',
#                                                                                                                 number_fmt = scales::percent)),
#                                                                          `Last 10` = colDef(cell = data_bars(stl_blk_picks, 
#                                                                                                              fill_color = color_set, 
#                                                                                                              background = '#F1F1F1', 
#                                                                                                              min_value = 0, 
#                                                                                                              max_value = 1, 
#                                                                                                              text_position = 'outside-end',
#                                                                                                              number_fmt = scales::percent)),
#                                                                          `Last 5` = colDef(cell = data_bars(stl_blk_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                          `Regular Season` = colDef(cell = data_bars(stl_blk_picks, 
#                                                                                                                     fill_color = color_set, 
#                                                                                                                     background = '#F1F1F1', 
#                                                                                                                     min_value = 0, 
#                                                                                                                     max_value = 1, 
#                                                                                                                     text_position = 'outside-end',
#                                                                                                                     number_fmt = scales::percent))),
#           theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"),
#           paginationType = "simple")
# 

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


fg3m_df <- fg3m_df %>% left_join(dk_fg3m, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(fg3m), GP = n()), by = "idPlayer") %>% 
  left_join(fg3m_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

fg3m_df_join <- fg3m_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

fg3m_picks <- fg3m_df %>% left_join(fg3m_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(fg3m = list(fg3m)), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, OU, Under, Type, avg = round(avg,1),
                                  variation_regular = round(variation_regular,1), minutes, fg3m, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%
  relocate(minutes, .after = Under) %>% relocate(fg3m, .after = minutes)%>% relocate(avg, .after = GP) %>% 
  select(-c(slugTeam,idPlayer, variation_regular))




# reactable(highlight = TRUE, striped = TRUE,fg3m_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
#                                                                                           style = cell_style(font_weight = "bold")),
#                                                                          urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
#                                                                       avg = colDef(name = "Season Avg"),
#                                                                       GP = colDef(name = "Games Played"),
#                                                                       minutes = colDef(name = "Minutes Last 10 GP",cell = function(value,index) {
#                                                                         sparkline(fg3m_picks$minutes[[index]], height = 50)
#                                                                       }),
#                                                                       fg3m = colDef(name = "Total Threes Made Last 10 GP",cell = function(value,index) {
#                                                                         sparkline(fg3m_picks$fg3m[[index]], type = "bar", height = 50)
#                                                                       }),
#                                                                       Under = colDef(name = "Odds"),
#                                                                          OU = colDef(name = "Total Threes Made O/U",width = 110),
#                                                                          `Away Games` = colDef(cell = data_bars(fg3m_picks, 
#                                                                                                                 fill_color = color_set, 
#                                                                                                                 background = '#F1F1F1', 
#                                                                                                                 min_value = 0, 
#                                                                                                                 max_value = 1, 
#                                                                                                                 text_position = 'outside-end',
#                                                                                                                 number_fmt = scales::percent)),
#                                                                          `Home Games` = colDef(cell = data_bars(fg3m_picks, 
#                                                                                                                 fill_color = color_set, 
#                                                                                                                 background = '#F1F1F1', 
#                                                                                                                 min_value = 0, 
#                                                                                                                 max_value = 1, 
#                                                                                                                 text_position = 'outside-end',
#                                                                                                                 number_fmt = scales::percent)),
#                                                                          `Last 10` = colDef(cell = data_bars(fg3m_picks, 
#                                                                                                              fill_color = color_set, 
#                                                                                                              background = '#F1F1F1', 
#                                                                                                              min_value = 0, 
#                                                                                                              max_value = 1, 
#                                                                                                              text_position = 'outside-end',
#                                                                                                              number_fmt = scales::percent)),
#                                                                          `Last 5` = colDef(cell = data_bars(fg3m_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                          `Regular Season` = colDef(cell = data_bars(fg3m_picks, 
#                                                                                                                     fill_color = color_set, 
#                                                                                                                     background = '#F1F1F1', 
#                                                                                                                     min_value = 0, 
#                                                                                                                     max_value = 1, 
#                                                                                                                     text_position = 'outside-end',
#                                                                                                                     number_fmt = scales::percent))),
#           theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"),
#           paginationType = "simple") 
# 


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


stl_df <- stl_df %>% left_join(dk_stl, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(stl), GP = n()), by = "idPlayer") %>% 
  left_join(stl_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

stl_df_join <- stl_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

stl_picks <- stl_df %>% left_join(stl_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(stl = list(stl)), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, stl, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%
  relocate(minutes, .after = Under) %>% relocate(stl, .after = minutes) %>% relocate(GP, .after = stl)%>% select(-c(slugTeam,idPlayer, variation_regular))




# reactable(highlight = TRUE, striped = TRUE,stl_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
#                                                                                          style = cell_style(font_weight = "bold")),
#                                                                      urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
#                                                                      avg = colDef(name = "Season Avg"),
#                                                                      GP = colDef(name = "Games Played"),
#                                                                      minutes = colDef(name = "Minutes Last 10 GP",cell = function(value,index) {
#                                                                        sparkline(stl_picks$minutes[[index]], height = 50)
#                                                                      }),
#                                                                      stl = colDef(name = "Total Steals Last 10 GP",cell = function(value,index) {
#                                                                        sparkline(stl_picks$stl[[index]], type = "bar", height = 50)
#                                                                      }),
#                                                                      Under = colDef(name = "Odds"),
#                                                                      OU = colDef(name = "Total Steals O/U",width = 110),
#                                                                      `Away Games` = colDef(cell = data_bars(stl_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                      `Home Games` = colDef(cell = data_bars(stl_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                      `Last 10` = colDef(cell = data_bars(stl_picks, 
#                                                                                                          fill_color = color_set, 
#                                                                                                          background = '#F1F1F1', 
#                                                                                                          min_value = 0, 
#                                                                                                          max_value = 1, 
#                                                                                                          text_position = 'outside-end',
#                                                                                                          number_fmt = scales::percent)),
#                                                                      `Last 5` = colDef(cell = data_bars(stl_picks, 
#                                                                                                         fill_color = color_set, 
#                                                                                                         background = '#F1F1F1', 
#                                                                                                         min_value = 0, 
#                                                                                                         max_value = 1, 
#                                                                                                         text_position = 'outside-end',
#                                                                                                         number_fmt = scales::percent)),
#                                                                      `Regular Season` = colDef(cell = data_bars(stl_picks, 
#                                                                                                                 fill_color = color_set, 
#                                                                                                                 background = '#F1F1F1', 
#                                                                                                                 min_value = 0, 
#                                                                                                                 max_value = 1, 
#                                                                                                                 text_position = 'outside-end',
#                                                                                                                 number_fmt = scales::percent))),
#           theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"),
#           paginationType = "simple") 

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


blk_df <- blk_df %>% left_join(dk_blk, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(blk), GP = n()), by = "idPlayer") %>% 
  left_join(blk_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

blk_df_join <- blk_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

blk_picks <- blk_df %>% left_join(blk_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(blk = list(blk)), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, blk, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>% 
  relocate(minutes, .after = Under) %>% relocate(blk, .after = minutes) %>% relocate(GP, .after = blk) %>% select(-c(slugTeam,idPlayer, variation_regular))




# reactable(highlight = TRUE, striped = TRUE,blk_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
#                                                                                          style = cell_style(font_weight = "bold")),
#                                                                      urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
#                                                                      avg = colDef(name = "Season Avg"),
#                                                                      GP = colDef(name = "Games Played"),
#                                                                      minutes = colDef(name = "Minutes Last 10 GP",cell = function(value,index) {
#                                                                        sparkline(blk_picks$minutes[[index]], height = 50)
#                                                                      }),
#                                                                      blk = colDef(name = "Total Blocks Last 10 GP",cell = function(value,index) {
#                                                                        sparkline(blk_picks$blk[[index]], type = "bar", height = 50)
#                                                                      }),
#                                                                      Under = colDef(name = "Odds"),
#                                                                      OU = colDef(name = "Total Blocks O/U",width = 110),
#                                                                      `Away Games` = colDef(cell = data_bars(blk_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                      `Home Games` = colDef(cell = data_bars(blk_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                      `Last 10` = colDef(cell = data_bars(blk_picks, 
#                                                                                                          fill_color = color_set, 
#                                                                                                          background = '#F1F1F1', 
#                                                                                                          min_value = 0, 
#                                                                                                          max_value = 1, 
#                                                                                                          text_position = 'outside-end',
#                                                                                                          number_fmt = scales::percent)),
#                                                                      `Last 5` = colDef(cell = data_bars(blk_picks, 
#                                                                                                         fill_color = color_set, 
#                                                                                                         background = '#F1F1F1', 
#                                                                                                         min_value = 0, 
#                                                                                                         max_value = 1, 
#                                                                                                         text_position = 'outside-end',
#                                                                                                         number_fmt = scales::percent)),
#                                                                      `Regular Season` = colDef(cell = data_bars(blk_picks, 
#                                                                                                                 fill_color = color_set, 
#                                                                                                                 background = '#F1F1F1', 
#                                                                                                                 min_value = 0, 
#                                                                                                                 max_value = 1, 
#                                                                                                                 text_position = 'outside-end',
#                                                                                                                 number_fmt = scales::percent))),
#           theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"),
#           paginationType = "simple") 

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


tov_df <- tov_df %>% left_join(dk_tov, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(tov), GP = n()), by = "idPlayer") %>% 
  left_join(tov_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

tov_df_join <- tov_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

tov_picks <- tov_df %>% left_join(tov_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(tov = list(tov)), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, tov, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam)  %>% 
  relocate(minutes, .after = Under) %>% relocate(tov, .after = minutes) %>% relocate(GP, .after = tov)  %>% select(-c(slugTeam,idPlayer, variation_regular))




# reactable(highlight = TRUE, striped = TRUE,tov_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
#                                                                                          style = cell_style(font_weight = "bold")),
#                                                                      urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
#                                                                      avg = colDef(name = "Season Avg"),
#                                                                      GP = colDef(name = "Games Played"),
#                                                                      minutes = colDef(name = "Minutes Last 10 GP",cell = function(value,index) {
#                                                                        sparkline(tov_picks$minutes[[index]], height = 50)
#                                                                      }),
#                                                                      tov = colDef(name = "Total Turnovers Last 10 GP",cell = function(value,index) {
#                                                                        sparkline(tov_picks$tov[[index]], type = "bar", height = 50)
#                                                                      }),
#                                                                      Under = colDef(name = "Odds"),
#                                                                      OU = colDef(name = "Total Turnovers O/U",width = 110),
#                                                                      `Away Games` = colDef(cell = data_bars(tov_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                      `Home Games` = colDef(cell = data_bars(tov_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                      `Last 10` = colDef(cell = data_bars(tov_picks, 
#                                                                                                          fill_color = color_set, 
#                                                                                                          background = '#F1F1F1', 
#                                                                                                          min_value = 0, 
#                                                                                                          max_value = 1, 
#                                                                                                          text_position = 'outside-end',
#                                                                                                          number_fmt = scales::percent)),
#                                                                      `Last 5` = colDef(cell = data_bars(tov_picks, 
#                                                                                                         fill_color = color_set, 
#                                                                                                         background = '#F1F1F1', 
#                                                                                                         min_value = 0, 
#                                                                                                         max_value = 1, 
#                                                                                                         text_position = 'outside-end',
#                                                                                                         number_fmt = scales::percent)),
#                                                                      `Regular Season` = colDef(cell = data_bars(tov_picks, 
#                                                                                                                 fill_color = color_set, 
#                                                                                                                 background = '#F1F1F1', 
#                                                                                                                 min_value = 0, 
#                                                                                                                 max_value = 1, 
#                                                                                                                 text_position = 'outside-end',
#                                                                                                                 number_fmt = scales::percent))),
#           theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"),
#           paginationType = "simple") 

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


pts_df <- pts_df %>% left_join(dk_pts, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(pts), GP = n()), by = "idPlayer") %>% 
  left_join(pts_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

pts_df_join <- pts_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

pts_picks <- pts_df %>% left_join(pts_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(pts = list(pts)), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, pts, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam)  %>% 
  relocate(minutes, .after = Under) %>% relocate(pts, .after = minutes) %>% relocate(GP, .after = pts)  %>% select(-c(slugTeam,idPlayer, variation_regular))




# reactable(highlight = TRUE, striped = TRUE,pts_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
#                                                                                          style = cell_style(font_weight = "bold")),
#                                                                      urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
#                                                                      avg = colDef(name = "Season Avg"),
#                                                                      GP = colDef(name = "Games Played"),
#                                                                      minutes = colDef(name = "Minutes Last 10 GP",cell = function(value,index) {
#                                                                        sparkline(pts_picks$minutes[[index]], height = 50)
#                                                                      }),
#                                                                      pts = colDef(name = "Total Points Last 10 GP",cell = function(value,index) {
#                                                                        sparkline(pts_picks$pts[[index]], type = "bar", height = 50)
#                                                                      }),
#                                                                      Under = colDef(name = "Odds"),
#                                                                      OU = colDef(name = "Total Points O/U",width = 110),
#                                                                      `Away Games` = colDef(cell = data_bars(pts_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                      `Home Games` = colDef(cell = data_bars(pts_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                      `Last 10` = colDef(cell = data_bars(pts_picks, 
#                                                                                                          fill_color = color_set, 
#                                                                                                          background = '#F1F1F1', 
#                                                                                                          min_value = 0, 
#                                                                                                          max_value = 1, 
#                                                                                                          text_position = 'outside-end',
#                                                                                                          number_fmt = scales::percent)),
#                                                                      `Last 5` = colDef(cell = data_bars(pts_picks, 
#                                                                                                         fill_color = color_set, 
#                                                                                                         background = '#F1F1F1', 
#                                                                                                         min_value = 0, 
#                                                                                                         max_value = 1, 
#                                                                                                         text_position = 'outside-end',
#                                                                                                         number_fmt = scales::percent)),
#                                                                      `Regular Season` = colDef(cell = data_bars(pts_picks, 
#                                                                                                                 fill_color = color_set, 
#                                                                                                                 background = '#F1F1F1', 
#                                                                                                                 min_value = 0, 
#                                                                                                                 max_value = 1, 
#                                                                                                                 text_position = 'outside-end',
#                                                                                                                 number_fmt = scales::percent))),
#           theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"),
#           paginationType = "simple") 

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


ast_df <- ast_df %>% left_join(dk_ast, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(ast), GP = n()), by = "idPlayer") %>% 
  left_join(ast_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

ast_df_join <- ast_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

ast_picks <- ast_df %>% left_join(ast_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(ast = list(ast)), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, ast, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam)  %>% 
  relocate(minutes, .after = Under) %>% relocate(ast, .after = minutes) %>% relocate(GP, .after = ast)  %>% select(-c(slugTeam,idPlayer, variation_regular))




# reactable(highlight = TRUE, striped = TRUE,ast_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
#                                                                                          style = cell_style(font_weight = "bold")),
#                                                                      urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
#                                                                      avg = colDef(name = "Season Avg"),
#                                                                      GP = colDef(name = "Games Played"),
#                                                                      minutes = colDef(name = "Minutes Last 10 GP",cell = function(value,index) {
#                                                                        sparkline(ast_picks$minutes[[index]], height = 50)
#                                                                      }),
#                                                                      ast = colDef(name = "Total Assists Last 10 GP",cell = function(value,index) {
#                                                                        sparkline(ast_picks$ast[[index]], type = "bar", height = 50)
#                                                                      }),
#                                                                      Under = colDef(name = "Odds"),
#                                                                      OU = colDef(name = "Total Assists O/U",width = 110),
#                                                                      `Away Games` = colDef(cell = data_bars(ast_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                      `Home Games` = colDef(cell = data_bars(ast_picks, 
#                                                                                                             fill_color = color_set, 
#                                                                                                             background = '#F1F1F1', 
#                                                                                                             min_value = 0, 
#                                                                                                             max_value = 1, 
#                                                                                                             text_position = 'outside-end',
#                                                                                                             number_fmt = scales::percent)),
#                                                                      `Last 10` = colDef(cell = data_bars(ast_picks, 
#                                                                                                          fill_color = color_set, 
#                                                                                                          background = '#F1F1F1', 
#                                                                                                          min_value = 0, 
#                                                                                                          max_value = 1, 
#                                                                                                          text_position = 'outside-end',
#                                                                                                          number_fmt = scales::percent)),
#                                                                      `Last 5` = colDef(cell = data_bars(ast_picks, 
#                                                                                                         fill_color = color_set, 
#                                                                                                         background = '#F1F1F1', 
#                                                                                                         min_value = 0, 
#                                                                                                         max_value = 1, 
#                                                                                                         text_position = 'outside-end',
#                                                                                                         number_fmt = scales::percent)),
#                                                                      `Regular Season` = colDef(cell = data_bars(ast_picks, 
#                                                                                                                 fill_color = color_set, 
#                                                                                                                 background = '#F1F1F1', 
#                                                                                                                 min_value = 0, 
#                                                                                                                 max_value = 1, 
#                                                                                                                 text_position = 'outside-end',
#                                                                                                                 number_fmt = scales::percent))),
#           theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"),
#           paginationType = "simple") 

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


treb_df <- treb_df %>% left_join(dk_reb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(treb), GP = n()), by = "idPlayer") %>% 
  left_join(treb_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

treb_df_join <- treb_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

treb_picks <- treb_df %>% left_join(treb_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(treb = list(treb)), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, treb, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam)  %>% 
  relocate(minutes, .after = Under) %>% relocate(treb, .after = minutes) %>% relocate(GP, .after = treb)  %>% select(-c(slugTeam,idPlayer, variation_regular))




# reactable(highlight = TRUE, striped = TRUE,treb_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
#                                                                                           style = cell_style(font_weight = "bold")),
#                                                                       urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
#                                                                       avg = colDef(name = "Season Avg"),
#                                                                       GP = colDef(name = "Games Played"),
#                                                                       minutes = colDef(name = "Minutes Last 10 GP",cell = function(value,index) {
#                                                                         sparkline(treb_picks$minutes[[index]], height = 50)
#                                                                       }),
#                                                                       treb = colDef(name = "Total Rebounds Last 10 GP",cell = function(value,index) {
#                                                                         sparkline(treb_picks$treb[[index]], type = "bar", height = 50)
#                                                                       }),
#                                                                       Under = colDef(name = "Odds"),
#                                                                       OU = colDef(name = "Total Rebounds O/U",width = 110),
#                                                                       `Away Games` = colDef(cell = data_bars(treb_picks, 
#                                                                                                              fill_color = color_set, 
#                                                                                                              background = '#F1F1F1', 
#                                                                                                              min_value = 0, 
#                                                                                                              max_value = 1, 
#                                                                                                              text_position = 'outside-end',
#                                                                                                              number_fmt = scales::percent)),
#                                                                       `Home Games` = colDef(cell = data_bars(treb_picks, 
#                                                                                                              fill_color = color_set, 
#                                                                                                              background = '#F1F1F1', 
#                                                                                                              min_value = 0, 
#                                                                                                              max_value = 1, 
#                                                                                                              text_position = 'outside-end',
#                                                                                                              number_fmt = scales::percent)),
#                                                                       `Last 10` = colDef(cell = data_bars(treb_picks, 
#                                                                                                           fill_color = color_set, 
#                                                                                                           background = '#F1F1F1', 
#                                                                                                           min_value = 0, 
#                                                                                                           max_value = 1, 
#                                                                                                           text_position = 'outside-end',
#                                                                                                           number_fmt = scales::percent)),
#                                                                       `Last 5` = colDef(cell = data_bars(treb_picks, 
#                                                                                                          fill_color = color_set, 
#                                                                                                          background = '#F1F1F1', 
#                                                                                                          min_value = 0, 
#                                                                                                          max_value = 1, 
#                                                                                                          text_position = 'outside-end',
#                                                                                                          number_fmt = scales::percent)),
#                                                                       `Regular Season` = colDef(cell = data_bars(treb_picks, 
#                                                                                                                  fill_color = color_set, 
#                                                                                                                  background = '#F1F1F1', 
#                                                                                                                  min_value = 0, 
#                                                                                                                  max_value = 1, 
#                                                                                                                  text_position = 'outside-end',
#                                                                                                                  number_fmt = scales::percent))),
#           theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"),
#           paginationType = "simple") 

combined <- bind_rows(ptrebast_picks %>% rename(amount = pts_reb_ast) %>% mutate(Bet = "Total Pts, Reb, Ast"),
                      pt_reb_picks %>% rename(amount = pts_reb) %>% mutate(Bet = "Total Pts, Reb"),ast_reb_picks %>% 
                        rename(amount = ast_reb)%>% mutate(Bet = "Total Ast, Reb"), stl_blk_picks %>% 
                        rename(amount = stl_blk) %>% mutate(Bet = "Total Stl, Blk"), fg3m_picks %>% 
                        rename(amount = fg3m) %>% mutate(Bet = "Total Threes Made"), stl_picks %>% 
                        rename(amount = stl) %>% mutate(Bet = "Total Steals"), blk_picks %>% 
                        rename(amount = blk) %>% mutate(Bet = "Total Blocks"), tov_picks %>% 
                        rename(amount = tov) %>% mutate(Bet = "Total Turnovers"), pts_picks %>% 
                        rename(amount = pts) %>% mutate(Bet = "Total Points"), treb_picks %>% 
                        rename(amount = treb) %>% mutate(Bet = "Total Rebounds"), ast_picks %>% 
                        rename(amount = ast) %>% mutate(Bet = "Total Assists")) %>% relocate(Bet, .before = namePlayer) %>% 
  mutate(namePlayer = paste0(substr(namePlayer,1,1),".",gsub("^\\S+ ", "",namePlayer)))






Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")


rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Cheat_sheet.Rmd',
                  output_file = "prop_bet_cheat_sheet.html",
                  output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Matrix'))




combined_sample <- bind_rows(ptrebast_picks %>% rename(amount = pts_reb_ast) %>% mutate(Bet = "Total Pts, Reb, Ast"),
                      pt_reb_picks %>% rename(amount = pts_reb) %>% mutate(Bet = "Total Pts, Reb"),ast_reb_picks %>% 
                        rename(amount = ast_reb)%>% mutate(Bet = "Total Ast, Reb"), stl_blk_picks %>% 
                        rename(amount = stl_blk) %>% mutate(Bet = "Total Stl, Blk"), fg3m_picks %>% 
                        rename(amount = fg3m) %>% mutate(Bet = "Total Threes Made"), stl_picks %>% 
                        rename(amount = stl) %>% mutate(Bet = "Total Steals"), blk_picks %>% 
                        rename(amount = blk) %>% mutate(Bet = "Total Blocks"), tov_picks %>% 
                        rename(amount = tov) %>% mutate(Bet = "Total Turnovers"), pts_picks %>% 
                        rename(amount = pts) %>% mutate(Bet = "Total Points"), treb_picks %>% 
                        rename(amount = treb) %>% mutate(Bet = "Total Rebounds"), ast_picks %>% 
                        rename(amount = ast) %>% mutate(Bet = "Total Assists")) %>% relocate(Bet, .before = namePlayer) %>% 
  mutate(namePlayer = paste0(substr(namePlayer,1,1),".",gsub("^\\S+ ", "",namePlayer))) %>% arrange(desc(`Regular Season`)) %>% head(10)


rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Cheat_sheet_sample.Rmd',
                  output_file = "prop_bet_cheat_sheet_sample.html",
                  output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Matrix'))
