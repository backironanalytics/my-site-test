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

color_set <- c("#f7c844","#429460","#2e6d9e")



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
                                                                                                          ifelse(label == "8+",7.5,ifelse(label == "9+",8.5,ifelse(label == "9+",8.5,ifelse(label == "10+",9.5,label))))))))))

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
                                                                                                          ifelse(label == "8+",7.5,ifelse(label == "9+",8.5,ifelse(label == "9+",8.5,ifelse(label == "10+",9.5,ifelse(label == "2+",1.5,label)))))))))))


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



--
  
  
  
  
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
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,dateGame,locationGame,pts_reb_ast,urlPlayerHeadshot) %>% left_join(slug_team, by = "idPlayer")
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test),average = mean(pts_reb_ast), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ptrebast <- bind_rows(ptrebast) %>% mutate(Type = "Regular Season")

##Pts Reb Ast Home Games


ptrebast_home <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% select(idPlayer,slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,dateGame,locationGame,pts_reb_ast) %>% left_join(slug_team, by = "idPlayer")
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test),average = mean(pts_reb_ast), .groups = 'drop') %>% ungroup()
    
  })
  
  hit_rate_above
  
})

ptrebast_home <- bind_rows(ptrebast_home) %>% mutate(Type = "Home Games")



##Pts Reb Ast Away Games


ptrebast_away <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% select(idPlayer,slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,dateGame,locationGame,pts_reb_ast) %>% left_join(slug_team, by = "idPlayer")
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test),average = mean(pts_reb_ast), .groups = 'drop') %>% ungroup()
    
  })
  
  hit_rate_above
  
})

ptrebast_away <- bind_rows(ptrebast_away) %>% mutate(Type = "Away Games")




##Pts Reb Ast Last 5


ptrebast_five <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% select(idPlayer,slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,dateGame,locationGame,pts_reb_ast) %>% arrange(desc(dateGame)) %>% head(5) %>% left_join(slug_team, by = "idPlayer")
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test),average = mean(pts_reb_ast), .groups = 'drop') %>% ungroup()
    
  })
  
  hit_rate_above
  
})

ptrebast_five <- bind_rows(ptrebast_five) %>% mutate(Type = "Last 5")




##Pts Reb Ast Last 10


ptrebast_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% select(idPlayer,slugTeam)
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,dateGame,locationGame,pts_reb_ast) %>% arrange(desc(dateGame)) %>% head(10) %>% left_join(slug_team, by = "idPlayer")
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb_ast > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test),average = mean(pts_reb_ast), .groups = 'drop') %>% ungroup()
    
  })
  
  hit_rate_above
  
})

ptrebast_ten <- bind_rows(ptrebast_ten) %>% mutate(Type = "Last 10")

ptreb_ast_df <- bind_rows(ptrebast,ptrebast_away,ptrebast_home,ptrebast_five,ptrebast_ten)

ptreb_ast_df$namePlayer <- stri_trans_general(str = ptreb_ast_df$namePlayer, id = "Latin-ASCII")


ptreb_ast_df <- ptreb_ast_df %>% left_join(dk_ptrebast, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test)

ptreb_ast_df_join <- ptreb_ast_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

ptrebast_picks <- ptreb_ast_df %>% left_join(ptreb_ast_df_join, by = c("namePlayer","idPlayer")) %>% 
  filter(Ident != 0) %>% group_by(namePlayer, OU, slugTeam,Type) %>% 
  summarize(season_hit) %>% ungroup() %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = OU) %>% relocate(matchup, .after = urlThumbnailTeam) %>% select(!slugTeam)

reactable(highlight = TRUE, striped = TRUE,ptrebast_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110),
                                                                          urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
          OU = colDef(width = 110),
          `Away Games` = colDef(cell = data_bars(ptrebast_picks, 
                                                 fill_color = color_set, 
                                                 background = '#F1F1F1', 
                                                 min_value = 0, 
                                                 max_value = 1, 
                                                 text_position = 'outside-end',
                                                 number_fmt = scales::percent)),
          `Home Games` = colDef(cell = data_bars(ptrebast_picks, 
                                                 fill_color = color_set, 
                                                 background = '#F1F1F1', 
                                                 min_value = 0, 
                                                 max_value = 1, 
                                                 text_position = 'outside-end',
                                                 number_fmt = scales::percent)),
          `Last 10` = colDef(cell = data_bars(ptrebast_picks, 
                                              fill_color = color_set, 
                                              background = '#F1F1F1', 
                                              min_value = 0, 
                                              max_value = 1, 
                                              text_position = 'outside-end',
                                              number_fmt = scales::percent)),
          `Last 5` = colDef(cell = data_bars(ptrebast_picks, 
                                             fill_color = color_set, 
                                             background = '#F1F1F1', 
                                             min_value = 0, 
                                             max_value = 1, 
                                             text_position = 'outside-end',
                                             number_fmt = scales::percent)),
          `Regular Season` = colDef(cell = data_bars(ptrebast_picks, 
                                                     fill_color = color_set, 
                                                     background = '#F1F1F1', 
                                                     min_value = 0, 
                                                     max_value = 1, 
                                                     text_position = 'outside-end',
                                                     number_fmt = scales::percent))),
          theme = fivethirtyeight(), defaultPageSize = 20, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"), fullWidth = TRUE)


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
  
  play_play_makes <- play_play_player %>% select(idGame,numberPeriod, descriptionPlayHome,descriptionPlayNeutral,descriptionPlayVisitor, namePlayer1,slugScore,scoreHome,scoreAway,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% mutate(description = ifelse(is.na(descriptionPlayHome),ifelse(is.na(descriptionPlayNeutral),descriptionPlayVisitor,descriptionPlayNeutral),descriptionPlayHome)) %>% mutate(free_throw_flag = str_detect(description,"Free Throw"), three_point_flag = str_detect(description,"3PT")) %>% mutate(two_point_flag = ifelse(free_throw_flag == FALSE & three_point_flag == FALSE, TRUE,FALSE)) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam")) %>% mutate(pts = ifelse(free_throw_flag == TRUE,1,ifelse(two_point_flag== TRUE,2,3)),score_type = ifelse(free_throw_flag == TRUE,"Free Throw",ifelse(two_point_flag== TRUE,"2 pt","3 pt")))
  
  firstq_makes <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer,slugTeam) %>% summarize(n = n()) %>% left_join(play_play_makes %>% filter(numberPeriod == 1) %>% group_by(dateGame) %>% summarize(pts = sum(pts)), by = "dateGame") %>% mutate(pts = replace_na(pts,0))
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_makes %>% ungroup() %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup()
    
  })
  
  hit_rate_above
  
})

firstqpoints <- bind_rows(firstqpoints) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##1Q Points Home Games


firstqpoints_home <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,10.5,1)
  
  play_play_player <- play_play %>% filter(idPlayerNBA1 == x, !is.na(slugScore))
  
  play_play_makes <- play_play_player %>% select(idGame,numberPeriod, descriptionPlayHome,descriptionPlayNeutral,descriptionPlayVisitor, namePlayer1,slugScore,scoreHome,scoreAway,slugTeamPlayer1) %>% 
    rename(slugTeam = slugTeamPlayer1) %>% mutate(description = ifelse(is.na(descriptionPlayHome),ifelse(is.na(descriptionPlayNeutral),descriptionPlayVisitor,descriptionPlayNeutral),descriptionPlayHome)) %>% 
    mutate(free_throw_flag = str_detect(description,"Free Throw"), three_point_flag = str_detect(description,"3PT")) %>%
    mutate(two_point_flag = ifelse(free_throw_flag == FALSE & three_point_flag == FALSE, TRUE,FALSE)) %>% 
    left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% 
                summarize(n = n()), by = c("idGame","slugTeam")) %>% 
    mutate(pts = ifelse(free_throw_flag == TRUE,1,ifelse(two_point_flag== TRUE,2,3)),score_type = ifelse(free_throw_flag == TRUE,"Free Throw",ifelse(two_point_flag== TRUE,"2 pt","3 pt")))
  
  firstq_makes <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer) %>% summarize(n = n()) %>% 
    left_join(play_play_makes %>% filter(numberPeriod == 1) %>% group_by(dateGame) %>% summarize(pts = sum(pts)), by = "dateGame") %>% mutate(pts = replace_na(pts,0))
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_makes %>% ungroup() %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

firstqpoints_home <- bind_rows(firstqpoints_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")

##1Q Points Away Games


firstqpoints_away <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,10.5,1)
  
  play_play_player <- play_play %>% filter(idPlayerNBA1 == x, !is.na(slugScore))
  
  play_play_makes <- play_play_player %>% select(idGame,numberPeriod, descriptionPlayHome,descriptionPlayNeutral,descriptionPlayVisitor, namePlayer1,slugScore,scoreHome,scoreAway,slugTeamPlayer1) %>% 
    rename(slugTeam = slugTeamPlayer1) %>% mutate(description = ifelse(is.na(descriptionPlayHome),ifelse(is.na(descriptionPlayNeutral),descriptionPlayVisitor,descriptionPlayNeutral),descriptionPlayHome)) %>% 
    mutate(free_throw_flag = str_detect(description,"Free Throw"), three_point_flag = str_detect(description,"3PT")) %>%
    mutate(two_point_flag = ifelse(free_throw_flag == FALSE & three_point_flag == FALSE, TRUE,FALSE)) %>% 
    left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% 
                summarize(n = n()), by = c("idGame","slugTeam")) %>% 
    mutate(pts = ifelse(free_throw_flag == TRUE,1,ifelse(two_point_flag== TRUE,2,3)),score_type = ifelse(free_throw_flag == TRUE,"Free Throw",ifelse(two_point_flag== TRUE,"2 pt","3 pt")))
  
  firstq_makes <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer) %>% summarize(n = n()) %>% 
    left_join(play_play_makes %>% filter(numberPeriod == 1) %>% group_by(dateGame) %>% summarize(pts = sum(pts)), by = "dateGame") %>% mutate(pts = replace_na(pts,0))
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_makes %>% ungroup() %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

firstqpoints_away <- bind_rows(firstqpoints_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")

##1Q Points Last 10


firstqpoints_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,10.5,1)
  
  play_play_player <- play_play %>% filter(idPlayerNBA1 == x, !is.na(slugScore))
  
  play_play_makes <- play_play_player %>% select(idGame,numberPeriod, descriptionPlayHome,descriptionPlayNeutral,descriptionPlayVisitor, namePlayer1,slugScore,scoreHome,scoreAway,slugTeamPlayer1) %>% 
    rename(slugTeam = slugTeamPlayer1) %>% mutate(description = ifelse(is.na(descriptionPlayHome),ifelse(is.na(descriptionPlayNeutral),descriptionPlayVisitor,descriptionPlayNeutral),descriptionPlayHome)) %>% 
    mutate(free_throw_flag = str_detect(description,"Free Throw"), three_point_flag = str_detect(description,"3PT")) %>%
    mutate(two_point_flag = ifelse(free_throw_flag == FALSE & three_point_flag == FALSE, TRUE,FALSE)) %>% 
    left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% 
                summarize(n = n()), by = c("idGame","slugTeam")) %>% 
    mutate(pts = ifelse(free_throw_flag == TRUE,1,ifelse(two_point_flag== TRUE,2,3)),score_type = ifelse(free_throw_flag == TRUE,"Free Throw",ifelse(two_point_flag== TRUE,"2 pt","3 pt")))
  
  firstq_makes <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer) %>% summarize(n = n()) %>% arrange(desc(dateGame)) %>% head(10) %>% 
    left_join(play_play_makes %>% filter(numberPeriod == 1) %>% group_by(dateGame) %>% summarize(pts = sum(pts)), by = "dateGame") %>% mutate(pts = replace_na(pts,0))
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_makes %>% ungroup() %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

firstqpoints_ten <- bind_rows(firstqpoints_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")


##1Q Points Last 5


firstqpoints_five <- lapply(next_team_batch$idPlayer, function(x){
  
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,10.5,1)
  
  play_play_player <- play_play %>% filter(idPlayerNBA1 == x, !is.na(slugScore))
  
  play_play_makes <- play_play_player %>% select(idGame,numberPeriod, descriptionPlayHome,descriptionPlayNeutral,descriptionPlayVisitor, namePlayer1,slugScore,scoreHome,scoreAway,slugTeamPlayer1) %>% 
    rename(slugTeam = slugTeamPlayer1) %>% mutate(description = ifelse(is.na(descriptionPlayHome),ifelse(is.na(descriptionPlayNeutral),descriptionPlayVisitor,descriptionPlayNeutral),descriptionPlayHome)) %>% 
    mutate(free_throw_flag = str_detect(description,"Free Throw"), three_point_flag = str_detect(description,"3PT")) %>%
    mutate(two_point_flag = ifelse(free_throw_flag == FALSE & three_point_flag == FALSE, TRUE,FALSE)) %>% 
    left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% 
                summarize(n = n()), by = c("idGame","slugTeam")) %>% 
    mutate(pts = ifelse(free_throw_flag == TRUE,1,ifelse(two_point_flag== TRUE,2,3)),score_type = ifelse(free_throw_flag == TRUE,"Free Throw",ifelse(two_point_flag== TRUE,"2 pt","3 pt")))
  
  firstq_makes <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer) %>% summarize(n = n()) %>% arrange(desc(dateGame)) %>% head(5) %>% 
    left_join(play_play_makes %>% filter(numberPeriod == 1) %>% group_by(dateGame) %>% summarize(pts = sum(pts)), by = "dateGame") %>% mutate(pts = replace_na(pts,0))
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_makes %>% ungroup() %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

firstqpoints_five <- bind_rows(firstqpoints_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")

firstqpoints_df <- bind_rows(firstqpoints,firstqpoints_home,firstqpoints_away,firstqpoints_ten,firstqpoints_five)

firstqpoints_df$namePlayer <- stri_trans_general(str = firstqpoints_df$namePlayer, id = "Latin-ASCII")

firstqpoints_df <- firstqpoints_df %>% mutate(OU = as.character(OU)) %>% left_join(dk_firstqpts %>% rename(OU = label), by = c("namePlayer","OU")) %>% 
  filter(!is.na(odds)) %>% rename(season_hit = test) 

firstqpoints_df_join <- firstqpoints_df %>% mutate(favored_ident = substring(odds,1,1)) %>% mutate(odds = as.numeric(str_remove(odds,"."))) %>% 
  mutate(Ident = ifelse(season_hit >= .75 & Type == "Regular Season", 1, 0)) %>% 
  group_by(namePlayer, idPlayer, OU) %>% summarize(Ident = mean(Ident))

firstqpoints_picks <- firstqpoints_df %>% 
  left_join(firstqpoints_df_join, by = c("namePlayer","idPlayer","OU")) %>% filter(Ident!=0) %>% group_by(namePlayer,idPlayer,OU,Type) %>% summarize(season_hit) %>%ungroup() %>%
  pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>%select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>%select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>% relocate(urlThumbnailTeam, .after = namePlayer) %>%
  relocate(matchup, .after = urlThumbnailTeam) %>% 
  mutate(OU = ifelse(OU == "0.5","1+",ifelse(OU == "1.5","2+",ifelse(OU == "2.5","3+", 
                                                                     ifelse(OU == "3.5","4+", ifelse(OU == "4.5","5+", 
                                                                                                     ifelse(OU == "5.5","6+",ifelse(OU == "6.5","7+",
                                                                                                                                    ifelse(OU == "7.5","8+",ifelse(OU == "8.5","9+",OU)))))))))) %>% 
  select(-c(slugTeam,idPlayer))

reactable(highlight = TRUE, striped = TRUE,firstqpoints_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110),
                                                                          urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
                                                                          OU = colDef(name = "Total First Quarter Pts",width = 110),
                                                                          `Away Games` = colDef(cell = data_bars(firstqpoints_picks, 
                                                                                                                 fill_color = color_set, 
                                                                                                                 background = '#F1F1F1', 
                                                                                                                 min_value = 0, 
                                                                                                                 max_value = 1, 
                                                                                                                 text_position = 'outside-end',
                                                                                                                 number_fmt = scales::percent)),
                                                                          `Home Games` = colDef(cell = data_bars(firstqpoints_picks, 
                                                                                                                 fill_color = color_set, 
                                                                                                                 background = '#F1F1F1', 
                                                                                                                 min_value = 0, 
                                                                                                                 max_value = 1, 
                                                                                                                 text_position = 'outside-end',
                                                                                                                 number_fmt = scales::percent)),
                                                                          `Last 10` = colDef(cell = data_bars(firstqpoints_picks, 
                                                                                                              fill_color = color_set, 
                                                                                                              background = '#F1F1F1', 
                                                                                                              min_value = 0, 
                                                                                                              max_value = 1, 
                                                                                                              text_position = 'outside-end',
                                                                                                              number_fmt = scales::percent)),
                                                                          `Last 5` = colDef(cell = data_bars(firstqpoints_picks, 
                                                                                                             fill_color = color_set, 
                                                                                                             background = '#F1F1F1', 
                                                                                                             min_value = 0, 
                                                                                                             max_value = 1, 
                                                                                                             text_position = 'outside-end',
                                                                                                             number_fmt = scales::percent)),
                                                                          `Regular Season` = colDef(cell = data_bars(firstqpoints_picks, 
                                                                                                                     fill_color = color_set, 
                                                                                                                     background = '#F1F1F1', 
                                                                                                                     min_value = 0, 
                                                                                                                     max_value = 1, 
                                                                                                                     text_position = 'outside-end',
                                                                                                                     number_fmt = scales::percent))),
          theme = fivethirtyeight(), defaultPageSize = 20, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"), fullWidth = TRUE)











##Steals + Blocks


stl_blk <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,7.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

stl_blk <- bind_rows(stl_blk) %>% unnest(cols = everything())

stl_blk %>% left_join(dk_stlblk, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test)


##Three Pointers Made


fg3m <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

fg3m <- bind_rows(fg3m) %>% unnest(cols = everything())

fg3m %>% left_join(dk_fg3m, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test)


##1Q Assists


firstqassists <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_assists <- play_play %>% filter(idPlayerNBA2 == x, str_detect(descriptionPlayHome,"AST") | str_detect(descriptionPlayVisitor,"AST"))
  
  play_play_assists <- play_play_player_assists %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,namePlayer2,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam"))
  
  firstq_assists <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason, namePlayer,idPlayer) %>% summarize(n = n()) %>% left_join(play_play_assists %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% summarize(assists = n()), by = "dateGame") %>% mutate(assists = replace_na(assists,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_assists %>% ungroup() %>% mutate(test = mean(assists > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

firstqassists <- bind_rows(firstqassists) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")



##1Q Assists Home Games


firstqassists_home <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_assists <- play_play %>% filter(idPlayerNBA2 == x, str_detect(descriptionPlayHome,"AST") | str_detect(descriptionPlayVisitor,"AST"))
  
  play_play_assists <- play_play_player_assists %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,namePlayer2,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam"))
  
  firstq_assists <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason, namePlayer,idPlayer) %>% summarize(n = n()) %>% 
    left_join(play_play_assists %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% 
                summarize(assists = n()), by = "dateGame") %>% mutate(assists = replace_na(assists,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_assists %>% ungroup() %>% mutate(test = mean(assists > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

firstqassists_home <- bind_rows(firstqassists_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")



##1Q Assists Away Games


firstqassists_away <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_assists <- play_play %>% filter(idPlayerNBA2 == x, str_detect(descriptionPlayHome,"AST") | str_detect(descriptionPlayVisitor,"AST"))
  
  play_play_assists <- play_play_player_assists %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,namePlayer2,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam"))
  
  firstq_assists <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason, namePlayer,idPlayer) %>% summarize(n = n()) %>% 
    left_join(play_play_assists %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% 
                summarize(assists = n()), by = "dateGame") %>% mutate(assists = replace_na(assists,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_assists %>% ungroup() %>% mutate(test = mean(assists > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

firstqassists_away <- bind_rows(firstqassists_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")


##1Q Assists Last 10


firstqassists_ten <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_assists <- play_play %>% filter(idPlayerNBA2 == x, str_detect(descriptionPlayHome,"AST") | str_detect(descriptionPlayVisitor,"AST"))
  
  play_play_assists <- play_play_player_assists %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,namePlayer2,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam"))
  
  firstq_assists <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason, namePlayer,idPlayer) %>% summarize(n = n()) %>% arrange(desc(dateGame)) %>% head(10) %>% 
    left_join(play_play_assists %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% 
                summarize(assists = n()), by = "dateGame") %>% mutate(assists = replace_na(assists,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_assists %>% ungroup() %>% mutate(test = mean(assists > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

firstqassists_ten <- bind_rows(firstqassists_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##1Q Assists Last 5


firstqassists_five <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_assists <- play_play %>% filter(idPlayerNBA2 == x, str_detect(descriptionPlayHome,"AST") | str_detect(descriptionPlayVisitor,"AST"))
  
  play_play_assists <- play_play_player_assists %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,namePlayer2,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam"))
  
  firstq_assists <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason, namePlayer,idPlayer) %>% summarize(n = n()) %>% arrange(desc(dateGame)) %>% head(5) %>% 
    left_join(play_play_assists %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% 
                summarize(assists = n()), by = "dateGame") %>% mutate(assists = replace_na(assists,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_assists %>% ungroup() %>% mutate(test = mean(assists > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

firstqassists_five <- bind_rows(firstqassists_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")

firstqassists_df <- bind_rows(firstqassists,firstqassists_home,firstqassists_away,firstqassists_ten,firstqassists_five)

firstqassists_df$namePlayer <- stri_trans_general(str = firstqassists_df$namePlayer, id = "Latin-ASCII")

firstqassists_df <- firstqassists_df %>% mutate(OU = as.character(OU)) %>% left_join(dk_firstqast %>% rename(OU = label), by = c("namePlayer","OU")) %>% 
  filter(!is.na(odds)) %>% rename(season_hit = test) 

firstqassists_df_join <- firstqassists_df %>% mutate(favored_ident = substring(odds,1,1)) %>% mutate(odds = as.numeric(str_remove(odds,"."))) %>% 
  mutate(Ident = ifelse(season_hit >= .75 & Type == "Regular Season", 1, 0)) %>% 
  group_by(namePlayer, idPlayer, OU) %>% summarize(Ident = mean(Ident))

firstqassists_picks <- firstqassists_df %>% 
  left_join(firstqassists_df_join, by = c("namePlayer","idPlayer","OU")) %>% filter(Ident!=0) %>% group_by(namePlayer,idPlayer,OU,Type) %>% summarize(season_hit) %>%ungroup() %>%
  pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>%select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>%select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>% relocate(urlThumbnailTeam, .after = namePlayer) %>%
  relocate(matchup, .after = urlThumbnailTeam) %>% 
  mutate(OU = ifelse(OU == "0.5","1+",ifelse(OU == "1.5","2+",ifelse(OU == "2.5","3+", 
                                                                     ifelse(OU == "3.5","4+", ifelse(OU == "4.5","5+", 
                                                                                                     ifelse(OU == "5.5","6+",ifelse(OU == "6.5","7+",
                                                                                                                                    ifelse(OU == "7.5","8+",ifelse(OU == "8.5","9+",OU)))))))))) %>% 
  select(-c(slugTeam,idPlayer))


# First Quarter Rebounds


firstqrebounds <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_rebounds <- play_play %>% filter(is.na(slugScore),idPlayerNBA1 == x, str_detect(descriptionPlayHome,"REBOUND") | str_detect(descriptionPlayVisitor,"REBOUND"))
  
  play_play_rebounds <- play_play_player_rebounds %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam")) 
  
  firstq_rebounds <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25")  %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer) %>% summarize(n = n()) %>% left_join(play_play_rebounds %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% summarize(rebounds = n()), by = "dateGame") %>% mutate(rebounds = replace_na(rebounds,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_rebounds %>% ungroup() %>% mutate(test = mean(rebounds > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

firstqrebounds <- bind_rows(firstqrebounds) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


# First Quarter Rebounds - Home Games


firstqrebounds_home <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_rebounds <- play_play %>% filter(is.na(slugScore),idPlayerNBA1 == x, str_detect(descriptionPlayHome,"REBOUND") | str_detect(descriptionPlayVisitor,"REBOUND"))
  
  play_play_rebounds <- play_play_player_rebounds %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam")) 
  
  firstq_rebounds <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H")  %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer) %>% summarize(n = n()) %>% left_join(play_play_rebounds %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% summarize(rebounds = n()), by = "dateGame") %>% mutate(rebounds = replace_na(rebounds,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_rebounds %>% ungroup() %>% mutate(test = mean(rebounds > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

firstqrebounds_home <- bind_rows(firstqrebounds_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


# First Quarter Rebounds - Away Games


firstqrebounds_away <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_rebounds <- play_play %>% filter(is.na(slugScore),idPlayerNBA1 == x, str_detect(descriptionPlayHome,"REBOUND") | str_detect(descriptionPlayVisitor,"REBOUND"))
  
  play_play_rebounds <- play_play_player_rebounds %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam")) 
  
  firstq_rebounds <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A")  %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer) %>% summarize(n = n()) %>% left_join(play_play_rebounds %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% summarize(rebounds = n()), by = "dateGame") %>% mutate(rebounds = replace_na(rebounds,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_rebounds %>% ungroup() %>% mutate(test = mean(rebounds > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

firstqrebounds_away <- bind_rows(firstqrebounds_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



# First Quarter Rebounds - Last 10


firstqrebounds_ten <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_rebounds <- play_play %>% filter(is.na(slugScore),idPlayerNBA1 == x, str_detect(descriptionPlayHome,"REBOUND") | str_detect(descriptionPlayVisitor,"REBOUND"))
  
  play_play_rebounds <- play_play_player_rebounds %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam")) 
  
  firstq_rebounds <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25")  %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer) %>% summarize(n = n()) %>% arrange(desc(dateGame)) %>% head(10) %>% 
    left_join(play_play_rebounds %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% summarize(rebounds = n()), by = "dateGame") %>% mutate(rebounds = replace_na(rebounds,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_rebounds %>% ungroup() %>% mutate(test = mean(rebounds > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

firstqrebounds_ten <- bind_rows(firstqrebounds_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



# First Quarter Rebounds - Last 5


firstqrebounds_five <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(0.5,6.5,1)
  
  
  play_play_player_rebounds <- play_play %>% filter(is.na(slugScore),idPlayerNBA1 == x, str_detect(descriptionPlayHome,"REBOUND") | str_detect(descriptionPlayVisitor,"REBOUND"))
  
  play_play_rebounds <- play_play_player_rebounds %>% select(idGame,numberPeriod,descriptionPlayHome,descriptionPlayVisitor, namePlayer1,slugTeamPlayer1) %>% rename(slugTeam = slugTeamPlayer1) %>% left_join(gamedata %>% group_by(idGame,dateGame,typeSeason,locationGame,slugTeam,slugSeason) %>% summarize(n = n()), by = c("idGame","slugTeam")) 
  
  firstq_rebounds <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25")  %>% 
    group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason,namePlayer,idPlayer) %>% summarize(n = n()) %>% arrange(desc(dateGame)) %>% head(5) %>% 
    left_join(play_play_rebounds %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% summarize(rebounds = n()), by = "dateGame") %>% mutate(rebounds = replace_na(rebounds,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_rebounds %>% ungroup() %>% mutate(test = mean(rebounds > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

firstqrebounds_five <- bind_rows(firstqrebounds_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")


firstqrebounds_df <- bind_rows(firstqrebounds,firstqrebounds_home,firstqrebounds_away,firstqrebounds_ten,firstqrebounds_five)

firstqrebounds_df$namePlayer <- stri_trans_general(str = firstqrebounds_df$namePlayer, id = "Latin-ASCII")

firstqrebounds_df <- firstqrebounds_df %>% mutate(OU = as.character(OU)) %>% left_join(dk_firstqreb %>% rename(OU = label), by = c("namePlayer","OU")) %>% 
  filter(!is.na(odds)) %>% rename(season_hit = test) 

firstqrebounds_df_join <- firstqrebounds_df %>% mutate(favored_ident = substring(odds,1,1)) %>% mutate(odds = as.numeric(str_remove(odds,"."))) %>% 
  mutate(Ident = ifelse(season_hit >= .70 & Type == "Regular Season", 1, 0)) %>% 
  group_by(namePlayer, idPlayer, OU) %>% summarize(Ident = mean(Ident))

firstqrebounds_picks <- firstqrebounds_df %>% 
  left_join(firstqrebounds_df_join, by = c("namePlayer","idPlayer","OU")) %>% filter(Ident!=0) %>% group_by(namePlayer,idPlayer,OU,Type) %>% summarize(season_hit) %>%ungroup() %>%
  pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>%select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>%select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>% relocate(urlThumbnailTeam, .after = namePlayer) %>%
  relocate(matchup, .after = urlThumbnailTeam) %>% 
  mutate(OU = ifelse(OU == "0.5","1+",ifelse(OU == "1.5","2+",ifelse(OU == "2.5","3+", 
                                                                     ifelse(OU == "3.5","4+", ifelse(OU == "4.5","5+", 
                                                                                                     ifelse(OU == "5.5","6+",ifelse(OU == "6.5","7+",
                                                                                                                                    ifelse(OU == "7.5","8+",ifelse(OU == "8.5","9+",OU)))))))))) %>% 
  select(-c(slugTeam,idPlayer))



##Points + Rebounds


pt_reb <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb = pts+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup()
    
  })
  
  hit_rate_above
  
})

pt_reb <- bind_rows(pt_reb) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")



##Points + Rebounds Home Games


pt_reb_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb = pts+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup()
    
  })
  
  hit_rate_above
  
})

pt_reb_home <- bind_rows(pt_reb_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")



##Points + Rebounds Away Games


pt_reb_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb = pts+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup()
    
  })
  
  hit_rate_above
  
})

pt_reb_away <- bind_rows(pt_reb_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Points + Rebounds Last 10


pt_reb_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb = pts+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup()
    
  })
  
  hit_rate_above
  
})

pt_reb_ten <- bind_rows(pt_reb_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")


##Points + Rebounds Last 5


pt_reb_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(10.5,60.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb = pts+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,pts_reb) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(pts_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup()
    
  })
  
  hit_rate_above
  
})

pt_reb_five <- bind_rows(pt_reb_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")



pt_reb_df <- bind_rows(pt_reb,pt_reb_away,pt_reb_home,pt_reb_five,pt_reb_ten)

pt_reb_df$namePlayer <- stri_trans_general(str = pt_reb_df$namePlayer, id = "Latin-ASCII")


pt_reb_df <- pt_reb_df %>% left_join(dk_ptreb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test)

pt_reb_df_join <- pt_reb_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

pt_reb_picks <- pt_reb_df %>% left_join(pt_reb_df_join, by = c("namePlayer","idPlayer")) %>% 
  filter(Ident != 0) %>% group_by(namePlayer,idPlayer, OU, Type) %>% 
  summarize(season_hit) %>% ungroup() %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>% select(-c(slugTeam,idPlayer))



reactable(highlight = TRUE, striped = TRUE,pt_reb_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110),
                                                                          urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
                                                                          OU = colDef(name = "Total Points & Rebounds O/U",width = 110),
                                                                          `Away Games` = colDef(cell = data_bars(pt_reb_picks, 
                                                                                                                 fill_color = color_set, 
                                                                                                                 background = '#F1F1F1', 
                                                                                                                 min_value = 0, 
                                                                                                                 max_value = 1, 
                                                                                                                 text_position = 'outside-end',
                                                                                                                 number_fmt = scales::percent)),
                                                                          `Home Games` = colDef(cell = data_bars(pt_reb_picks, 
                                                                                                                 fill_color = color_set, 
                                                                                                                 background = '#F1F1F1', 
                                                                                                                 min_value = 0, 
                                                                                                                 max_value = 1, 
                                                                                                                 text_position = 'outside-end',
                                                                                                                 number_fmt = scales::percent)),
                                                                          `Last 10` = colDef(cell = data_bars(pt_reb_picks, 
                                                                                                              fill_color = color_set, 
                                                                                                              background = '#F1F1F1', 
                                                                                                              min_value = 0, 
                                                                                                              max_value = 1, 
                                                                                                              text_position = 'outside-end',
                                                                                                              number_fmt = scales::percent)),
                                                                          `Last 5` = colDef(cell = data_bars(pt_reb_picks, 
                                                                                                             fill_color = color_set, 
                                                                                                             background = '#F1F1F1', 
                                                                                                             min_value = 0, 
                                                                                                             max_value = 1, 
                                                                                                             text_position = 'outside-end',
                                                                                                             number_fmt = scales::percent)),
                                                                          `Regular Season` = colDef(cell = data_bars(pt_reb_picks, 
                                                                                                                     fill_color = color_set, 
                                                                                                                     background = '#F1F1F1', 
                                                                                                                     min_value = 0, 
                                                                                                                     max_value = 1, 
                                                                                                                     text_position = 'outside-end',
                                                                                                                     number_fmt = scales::percent))),
          theme = fivethirtyeight(), defaultPageSize = 20, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"), fullWidth = TRUE)



##Assists + Rebounds


ast_reb <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(5.5,25.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_reb <- bind_rows(ast_reb) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Assists + Rebounds Home Games


ast_reb_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(5.5,25.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_reb_home <- bind_rows(ast_reb_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Assists + Rebounds Away Games


ast_reb_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(5.5,25.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_reb_away <- bind_rows(ast_reb_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")


##Assists + Rebounds Last 10


ast_reb_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(5.5,25.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_reb_ten <- bind_rows(ast_reb_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Assists + Rebounds Last 5


ast_reb_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(5.5,25.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

ast_reb_five <- bind_rows(ast_reb_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")


ast_reb_df <- bind_rows(ast_reb,ast_reb_away,ast_reb_home,ast_reb_five,ast_reb_ten)

ast_reb_df$namePlayer <- stri_trans_general(str = ast_reb_df$namePlayer, id = "Latin-ASCII")


ast_reb_df <- ast_reb_df %>% left_join(dk_astreb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test)

ast_reb_df_join <- ast_reb_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

ast_reb_picks <- ast_reb_df %>% left_join(ast_reb_df_join, by = c("namePlayer","idPlayer")) %>% 
  filter(Ident != 0) %>% group_by(namePlayer,idPlayer, OU, Type) %>% 
  summarize(season_hit) %>% ungroup() %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>% select(-c(slugTeam,idPlayer))


reactable(highlight = TRUE, striped = TRUE,ast_reb_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110),
                                                                        urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
                                                                        OU = colDef(name = "Total Assists & Rebounds O/U",width = 110),
                                                                        `Away Games` = colDef(cell = data_bars(ast_reb_picks, 
                                                                                                               fill_color = color_set, 
                                                                                                               background = '#F1F1F1', 
                                                                                                               min_value = 0, 
                                                                                                               max_value = 1, 
                                                                                                               text_position = 'outside-end',
                                                                                                               number_fmt = scales::percent)),
                                                                        `Home Games` = colDef(cell = data_bars(ast_reb_picks, 
                                                                                                               fill_color = color_set, 
                                                                                                               background = '#F1F1F1', 
                                                                                                               min_value = 0, 
                                                                                                               max_value = 1, 
                                                                                                               text_position = 'outside-end',
                                                                                                               number_fmt = scales::percent)),
                                                                        `Last 10` = colDef(cell = data_bars(ast_reb_picks, 
                                                                                                            fill_color = color_set, 
                                                                                                            background = '#F1F1F1', 
                                                                                                            min_value = 0, 
                                                                                                            max_value = 1, 
                                                                                                            text_position = 'outside-end',
                                                                                                            number_fmt = scales::percent)),
                                                                        `Last 5` = colDef(cell = data_bars(ast_reb_picks, 
                                                                                                           fill_color = color_set, 
                                                                                                           background = '#F1F1F1', 
                                                                                                           min_value = 0, 
                                                                                                           max_value = 1, 
                                                                                                           text_position = 'outside-end',
                                                                                                           number_fmt = scales::percent)),
                                                                        `Regular Season` = colDef(cell = data_bars(ast_reb_picks, 
                                                                                                                   fill_color = color_set, 
                                                                                                                   background = '#F1F1F1', 
                                                                                                                   min_value = 0, 
                                                                                                                   max_value = 1, 
                                                                                                                   text_position = 'outside-end',
                                                                                                                   number_fmt = scales::percent))),
          theme = fivethirtyeight(), defaultPageSize = 20, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"), fullWidth = TRUE)




##Steals + Blocks


stl_blk <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,7.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_blk <- bind_rows(stl_blk) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")



##Steals + Blocks Home Games


stl_blk_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,7.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_blk_home <- bind_rows(stl_blk_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Steals + Blocks Away Games


stl_blk_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,7.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_blk_away <- bind_rows(stl_blk_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Steals + Blocks Last 10


stl_blk_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,7.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_blk_ten <- bind_rows(stl_blk_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Steals + Blocks Last 5


stl_blk_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,7.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(stl_blk = stl+blk) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl_blk) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl_blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_blk_five <- bind_rows(stl_blk_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")


stl_blk_df <- bind_rows(stl_blk,stl_blk_away,stl_blk_home,stl_blk_five,stl_blk_ten)

stl_blk_df$namePlayer <- stri_trans_general(str = stl_blk_df$namePlayer, id = "Latin-ASCII")


stl_blk_df <- stl_blk_df %>% left_join(dk_stlblk, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test)

stl_blk_df_join <- stl_blk_df  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

stl_blk_picks <- stl_blk_df %>% left_join(stl_blk_df_join, by = c("namePlayer","idPlayer")) %>% 
  filter(Ident != 0) %>% group_by(namePlayer,idPlayer, OU, Type) %>% 
  summarize(season_hit) %>% ungroup() %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>% select(-c(slugTeam,idPlayer))




reactable(highlight = TRUE, striped = TRUE,stl_blk_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110),
                                                                         urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
                                                                         OU = colDef(name = "Total Steals & Blocks O/U",width = 110),
                                                                         `Away Games` = colDef(cell = data_bars(stl_blk_picks, 
                                                                                                                fill_color = color_set, 
                                                                                                                background = '#F1F1F1', 
                                                                                                                min_value = 0, 
                                                                                                                max_value = 1, 
                                                                                                                text_position = 'outside-end',
                                                                                                                number_fmt = scales::percent)),
                                                                         `Home Games` = colDef(cell = data_bars(stl_blk_picks, 
                                                                                                                fill_color = color_set, 
                                                                                                                background = '#F1F1F1', 
                                                                                                                min_value = 0, 
                                                                                                                max_value = 1, 
                                                                                                                text_position = 'outside-end',
                                                                                                                number_fmt = scales::percent)),
                                                                         `Last 10` = colDef(cell = data_bars(stl_blk_picks, 
                                                                                                             fill_color = color_set, 
                                                                                                             background = '#F1F1F1', 
                                                                                                             min_value = 0, 
                                                                                                             max_value = 1, 
                                                                                                             text_position = 'outside-end',
                                                                                                             number_fmt = scales::percent)),
                                                                         `Last 5` = colDef(cell = data_bars(stl_blk_picks, 
                                                                                                            fill_color = color_set, 
                                                                                                            background = '#F1F1F1', 
                                                                                                            min_value = 0, 
                                                                                                            max_value = 1, 
                                                                                                            text_position = 'outside-end',
                                                                                                            number_fmt = scales::percent)),
                                                                         `Regular Season` = colDef(cell = data_bars(stl_blk_picks, 
                                                                                                                    fill_color = color_set, 
                                                                                                                    background = '#F1F1F1', 
                                                                                                                    min_value = 0, 
                                                                                                                    max_value = 1, 
                                                                                                                    text_position = 'outside-end',
                                                                                                                    number_fmt = scales::percent))),
          theme = fivethirtyeight(), defaultPageSize = 20, searchable = TRUE, language = reactableLang(searchPlaceholder = "SEARCH FOR A PLAYER"), fullWidth = TRUE)
