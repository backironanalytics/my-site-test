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
    
    firstq_makes %>% ungroup() %>% mutate(test = mean(pts > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
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

firstqpoints_df <- firstqpoints_df %>% left_join(dk_firstqpoints, by = c("namePlayer","OU")) %>% filter(!.isna(Over)) %>% rename(season_hit = test) 

firstqpoints_df_join <- firstqpoints_df %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1, 0)) %>% group_by(namePlayer, idPlayer) %>% summarize(Ident = mean(Ident))


dk_firstqpts %>% left_join(firstqpoints %>% rename(label = OU) %>% mutate(label = as.character(label)), by = c("namePlayer","label"))

dk_firstqpts %>% left_join(firstqpoints %>% rename(label = OU) %>% mutate(label = as.character(label)), by = c("namePlayer","label")) %>% 
  filter(test >=0.7) %>% mutate(winnings = 100/(as.numeric(str_remove(odds,"."))/100))


##Assists + Rebounds


ast_reb <- lapply(next_team_batch$idPlayer, function(x){
  slug_team <- all_rosters %>% filter(idPlayer == x) %>% pull(slugTeam)
  
  hit_rate <- seq(5.5,25.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(ast_reb = ast+treb) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,ast_reb)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(ast_reb > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

ast_reb <- bind_rows(ast_reb) %>% unnest(cols = everything())

ast_reb %>% left_join(dk_astreb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test)



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
  
  firstq_assists <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(dateGame,locationGame,typeSeason,slugOpponent,slugSeason, namePlayer,idPlayer,slugTeam) %>% summarize(n = n()) %>% left_join(play_play_assists %>% filter(numberPeriod == 1) %>% group_by(dateGame,typeSeason,locationGame) %>% summarize(assists = n()), by = "dateGame") %>% mutate(assists = replace_na(assists,0)) %>% rename(locationGame = locationGame.x, typeSeason = typeSeason.x)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    firstq_assists %>% ungroup() %>% mutate(test = mean(assists > x), OU = x) %>% group_by(namePlayer, idPlayer, slugTeam, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() %>% mutate(slugTeam = slug_team)
    
  })
  
  hit_rate_above
  
})

firstqassists <- bind_rows(firstqassists) %>% unnest(cols = everything())


dk_firstqast %>% left_join(firstqassists %>% rename(label = OU) %>% mutate(label = as.character(label)), by = c("namePlayer","label"))


