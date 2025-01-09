## Over Success Rates

##Three Pointers Made


fg3m <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,6.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

fg3m <- bind_rows(fg3m) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Three Pointers Made Home Games


fg3m_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,6.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

fg3m_home <- bind_rows(fg3m_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")


##Three Pointers Made Away Games


fg3m_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,6.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

fg3m_away <- bind_rows(fg3m_away) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")



##Three Pointers Made Last 10


fg3m_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,6.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

fg3m_ten <- bind_rows(fg3m_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")

##Three Pointers Made Last 5


fg3m_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,6.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,fg3m) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(fg3m > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

fg3m_five <- bind_rows(fg3m_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")



fg3m_df <- bind_rows(fg3m,fg3m_away,fg3m_home,fg3m_five,fg3m_ten)

fg3m_df$namePlayer <- stri_trans_general(str = fg3m_df$namePlayer, id = "Latin-ASCII")


fg3m_df <- fg3m_df %>% left_join(dk_fg3m, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% summarize(avg = mean(fg3m)), by = "idPlayer")

fg3m_df_join <- fg3m_df  %>% mutate(Ident = ifelse(season_hit >= .80 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

fg3m_picks <- fg3m_df %>% left_join(fg3m_df_join, by = c("namePlayer","idPlayer")) %>% 
  filter(Ident != 0) %>% group_by(namePlayer,idPlayer, OU, Type, avg = as.character(round(avg,1))) %>% 
  summarize(season_hit) %>% ungroup() %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>% select(-c(slugTeam,idPlayer))

reactable(highlight = TRUE, striped = TRUE,fg3m_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110),
                                                                      urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
                                                                      avg = colDef(name = "Season Avg"),
                                                                      OU = colDef(name = "Total Threes Made O/U",width = 110),
                                                                      `Away Games` = colDef(cell = data_bars(fg3m_picks, 
                                                                                                             fill_color = color_set, 
                                                                                                             background = '#F1F1F1', 
                                                                                                             min_value = 0, 
                                                                                                             max_value = 1, 
                                                                                                             text_position = 'outside-end',
                                                                                                             number_fmt = scales::percent)),
                                                                      `Home Games` = colDef(cell = data_bars(fg3m_picks, 
                                                                                                             fill_color = color_set, 
                                                                                                             background = '#F1F1F1', 
                                                                                                             min_value = 0, 
                                                                                                             max_value = 1, 
                                                                                                             text_position = 'outside-end',
                                                                                                             number_fmt = scales::percent)),
                                                                      `Last 10` = colDef(cell = data_bars(fg3m_picks, 
                                                                                                          fill_color = color_set, 
                                                                                                          background = '#F1F1F1', 
                                                                                                          min_value = 0, 
                                                                                                          max_value = 1, 
                                                                                                          text_position = 'outside-end',
                                                                                                          number_fmt = scales::percent)),
                                                                      `Last 5` = colDef(cell = data_bars(fg3m_picks, 
                                                                                                         fill_color = color_set, 
                                                                                                         background = '#F1F1F1', 
                                                                                                         min_value = 0, 
                                                                                                         max_value = 1, 
                                                                                                         text_position = 'outside-end',
                                                                                                         number_fmt = scales::percent)),
                                                                      `Regular Season` = colDef(cell = data_bars(fg3m_picks, 
                                                                                                                 fill_color = color_set, 
                                                                                                                 background = '#F1F1F1', 
                                                                                                                 min_value = 0, 
                                                                                                                 max_value = 1, 
                                                                                                                 text_position = 'outside-end',
                                                                                                                 number_fmt = scales::percent))),
          theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE) %>% add_title("Over Success Rates") %>% add_subtitle("2024/25 Regular Season")


##Steals


stl <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl <- bind_rows(stl) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Steals Home Games


stl_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_home <- bind_rows(stl_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")



##Steals Away Games


stl_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_away <- bind_rows(stl_home) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")


##Steals Last 10


stl_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_ten <- bind_rows(stl_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Steals Last 5


stl_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,stl) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(stl > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

stl_five <- bind_rows(stl_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")

stl_df <- bind_rows(stl,stl_away,stl_home,stl_five,stl_ten)

stl_df$namePlayer <- stri_trans_general(str = stl_df$namePlayer, id = "Latin-ASCII")


stl_df <- stl_df %>% left_join(dk_stl, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(avg = mean(stl)), by = "idPlayer")

stl_df_join <- stl_df  %>% mutate(Ident = ifelse(season_hit >= .70 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

stl_picks <- stl_df %>% left_join(stl_df_join, by = c("namePlayer","idPlayer")) %>% 
  filter(Ident != 0) %>% group_by(namePlayer,idPlayer, OU, Type, avg = as.character(round(avg,1))) %>% 
  summarize(season_hit) %>% ungroup()  %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>% select(-c(slugTeam,idPlayer))




reactable(highlight = TRUE, striped = TRUE,stl_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110),
                                                                     urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
                                                                     avg = colDef(name = "Season Avg"),
                                                                     OU = colDef(name = "Total Steals O/U",width = 110),
                                                                     `Away Games` = colDef(cell = data_bars(stl_picks, 
                                                                                                            fill_color = color_set, 
                                                                                                            background = '#F1F1F1', 
                                                                                                            min_value = 0, 
                                                                                                            max_value = 1, 
                                                                                                            text_position = 'outside-end',
                                                                                                            number_fmt = scales::percent)),
                                                                     `Home Games` = colDef(cell = data_bars(stl_picks, 
                                                                                                            fill_color = color_set, 
                                                                                                            background = '#F1F1F1', 
                                                                                                            min_value = 0, 
                                                                                                            max_value = 1, 
                                                                                                            text_position = 'outside-end',
                                                                                                            number_fmt = scales::percent)),
                                                                     `Last 10` = colDef(cell = data_bars(stl_picks, 
                                                                                                         fill_color = color_set, 
                                                                                                         background = '#F1F1F1', 
                                                                                                         min_value = 0, 
                                                                                                         max_value = 1, 
                                                                                                         text_position = 'outside-end',
                                                                                                         number_fmt = scales::percent)),
                                                                     `Last 5` = colDef(cell = data_bars(stl_picks, 
                                                                                                        fill_color = color_set, 
                                                                                                        background = '#F1F1F1', 
                                                                                                        min_value = 0, 
                                                                                                        max_value = 1, 
                                                                                                        text_position = 'outside-end',
                                                                                                        number_fmt = scales::percent)),
                                                                     `Regular Season` = colDef(cell = data_bars(stl_picks, 
                                                                                                                fill_color = color_set, 
                                                                                                                background = '#F1F1F1', 
                                                                                                                min_value = 0, 
                                                                                                                max_value = 1, 
                                                                                                                text_position = 'outside-end',
                                                                                                                number_fmt = scales::percent))),
          theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE) %>% add_title("Over Success Rates") %>% add_subtitle("2024/25 Regular Season")



##Blocks


blk <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

blk <- bind_rows(blk) %>% unnest(cols = everything()) %>% mutate(Type = "Regular Season")


##Blocks Home Games


blk_home <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "H") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

blk_home <- bind_rows(blk_home) %>% unnest(cols = everything()) %>% mutate(Type = "Home Games")



##Blocks Away Games


blk_away <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25", locationGame == "A") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,blk)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

blk_away <- bind_rows(blk_home) %>% unnest(cols = everything()) %>% mutate(Type = "Away Games")


##Blocks Last 10


blk_ten <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,blk) %>% arrange(desc(dateGame)) %>% head(10)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

blk_ten <- bind_rows(blk_ten) %>% unnest(cols = everything()) %>% mutate(Type = "Last 10")



##Blocks Last 5


blk_five <- lapply(next_team_batch$idPlayer, function(x){
  
  
  hit_rate <- seq(0.5,5.5,1)
  
  df <- playerdata %>% filter(idPlayer == x, typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
    mutate(pts_reb_ast = pts+treb+ast) %>% select(namePlayer,idPlayer,slugTeam,dateGame,locationGame,blk) %>% arrange(desc(dateGame)) %>% head(5)
  
  hit_rate_above <- lapply(hit_rate, function(x){
    
    df %>% mutate(test = mean(blk > x), OU = x) %>% group_by(namePlayer, idPlayer, OU) %>% summarize(test = min(test), .groups = 'drop') %>% 
      ungroup() 
    
  })
  
  hit_rate_above
  
})

blk_five <- bind_rows(blk_five) %>% unnest(cols = everything()) %>% mutate(Type = "Last 5")

blk_df <- bind_rows(blk,blk_away,blk_home,blk_five,blk_ten)

blk_df$namePlayer <- stri_trans_general(str = blk_df$namePlayer, id = "Latin-ASCII")


blk_df <- blk_df %>% left_join(dk_blk, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(avg = mean(blk)), by = "idPlayer")

blk_df_join <- blk_df  %>% mutate(Ident = ifelse(season_hit >= .80 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

blk_picks <- blk_df %>% left_join(blk_df_join, by = c("namePlayer","idPlayer")) %>% 
  filter(Ident != 0) %>% group_by(namePlayer,idPlayer, OU, Type, avg = as.character(round(avg,1))) %>% 
  summarize(season_hit) %>% ungroup()  %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>% select(-c(slugTeam,idPlayer))




reactable(highlight = TRUE, striped = TRUE,blk_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110),
                                                                     urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
                                                                     avg = colDef(name = "Season Avg"),
                                                                     OU = colDef(name = "Total Blocks O/U",width = 110),
                                                                     `Away Games` = colDef(cell = data_bars(blk_picks, 
                                                                                                            fill_color = color_set, 
                                                                                                            background = '#F1F1F1', 
                                                                                                            min_value = 0, 
                                                                                                            max_value = 1, 
                                                                                                            text_position = 'outside-end',
                                                                                                            number_fmt = scales::percent)),
                                                                     `Home Games` = colDef(cell = data_bars(blk_picks, 
                                                                                                            fill_color = color_set, 
                                                                                                            background = '#F1F1F1', 
                                                                                                            min_value = 0, 
                                                                                                            max_value = 1, 
                                                                                                            text_position = 'outside-end',
                                                                                                            number_fmt = scales::percent)),
                                                                     `Last 10` = colDef(cell = data_bars(blk_picks, 
                                                                                                         fill_color = color_set, 
                                                                                                         background = '#F1F1F1', 
                                                                                                         min_value = 0, 
                                                                                                         max_value = 1, 
                                                                                                         text_position = 'outside-end',
                                                                                                         number_fmt = scales::percent)),
                                                                     `Last 5` = colDef(cell = data_bars(blk_picks, 
                                                                                                        fill_color = color_set, 
                                                                                                        background = '#F1F1F1', 
                                                                                                        min_value = 0, 
                                                                                                        max_value = 1, 
                                                                                                        text_position = 'outside-end',
                                                                                                        number_fmt = scales::percent)),
                                                                     `Regular Season` = colDef(cell = data_bars(blk_picks, 
                                                                                                                fill_color = color_set, 
                                                                                                                background = '#F1F1F1', 
                                                                                                                min_value = 0, 
                                                                                                                max_value = 1, 
                                                                                                                text_position = 'outside-end',
                                                                                                                number_fmt = scales::percent))),
          theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE) %>% add_title("Over Success Rates") %>% add_subtitle("2024/25 Regular Season")


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


ptreb_ast_df <- ptreb_ast_df %>% left_join(dk_ptrebast, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = pts+treb+ast) %>% group_by(idPlayer) %>% summarize(avg = mean(avg)), by = "idPlayer")

ptreb_ast_df_join <- ptreb_ast_df  %>% mutate(Ident = ifelse(season_hit >= .80 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

ptrebast_picks <- ptreb_ast_df %>% left_join(ptreb_ast_df_join, by = c("namePlayer","idPlayer")) %>% 
  filter(Ident != 0) %>% group_by(namePlayer, OU, slugTeam,Type, avg = as.character(round(avg,1))) %>% 
  summarize(season_hit) %>% ungroup()  %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = OU) %>% relocate(matchup, .after = urlThumbnailTeam) %>% select(!slugTeam)

reactable(highlight = TRUE, ptrebast_picks, columns = list(namePlayer = colDef(name = "Player",sticky = "left", width = 110,
                                                                               style = cell_style(font_weight = "bold")),
                                                           urlThumbnailTeam = colDef(name = "Team",cell = embed_img(height = "25",width="25")),
                                                           avg = colDef(name = "Season Avg"),
                                                           OU = colDef(name = "Total Points, Rebounds, Assists OU",width = 110),
                                                           `Away Games` = colDef(cell = data_bars(ptrebast_picks, 
                                                                                                  fill_color = color_set, 
                                                                                                  background = '#F1F1F1', 
                                                                                                  min_value = 0, 
                                                                                                  max_value = 1, 
                                                                                                  text_position = 'outside-end',
                                                                                                  number_fmt = scales::percent,
                                                                                                  bold_text = TRUE)),
                                                           `Home Games` = colDef(cell = data_bars(ptrebast_picks, 
                                                                                                  fill_color = color_set, 
                                                                                                  background = '#F1F1F1', 
                                                                                                  min_value = 0, 
                                                                                                  max_value = 1, 
                                                                                                  text_position = 'outside-end',
                                                                                                  number_fmt = scales::percent,
                                                                                                  bold_text = TRUE)),
                                                           `Last 10` = colDef(cell = data_bars(ptrebast_picks, 
                                                                                               fill_color = color_set, 
                                                                                               background = '#F1F1F1', 
                                                                                               min_value = 0, 
                                                                                               max_value = 1, 
                                                                                               text_position = 'outside-end',
                                                                                               number_fmt = scales::percent,
                                                                                               bold_text = TRUE)),
                                                           `Last 5` = colDef(cell = data_bars(ptrebast_picks, 
                                                                                              fill_color = color_set, 
                                                                                              background = '#F1F1F1', 
                                                                                              min_value = 0, 
                                                                                              max_value = 1, 
                                                                                              text_position = 'outside-end',
                                                                                              number_fmt = scales::percent,
                                                                                              bold_text = TRUE)),
                                                           `Regular Season` = colDef(cell = data_bars(ptrebast_picks, 
                                                                                                      fill_color = color_set, 
                                                                                                      background = '#F1F1F1', 
                                                                                                      min_value = 0, 
                                                                                                      max_value = 1, 
                                                                                                      text_position = 'outside-end',
                                                                                                      number_fmt = scales::percent,
                                                                                                      bold_text = TRUE))),
          theme = fivethirtyeight(), defaultPageSize = 20, fullWidth = TRUE) %>% add_title("Over Success Rates") %>% add_subtitle("2024/25 Regular Season")

