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

dk_ptast <- json_data[5]

dk_ptast <- as.data.frame(dk_ptast)

dk_ptast <- unnest(dk_ptast)

dk_ptast <- dk_ptast %>% filter(selections.tags == "MainPointLine") %>% select(seoIdentifier,selections.label,american,selections.points) %>% 
  rename(namePlayer = seoIdentifier,label = selections.label,odds = american,OU = selections.points) %>% pivot_wider(names_from = label, values_from = odds)

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




#Pt Reb Ast

ptreb_ast_df_new <- ptreb_ast_df %>% left_join(dk_ptrebast, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = pts+treb+ast) %>% group_by(idPlayer) %>% summarize(avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(ptreb_ast_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer") %>%
  left_join(ptreb_ast_df %>% filter(Type == "Home Games") %>% group_by(idPlayer) %>% summarize(variation_home = mean(sd)), by = "idPlayer") %>%
  left_join(ptreb_ast_df %>% filter(Type == "Away Games") %>% group_by(idPlayer) %>% summarize(variation_away = mean(sd)), by = "idPlayer") %>%
  left_join(ptreb_ast_df %>% filter(Type == "Last 10") %>% group_by(idPlayer) %>% summarize(variation_ten = mean(sd)), by = "idPlayer") %>%
  left_join(ptreb_ast_df %>% filter(Type == "Last 5") %>% group_by(idPlayer) %>% summarize(variation_five = mean(sd)), by = "idPlayer")

ptreb_ast_df_join <- ptreb_ast_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Last 10", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

ptrebast_picks <- ptreb_ast_df_new %>% left_join(ptreb_ast_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten %>% 
              mutate(pts_reb_ast = pts+treb+ast) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(pts_reb_ast = list(pts_reb_ast)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, avg = round(avg,1),
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


#Pt Reb

pt_reb_df_new <- pt_reb_df %>% left_join(dk_ptreb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = pts+treb) %>% group_by(idPlayer) %>% summarize(avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(pt_reb_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

pt_reb_df_join <- pt_reb_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

pt_reb_picks <- pt_reb_df_new %>% left_join(pt_reb_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten %>% mutate(pts_reb = pts+treb) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(pts_reb = list(pts_reb)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer,urlPlayerHeadshot, OU,Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, pts_reb, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>% pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% 
  relocate(matchup, .after = urlThumbnailTeam) %>% relocate(minutes, .after = Under) %>% relocate(pts_reb, .after = minutes) %>% relocate(GP, .after = pts_reb) %>% 
  select(-c(slugTeam, variation_regular))


# Ast Reb

ast_reb_df_new <- ast_reb_df %>% left_join(dk_astreb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = ast+treb) %>% group_by(idPlayer) %>% summarize(avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(ast_reb_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

ast_reb_df_join <- ast_reb_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Last 5", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

ast_reb_picks <- ast_reb_df_new %>% left_join(ast_reb_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten %>% mutate(ast_reb = ast+treb) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(ast_reb = list(ast_reb)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer")  %>% 
  group_by(namePlayer,idPlayer,urlPlayerHeadshot, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, ast_reb, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%
  relocate(minutes, .after = Under) %>% relocate(ast_reb, .after = minutes) %>% relocate(GP, .after = ast_reb) %>% select(-c(slugTeam, variation_regular))

# Pts Ast
 
# pt_ast_df_new <- pt_ast_df %>% left_join(dk_ptast, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>%
#   left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>%
#               mutate(avg = ast+treb) %>% group_by(idPlayer) %>% summarize(avg = mean(avg), GP = n()), by = "idPlayer") %>%
#   left_join(pt_ast_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")
# 
# pt_ast_df_join <- pt_ast_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Last 5", 1,0)) %>%
#   group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))
# 
# pt_ast_picks <- pt_ast_df_new %>% left_join(pt_ast_df_join, by = c("namePlayer","idPlayer")) %>%
#   left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
#   left_join(min_ten %>% mutate(pt_ast = ast+treb) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(pt_ast = list(pt_ast)), by = "idPlayer") %>%
#   left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer")  %>%
#   group_by(namePlayer,idPlayer,urlPlayerHeadshot, OU, Under, Type, avg = round(avg,1),
#            variation_regular = round(variation_regular,1), minutes, pt_ast, GP) %>%
#   summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>%
#   left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>%
#   left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>%
#   left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
#   relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%
#   relocate(minutes, .after = Under) %>% relocate(pt_ast, .after = minutes) %>% relocate(GP, .after = pt_ast) %>% select(-c(slugTeam, variation_regular))


#Stl Blk

stl_blk_df_new <- stl_blk_df %>% left_join(dk_stlblk, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = stl+blk) %>% group_by(idPlayer) %>% summarize(avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(stl_blk_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% 
              summarize(variation_regular = mean(sd)), by = "idPlayer")

stl_blk_df_join <- stl_blk_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

stl_blk_picks <- stl_blk_df_new %>% left_join(stl_blk_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten %>% mutate(stl_blk = stl+blk) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(stl_blk = list(stl_blk)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer,urlPlayerHeadshot, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, stl_blk, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%
  relocate(minutes, .after = Under) %>% relocate(stl_blk, .after = minutes) %>% relocate(GP, .after = stl_blk)  %>% select(-c(slugTeam, variation_regular))


# FG3M

fg3m_df_new <- fg3m_df %>% left_join(dk_fg3m, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(fg3m), GP = n()), by = "idPlayer") %>% 
  left_join(fg3m_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

fg3m_df_join <- fg3m_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

fg3m_picks <- fg3m_df_new %>% left_join(fg3m_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(fg3m = list(fg3m)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer,urlPlayerHeadshot, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, fg3m, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%
  relocate(minutes, .after = Under) %>% relocate(fg3m, .after = minutes)%>% relocate(avg, .after = GP) %>% 
  select(-c(slugTeam, variation_regular))


# Stl


stl_df_new <- stl_df %>% left_join(dk_stl, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(stl), GP = n()), by = "idPlayer") %>% 
  left_join(stl_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

stl_df_join <- stl_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

stl_picks <- stl_df_new %>% left_join(stl_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(stl = list(stl)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer,urlPlayerHeadshot, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, stl, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%
  relocate(minutes, .after = Under) %>% relocate(stl, .after = minutes) %>% relocate(GP, .after = stl)%>% select(-c(slugTeam, variation_regular))



#Blk


blk_df_new <- blk_df %>% left_join(dk_blk, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(blk), GP = n()), by = "idPlayer") %>% 
  left_join(blk_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

blk_df_join <- blk_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

blk_picks <- blk_df_new %>% left_join(blk_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(blk = list(blk)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, urlPlayerHeadshot, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, blk, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>% 
  relocate(minutes, .after = Under) %>% relocate(blk, .after = minutes) %>% relocate(GP, .after = blk) %>% select(-c(slugTeam, variation_regular))


#TOV

tov_df_new <- tov_df %>% left_join(dk_tov, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(tov), GP = n()), by = "idPlayer") %>% 
  left_join(tov_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

tov_df_join <- tov_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

tov_picks <- tov_df_new %>% left_join(tov_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(tov = list(tov)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, urlPlayerHeadshot, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, tov, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam)  %>% 
  relocate(minutes, .after = Under) %>% relocate(tov, .after = minutes) %>% relocate(GP, .after = tov)  %>% select(-c(slugTeam, variation_regular))


#Pts

pts_df_new <- pts_df %>% left_join(dk_pts, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(pts), GP = n()), by = "idPlayer") %>% 
  left_join(pts_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

pts_df_join <- pts_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

pts_picks <- pts_df_new %>% left_join(pts_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(pts = list(pts)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, urlPlayerHeadshot, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, pts, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam)  %>% 
  relocate(minutes, .after = Under) %>% relocate(pts, .after = minutes) %>% relocate(GP, .after = pts)  %>% select(-c(slugTeam, variation_regular))


# Ast

ast_df_new <- ast_df %>% left_join(dk_ast, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(ast), GP = n()), by = "idPlayer") %>% 
  left_join(ast_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

ast_df_join <- ast_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

ast_picks <- ast_df_new %>% left_join(ast_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(ast = list(ast)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, urlPlayerHeadshot, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, ast, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam)  %>% 
  relocate(minutes, .after = Under) %>% relocate(ast, .after = minutes) %>% relocate(GP, .after = ast)  %>% select(-c(slugTeam, variation_regular))

#Reb

treb_df_new <- treb_df %>% left_join(dk_reb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% group_by(idPlayer) %>% 
              summarize(avg = mean(treb), GP = n()), by = "idPlayer") %>% 
  left_join(treb_df %>% filter(Type == "Regular Season") %>% group_by(idPlayer) %>% summarize(variation_regular = mean(sd)), by = "idPlayer")

treb_df_join <- treb_df_new  %>% mutate(Ident = ifelse(season_hit < .30 & Type == "Regular Season", 1,0)) %>% 
  group_by(namePlayer,idPlayer) %>% summarize(Ident = mean(Ident))

treb_picks <- treb_df_new %>% left_join(treb_df_join, by = c("namePlayer","idPlayer")) %>%
  left_join(min_ten %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(treb = list(treb)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  group_by(namePlayer,idPlayer, urlPlayerHeadshot, OU, Under, Type, avg = round(avg,1),
           variation_regular = round(variation_regular,1), minutes, treb, GP) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1 - season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(all_rosters %>% select(idPlayer,slugTeam), by = "idPlayer") %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam") %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam)  %>% 
  relocate(minutes, .after = Under) %>% relocate(treb, .after = minutes) %>% relocate(GP, .after = treb)  %>% select(-c(slugTeam, variation_regular))



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
  mutate(namePlayer = paste0(substr(namePlayer,1,1),".",gsub("^\\S+ ", "",namePlayer))) %>% select(-c(idPlayer, urlPlayerHeadshot))






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
  mutate(namePlayer = paste0(substr(namePlayer,1,1),".",gsub("^\\S+ ", "",namePlayer))) %>% arrange(desc(`Regular Season`)) %>% head(10) %>% select(-c(idPlayer, urlPlayerHeadshot))


rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Cheat_sheet_sample.Rmd',
                  output_file = "prop_bet_cheat_sheet_sample.html",
                  output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Matrix'))

