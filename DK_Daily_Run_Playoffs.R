

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







##Playoffs

#Pt Reb Ast

ptreb_ast_df_new_all <- bind_rows(ptreb_ast_df %>% filter(typeSeason == "All"),ptreb_ast_df %>% filter(typeSeason == "Regular Season",Type == "Regular Season"),ptreb_ast_df %>% filter(typeSeason == "Playoffs",Type == "Regular Season")) %>% 
  left_join(dk_ptrebast, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% 
  rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = pts+treb+ast) %>% group_by(idPlayer) %>% summarize(season_avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(playerdata %>% filter(typeSeason == "Playoffs", slugSeason == "2024-25") %>% 
              mutate(avg = pts+treb+ast) %>% group_by(idPlayer) %>% summarize(playoff_avg = mean(avg)), by = "idPlayer")


ptrebast_picks_all <- ptreb_ast_df_new_all  %>%
  left_join(min_ten_all %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten_all %>% 
              mutate(pts_reb_ast = pts+treb+ast) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(pts_reb_ast = list(pts_reb_ast)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  mutate(Type = ifelse(typeSeason == "All" & Type == "Regular Season","Regular Season + Playoffs",ifelse(typeSeason == "Regular Season","Regular Season",ifelse(typeSeason == "Playoffs","Playoffs",Type)))) %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, season_avg = round(season_avg,1),playoff_avg = round(playoff_avg,1),
           minutes, pts_reb_ast) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(pts_reb_ast, .after = minutes) %>%
  select(!c(slugTeam,idPlayer))

## Pt Reb


pt_reb_df_new_all <- bind_rows(pt_reb_df %>% filter(typeSeason == "All"),pt_reb_df %>% filter(typeSeason == "Regular Season",Type == "Regular Season"),pt_reb_df %>% filter(typeSeason == "Playoffs",Type == "Regular Season")) %>% 
  left_join(dk_ptreb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% 
  rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = pts+treb) %>% group_by(idPlayer) %>% summarize(season_avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(playerdata %>% filter(typeSeason == "Playoffs", slugSeason == "2024-25") %>% 
              mutate(avg = pts+treb) %>% group_by(idPlayer) %>% summarize(playoff_avg = mean(avg)), by = "idPlayer")


pt_reb_picks_all <- pt_reb_df_new_all  %>%
  left_join(min_ten_all %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten_all %>% 
              mutate(pts_reb = pts+treb) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(pts_reb = list(pts_reb)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  mutate(Type = ifelse(typeSeason == "All" & Type == "Regular Season","Regular Season + Playoffs",ifelse(typeSeason == "Regular Season","Regular Season",ifelse(typeSeason == "Playoffs","Playoffs",Type)))) %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, season_avg = round(season_avg,1),playoff_avg = round(playoff_avg,1),
           minutes, pts_reb) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(pts_reb, .after = minutes) %>%
  select(!c(slugTeam,idPlayer))

## Ast Reb

ast_reb_df_new_all <- bind_rows(ast_reb_df %>% filter(typeSeason == "All"),ast_reb_df %>% filter(typeSeason == "Regular Season",Type == "Regular Season"),ast_reb_df %>% filter(typeSeason == "Playoffs",Type == "Regular Season")) %>% 
  left_join(dk_astreb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% 
  rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = treb+ast) %>% group_by(idPlayer) %>% summarize(season_avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(playerdata %>% filter(typeSeason == "Playoffs", slugSeason == "2024-25") %>% 
              mutate(avg = treb+ast) %>% group_by(idPlayer) %>% summarize(playoff_avg = mean(avg)), by = "idPlayer")


ast_reb_picks_all <- ast_reb_df_new_all  %>%
  left_join(min_ten_all %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten_all %>% 
              mutate(ast_reb = treb+ast) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(ast_reb = list(ast_reb)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  mutate(Type = ifelse(typeSeason == "All" & Type == "Regular Season","Regular Season + Playoffs",ifelse(typeSeason == "Regular Season","Regular Season",ifelse(typeSeason == "Playoffs","Playoffs",Type)))) %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, season_avg = round(season_avg,1),playoff_avg = round(playoff_avg,1),
           minutes, ast_reb) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(ast_reb, .after = minutes) %>%
  select(!c(slugTeam,idPlayer))

## Stl Blk

stl_blk_df_new_all <- bind_rows(stl_blk_df %>% filter(typeSeason == "All"),stl_blk_df %>% filter(typeSeason == "Regular Season",Type == "Regular Season"),stl_blk_df %>% filter(typeSeason == "Playoffs",Type == "Regular Season")) %>% 
  left_join(dk_stlblk, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% 
  rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25") %>% 
              mutate(avg = stl+blk) %>% group_by(idPlayer) %>% summarize(season_avg = mean(avg), GP = n()), by = "idPlayer") %>% 
  left_join(playerdata %>% filter(typeSeason == "Playoffs", slugSeason == "2024-25") %>% 
              mutate(avg = stl+blk) %>% group_by(idPlayer) %>% summarize(playoff_avg = mean(avg)), by = "idPlayer")


stl_blk_picks_all <- stl_blk_df_new_all  %>%
  left_join(min_ten_all %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten_all %>% 
              mutate(stl_blk = stl+blk) %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(stl_blk = list(stl_blk)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  mutate(Type = ifelse(typeSeason == "All" & Type == "Regular Season","Regular Season + Playoffs",ifelse(typeSeason == "Regular Season","Regular Season",ifelse(typeSeason == "Playoffs","Playoffs",Type)))) %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, season_avg = round(season_avg,1),playoff_avg = round(playoff_avg,1),
           minutes, stl_blk) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(stl_blk, .after = minutes) %>%
  select(!c(slugTeam,idPlayer))

##FG3M

fg3m_df_new_all <- bind_rows(fg3m_df %>% filter(typeSeason == "All"),fg3m_df %>% filter(typeSeason == "Regular Season",Type == "Regular Season"),fg3m_df %>% filter(typeSeason == "Playoffs",Type == "Regular Season")) %>% 
  left_join(dk_fg3m, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% 
  rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(season_avg = mean(fg3m), GP = n()), by = "idPlayer") %>% 
  left_join(playerdata %>% filter(typeSeason == "Playoffs", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(playoff_avg = mean(fg3m)), by = "idPlayer")


fg3m_picks_all <- fg3m_df_new_all  %>%
  left_join(min_ten_all %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten_all  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(fg3m = list(fg3m)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  mutate(Type = ifelse(typeSeason == "All" & Type == "Regular Season","Regular Season + Playoffs",ifelse(typeSeason == "Regular Season","Regular Season",ifelse(typeSeason == "Playoffs","Playoffs",Type)))) %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, season_avg = round(season_avg,1),playoff_avg = round(playoff_avg,1),
           minutes, fg3m) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(fg3m, .after = minutes) %>%
  select(!c(slugTeam,idPlayer))


##STL

stl_df_new_all <- bind_rows(stl_df %>% filter(typeSeason == "All"),stl_df %>% filter(typeSeason == "Regular Season",Type == "Regular Season"),stl_df %>% filter(typeSeason == "Playoffs",Type == "Regular Season")) %>% 
  left_join(dk_stl, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% 
  rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(season_avg = mean(stl), GP = n()), by = "idPlayer") %>% 
  left_join(playerdata %>% filter(typeSeason == "Playoffs", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(playoff_avg = mean(stl)), by = "idPlayer")


stl_picks_all <- stl_df_new_all  %>%
  left_join(min_ten_all %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten_all  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(stl = list(stl)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  mutate(Type = ifelse(typeSeason == "All" & Type == "Regular Season","Regular Season + Playoffs",ifelse(typeSeason == "Regular Season","Regular Season",ifelse(typeSeason == "Playoffs","Playoffs",Type)))) %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, season_avg = round(season_avg,1),playoff_avg = round(playoff_avg,1),
           minutes, stl) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(stl, .after = minutes) %>%
  select(!c(slugTeam,idPlayer))


##BLK

blk_df_new_all <- bind_rows(blk_df %>% filter(typeSeason == "All"),blk_df %>% filter(typeSeason == "Regular Season",Type == "Regular Season"),blk_df %>% filter(typeSeason == "Playoffs",Type == "Regular Season")) %>% 
  left_join(dk_blk, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% 
  rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(season_avg = mean(blk), GP = n()), by = "idPlayer") %>% 
  left_join(playerdata %>% filter(typeSeason == "Playoffs", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(playoff_avg = mean(blk)), by = "idPlayer")


blk_picks_all <- blk_df_new_all  %>%
  left_join(min_ten_all %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten_all  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(blk = list(blk)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  mutate(Type = ifelse(typeSeason == "All" & Type == "Regular Season","Regular Season + Playoffs",ifelse(typeSeason == "Regular Season","Regular Season",ifelse(typeSeason == "Playoffs","Playoffs",Type)))) %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, season_avg = round(season_avg,1),playoff_avg = round(playoff_avg,1),
           minutes, blk) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(blk, .after = minutes) %>%
  select(!c(slugTeam,idPlayer))


##TOV

tov_df_new_all <- bind_rows(tov_df %>% filter(typeSeason == "All"),tov_df %>% filter(typeSeason == "Regular Season",Type == "Regular Season"),tov_df %>% filter(typeSeason == "Playoffs",Type == "Regular Season")) %>% 
  left_join(dk_tov, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% 
  rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(season_avg = mean(tov), GP = n()), by = "idPlayer") %>% 
  left_join(playerdata %>% filter(typeSeason == "Playoffs", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(playoff_avg = mean(tov)), by = "idPlayer")


tov_picks_all <- tov_df_new_all  %>%
  left_join(min_ten_all %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten_all  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(tov = list(tov)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  mutate(Type = ifelse(typeSeason == "All" & Type == "Regular Season","Regular Season + Playoffs",ifelse(typeSeason == "Regular Season","Regular Season",ifelse(typeSeason == "Playoffs","Playoffs",Type)))) %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, season_avg = round(season_avg,1),playoff_avg = round(playoff_avg,1),
           minutes, tov) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(tov, .after = minutes) %>%
  select(!c(slugTeam,idPlayer))

##PTS

pts_df_new_all <- bind_rows(pts_df %>% filter(typeSeason == "All"),pts_df %>% filter(typeSeason == "Regular Season",Type == "Regular Season"),pts_df %>% filter(typeSeason == "Playoffs",Type == "Regular Season")) %>% 
  left_join(dk_pts, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% 
  rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(season_avg = mean(pts), GP = n()), by = "idPlayer") %>% 
  left_join(playerdata %>% filter(typeSeason == "Playoffs", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(playoff_avg = mean(pts)), by = "idPlayer")


pts_picks_all <- pts_df_new_all  %>%
  left_join(min_ten_all %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten_all  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(pts = list(pts)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  mutate(Type = ifelse(typeSeason == "All" & Type == "Regular Season","Regular Season + Playoffs",ifelse(typeSeason == "Regular Season","Regular Season",ifelse(typeSeason == "Playoffs","Playoffs",Type)))) %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, season_avg = round(season_avg,1),playoff_avg = round(playoff_avg,1),
           minutes, pts) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(pts, .after = minutes) %>%
  select(!c(slugTeam,idPlayer))

##AST

ast_df_new_all <- bind_rows(ast_df %>% filter(typeSeason == "All"),ast_df %>% filter(typeSeason == "Regular Season",Type == "Regular Season"),ast_df %>% filter(typeSeason == "Playoffs",Type == "Regular Season")) %>% 
  left_join(dk_ast, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% 
  rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(season_avg = mean(ast), GP = n()), by = "idPlayer") %>% 
  left_join(playerdata %>% filter(typeSeason == "Playoffs", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(playoff_avg = mean(ast)), by = "idPlayer")


ast_picks_all <- ast_df_new_all  %>%
  left_join(min_ten_all %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten_all  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(ast = list(ast)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  mutate(Type = ifelse(typeSeason == "All" & Type == "Regular Season","Regular Season + Playoffs",ifelse(typeSeason == "Regular Season","Regular Season",ifelse(typeSeason == "Playoffs","Playoffs",Type)))) %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, season_avg = round(season_avg,1),playoff_avg = round(playoff_avg,1),
           minutes, ast) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(ast, .after = minutes) %>%
  select(!c(slugTeam,idPlayer))

##REB

treb_df_new_all <- bind_rows(treb_df %>% filter(typeSeason == "All"),treb_df %>% filter(typeSeason == "Regular Season",Type == "Regular Season"),treb_df %>% filter(typeSeason == "Playoffs",Type == "Regular Season")) %>% 
  left_join(dk_reb, by = c("namePlayer","OU")) %>% filter(!is.na(Over)) %>% 
  rename(season_hit = test) %>% 
  left_join(playerdata %>% filter(typeSeason == "Regular Season", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(season_avg = mean(treb), GP = n()), by = "idPlayer") %>% 
  left_join(playerdata %>% filter(typeSeason == "Playoffs", slugSeason == "2024-25")  %>% group_by(idPlayer) %>% summarize(playoff_avg = mean(treb)), by = "idPlayer")


treb_picks_all <- treb_df_new_all  %>%
  left_join(min_ten_all %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(minutes = list(minutes)), by = "idPlayer") %>%
  left_join(min_ten_all  %>% arrange(dateGame) %>% group_by(idPlayer) %>% summarize(treb = list(treb)), by = "idPlayer") %>% 
  left_join(playerdata %>%group_by(idPlayer,urlPlayerHeadshot) %>% summarize(n = n()), by = "idPlayer") %>% 
  mutate(Type = ifelse(typeSeason == "All" & Type == "Regular Season","Regular Season + Playoffs",ifelse(typeSeason == "Regular Season","Regular Season",ifelse(typeSeason == "Playoffs","Playoffs",Type)))) %>% 
  group_by(namePlayer, idPlayer, urlPlayerHeadshot, OU,Under, slugTeam,Type, season_avg = round(season_avg,1),playoff_avg = round(playoff_avg,1),
           minutes, treb) %>% 
  summarize(season_hit) %>% ungroup() %>% mutate(season_hit = 1-season_hit) %>%pivot_wider(names_from = Type, values_from = season_hit) %>% 
  left_join(teams %>% select(slugTeam,urlThumbnailTeam), by = "slugTeam") %>% 
  left_join(matchup %>% select(slugTeam,matchup), by = "slugTeam")  %>%
  relocate(urlThumbnailTeam, .after = namePlayer) %>% relocate(matchup, .after = urlThumbnailTeam) %>%  relocate(minutes, .after = Under) %>%
  relocate(treb, .after = minutes) %>%
  select(!c(slugTeam,idPlayer))






combined_all <- bind_rows(ptrebast_picks_all %>% rename(amount = pts_reb_ast) %>% mutate(Bet = "Total Pts, Reb, Ast"),
                      pt_reb_picks_all %>% rename(amount = pts_reb) %>% mutate(Bet = "Total Pts, Reb"),ast_reb_picks_all %>% 
                        rename(amount = ast_reb)%>% mutate(Bet = "Total Ast, Reb"), stl_blk_picks_all %>% 
                        rename(amount = stl_blk) %>% mutate(Bet = "Total Stl, Blk"), fg3m_picks_all %>% 
                        rename(amount = fg3m) %>% mutate(Bet = "Total Threes Made"), stl_picks_all %>% 
                        rename(amount = stl) %>% mutate(Bet = "Total Steals"), blk_picks_all %>% 
                        rename(amount = blk) %>% mutate(Bet = "Total Blocks"), tov_picks_all %>% 
                        rename(amount = tov) %>% mutate(Bet = "Total Turnovers"), pts_picks_all %>% 
                        rename(amount = pts) %>% mutate(Bet = "Total Points"), treb_picks_all %>% 
                        rename(amount = treb) %>% mutate(Bet = "Total Rebounds"), ast_picks_all %>% 
                        rename(amount = ast) %>% mutate(Bet = "Total Assists")) %>% relocate(Bet, .before = namePlayer) %>% select(-c(urlPlayerHeadshot))






Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")


rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Cheat_sheetv2.Rmd',
                  output_file = "prop_bet_cheat_sheet.html",
                  output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Matrix'))





combined_sample_all <- bind_rows(ptrebast_picks_all %>% rename(amount = pts_reb_ast) %>% mutate(Bet = "Total Pts, Reb, Ast"),
                          pt_reb_picks_all %>% rename(amount = pts_reb) %>% mutate(Bet = "Total Pts, Reb"),ast_reb_picks_all %>% 
                            rename(amount = ast_reb)%>% mutate(Bet = "Total Ast, Reb"), stl_blk_picks_all %>% 
                            rename(amount = stl_blk) %>% mutate(Bet = "Total Stl, Blk"), fg3m_picks_all %>% 
                            rename(amount = fg3m) %>% mutate(Bet = "Total Threes Made"), stl_picks_all %>% 
                            rename(amount = stl) %>% mutate(Bet = "Total Steals"), blk_picks_all %>% 
                            rename(amount = blk) %>% mutate(Bet = "Total Blocks"), tov_picks_all %>% 
                            rename(amount = tov) %>% mutate(Bet = "Total Turnovers"), pts_picks_all %>% 
                            rename(amount = pts) %>% mutate(Bet = "Total Points"), treb_picks_all %>% 
                            rename(amount = treb) %>% mutate(Bet = "Total Rebounds"), ast_picks_all %>% 
                            rename(amount = ast) %>% mutate(Bet = "Total Assists")) %>% relocate(Bet, .before = namePlayer)  %>% head(10) %>% select(-c(urlPlayerHeadshot))





rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Cheat_sheet_samplev2.Rmd',
                  output_file = "prop_bet_cheat_sheet_sample.html",
                  output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Matrix'))

