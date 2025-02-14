library(ggdark)
library(ggrepel)
library(ggimage)
library(scales)
library(lubridate)
library(parallel)
library(ggforce)
library(ggplot2)
library(cowplot)
library(magick)
library(ggtext)


df <- bind_rows(ptrebast_picks %>% rename(amount = pts_reb_ast) %>% mutate(Bet = "Total Pts, Reb, Ast"),
          pt_reb_picks %>% rename(amount = pts_reb) %>% mutate(Bet = "Total Pts, Reb"),ast_reb_picks %>% 
            rename(amount = ast_reb)%>% mutate(Bet = "Total Ast, Reb"), stl_blk_picks %>% 
            rename(amount = stl_blk) %>% mutate(Bet = "Total Stl, Blk"), fg3m_picks %>% 
            rename(amount = fg3m) %>% mutate(Bet = "Total Threes Made"), stl_picks %>% 
            rename(amount = stl) %>% mutate(Bet = "Total Steals"), blk_picks %>% 
            rename(amount = blk) %>% mutate(Bet = "Total Blocks"), tov_picks %>% 
            rename(amount = tov) %>% mutate(Bet = "Total Turnovers"), pts_picks %>% 
            rename(amount = pts) %>% mutate(Bet = "Total Points"), treb_picks %>% 
            rename(amount = treb) %>% mutate(Bet = "Total Rebounds"), ast_picks %>% 
            rename(amount = ast) %>% mutate(Bet = "Total Assists")) %>% relocate(Bet, .before = namePlayer) 


player <- "Anthony Davis"

visual <- c("Away Games","Home Games","Last 10","Last 5","Regular Season")

  
output <- lapply(visual, function(x){
  
  df2 <- df %>% filter(namePlayer == player) %>% mutate(Bet = paste(Bet,"O/U",OU)) %>% select(!c(urlThumbnailTeam,Under,minutes,amount,GP,idPlayer)) %>% 
    pivot_longer(!c(Bet,namePlayer,urlPlayerHeadshot,OU,avg,matchup), names_to = "type", values_to = "success") %>% filter(type == x)
  
  p <- df2 %>% ggplot(aes(x = success, y = Bet), label = success) + 
    geom_link(aes(x = 0,xend = success, y = Bet, yend = Bet, alpha = (stat(index))), size = 3.2, color = "lightblue2") + 
    dark_theme_minimal() + labs(x = element_blank(), y = element_blank(), title = paste0(df2$namePlayer[1],"\n",x, " Under Success Rates"),
                                subtitle = paste0("Next Game: ",df2$matchup[1])) + 
    theme(axis.text.x = element_blank(),
          legend.position = "none",
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.text.y = element_text(face = "bold", color = "white"),
          plot.title = element_text(face = "bold", size = 12, color = "gray"),
          plot.subtitle = element_text(face = "bold", size = 12, color = "gray", hjust = 0.9, vjust = 7.5),
          plot.background = element_rect(color ="black"))+ 
    geom_text(aes(x = success, y = Bet, label = percent(success,1)), color = "white", size = 4, hjust = -0.8, fontface = "bold") + xlim(0,1.8) 
  
  
  p_final <- ggdraw(p) + 
    theme(plot.background = element_rect(fill="#000000", color = NA),
          axis.line.x.top = element_line(size =2, color = "gray"),
          plot.subtitle = element_text(face = "bold", size = 12, color = "gray", hjust = 0.7, vjust = 8))+ 
    draw_image(image_read(df2$urlPlayerHeadshot[1]), scale = .3, x=.35, y = -0.32) 
  
  ggsave(plot = p_final, 
         path = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Visuals',
         filename = paste0("visual",x,player,".png"), width = 1080, height = 1080, units = "px", dpi = 150)
  
  
  
})

visual <- c("Away Games","Home Games","Last 10","Last 5","Regular Season")
bets <- df %>% group_by(Bet) %>% summarize(n = n()) %>% pull(Bet)


output_outer <- lapply(bets, function(y){
  
  output <- lapply(visual, function(x){
    
    df3 <- df %>% filter(Bet == y) %>% 
      mutate(label = paste0("<img src='",urlPlayerHeadshot,"'width='25'/>")) %>% mutate(ylabel = paste(namePlayer,"\n",Bet,"O/U",OU)) %>% 
      select(!c(urlThumbnailTeam,Under,minutes,amount,GP,idPlayer,label)) %>% 
      pivot_longer(!c(Bet,namePlayer,urlPlayerHeadshot,OU,avg,matchup,ylabel), names_to = "type", values_to = "success") %>% filter(type == x) %>% arrange(desc(success)) %>% head(10)
    
    p <- df3 %>% ggplot(aes(x = success, y = reorder(ylabel,success)), label = success) + 
      geom_link(aes(x = 0,xend = success, y = reorder(ylabel,success), yend = reorder(ylabel,success), color = success, alpha = (stat(index))), size = 4) + 
      dark_theme_minimal()  + 
      theme(axis.text.x = element_blank(),
            legend.position = "none",
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.text.y = element_text(face = "bold", color = "white"),
            plot.title = element_text(face = "bold", size = 15, color = "gray"),
            plot.subtitle = element_text(face = "bold", size = 12, color = "gray", hjust = 0.9, vjust = 7.5),
            plot.background = element_rect(color ="black"),
            axis.title.y = element_blank(),
            axis.title.x = element_blank()) + 
      geom_text(aes(x = success, y = reorder(ylabel,success), label = percent(success,1)), color = "white", size = 4, hjust = -0.8, fontface = "bold") + xlim(0,1.8) + 
      geom_image(aes(image = urlPlayerHeadshot, x = 0), size = 0.1) + 
      labs(x = element_blank(), y = element_blank(), title = paste0("Prop Bet: ",df3$Bet[1],"\n",x," Under Success Rates")) + 
      scale_color_gradient2(low = "yellow",high = "green", mid = "orange",midpoint = 0.8)
    
    p_final <- ggdraw(p) + 
      theme(plot.background = element_rect(fill="#000000", color = NA),
            axis.line.x.top = element_line(size =2, color = "gray"),
            plot.subtitle = element_text(face = "bold", size = 12, color = "gray", hjust = 0.7, vjust = 8))
    
    ggsave(plot = p_final, 
           path = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Visuals',
           filename = paste0("visual",x,y,"compare",".png"), width = 1080, height = 1080, units = "px", dpi = 150)
    
  })

  
})



df_test <- df %>% 
  mutate(label = paste0("<img src='",urlPlayerHeadshot,"'width='25'/>")) %>% mutate(ylabel = paste(namePlayer,"\n",Bet,"O/U",OU)) %>% 
  left_join(all_rosters %>% group_by(idPlayer,slugTeam) %>% summarize(n = n()), by = "idPlayer") %>% 
  select(!c(urlThumbnailTeam,Under,minutes,amount,GP,idPlayer,label)) %>% 
  pivot_longer(!c(Bet,namePlayer,slugTeam,urlPlayerHeadshot,OU,avg,matchup,ylabel), names_to = "type", values_to = "success") %>% filter(type == "Last 10")


players <- df_test %>% group_by(namePlayer) %>% summarize(n = n()) %>% pull(namePlayer)


p_test <- df_test %>% mutate(y.label = paste0(namePlayer," - ",slugTeam)) %>% 
  filter(namePlayer %in% players[1:15])  %>% ggplot(aes(x=Bet,y=y.label,fill = success, label = paste("O/U:","\n",OU))) + geom_tile(stat = "Identity", color = "gray") + dark_theme_minimal() + 
  scale_fill_viridis_c(option= "turbo", labels = scales::label_percent(), name = "Success Rate") + 
  theme(axis.text.x = element_text(face = "bold", color = "gray", angle = 90, vjust = 0.5),
        legend.position = "right",
        legend.key.size = unit(1,'cm'),
        axis.ticks = element_line(color = "white", linewidth = 2),
        panel.grid = element_blank(),
        axis.text.y = element_text(face = "bold", color = "gray"),
        plot.title = element_text(face = "bold", size = 15, color = "gray"),
        plot.subtitle = element_text(face = "bold", size = 12, color = "gray"),
        plot.background = element_rect(color ="black"),
        axis.title.y = element_text(face = "bold", color = "white"),
        axis.title.x = element_text(face = "bold", color = "white")) + geom_text(color = "white", size = 3, fontface = "bold") + 
  labs(x = "Prop Bet", y = "Player", title = paste0("Under Success Rates"), subtitle = paste(df_test$type[1])) 


ggsave(plot = p_test, 
       path = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/Visuals',
       filename = paste0("visual","heatmap",".png"), width = 1080, height = 1080, units = "px", dpi = 150)
