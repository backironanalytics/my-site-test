




library(tidyverse)
library(nbastatR)
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
library(dplyr)
library(flexdashboard)
library(shinydashboard)
library(ggbreak)
library(plotly)
library(bslib)
library(rsconnect)





Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

gamedata <- game_logs(seasons = 2024, result_types = "team", season_types = c("Regular Season","Playoffs"))
gamedata_current <- game_logs(seasons = 2025, result_types = "team", season_types = c("Regular Season"))

current_season <- "2024-25"
last_season <- "2023-24"

gamedata <- bind_rows(gamedata,gamedata_current)


playerdata <- game_logs(seasons = 2024, result_types = "player", season_types = c("Regular Season","Playoffs"))
playerdata_current <- game_logs(seasons = 2025, result_types = "player", season_types = c("Regular Season"))

playerdata <- bind_rows(playerdata,playerdata_current)

season <- playerdata %>% pull(slugSeason)

player_test <- c("LeBron James","Stephen Curry")

player_id <- function(x){
  
  playerdata %>% filter(str_detect(namePlayer,x)) %>% 
    select(dateGame, idGame, slugOpponent, idPlayer, namePlayer, fgm,fga,pts,treb,ast,stl,blk,fg3m,fg3a,tov,
           plusminus,minutes,locationGame, countDaysRestPlayer, isB2B, isB2BFirst, isB2BSecond) %>% group_by(idPlayer) %>% summarize(n = n()) %>% select(idPlayer)
  
}

player_id <- lapply(player_test,player_id)

player_id <- bind_rows(player_id) %>% pull(idPlayer)


gameids<- playerdata %>% filter(idPlayer %in% player_id)%>% group_by(idGame) %>% summarize(n = n()) %>% pull(idGame)



player_data <- function(x){
  
  playerdata %>% filter(idPlayer == x) %>% 
    select(dateGame, idGame, slugOpponent, idPlayer, namePlayer, fgm,fga,pts,treb,ast,stl,blk,fg3m,fg3a,tov,
           plusminus,minutes,locationGame, countDaysRestPlayer, isB2B, isB2BFirst, isB2BSecond) 
  
  
}

player_data <- lapply(player_id,player_data)

player_data <- bind_rows(player_data)





##Shiny App


ui <- 
  
  dashboardPage(
    dashboardHeader(title = "Dollar Data Science"),
    dashboardSidebar(
      
      sidebarMenu(
        
        selectInput(inputId = "player",
                    label = "Choose Player",
                    choices = player_test,
                    selected = NULL),
        menuItem("Season Stats",tabName = "stats", icon = icon("dashboard")),
        menuItem("Exploratory Analysis", icon = icon("th"), tabName = "analysis", badgeLabel = "new", badgeColor = "green")
      )),
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "stats",
                
                
                fluidPage(
                  formattableOutput("table")),
                
        ),
        tabItem(tabName = "analysis",
                
                
                
                fluidRow(
                  column(width = 12,
                         box(
                           title = "Bin Selector",
                           sliderInput("slider","Select # of Bins:",1,10,1)
                         ),
                         box(
                           plotlyOutput("plot2", height = 500)))
                  
                  
                ),    
                
                fluidPage(
                  column(width = 12,
                         box(
                           plotlyOutput("plot1", height = 500))),
                  
                  
                  
                  
                )            
                
        )
      )
      
    ))

server <- function(input, output) {
  
  
  
  output$table<- renderFormattable({
    
    frame_test %>% filter(namePlayer == input$player)
    
  })
  
  output$plot1<- renderPlotly({
    
    metric = "minutes"
    
    avg <- mean(player_data %>% filter(typeSeason == "Regular Season", slugSeason == current_season) %>% pull(metric))
    avg_lastx <- mean(player_data %>% arrange(desc(dateGame)) %>% head(params$games) %>% pull(metric))
    
    p <- player_data %>% filter(namePlayer == input$player) %>% 
      ggplot(aes(dateGame,.data[[metric]], label = slugOpponent)) +geom_line() + 
      geom_point(aes(color = locationGame, shape = typeSeason)) + geom_smooth(span = 0.6) + labs(x = "Date")  + 
      scale_y_continuous(breaks = seq(0,90,2)) + 
      geom_hline(yintercept = avg) + ggtitle(toupper(paste(input$player,metric,"Per Game")), 
                                             subtitle = toupper(paste("Current Season Avg:",round(avg,1),"/","Avg Last",params$games,"games:",round(avg_lastx,1)))) + 
      theme_minimal() + scale_x_date(breaks = "1 month", date_labels = "%b")  + scale_shape_manual(values = c(15,16)) + 
      facet_grid(~ slugSeason, scales = "free_x")+ guides(color = guide_legend(title = ""),shape = guide_legend(title = ""))
    
    p
    
    
  })
  
  output$plot2<- renderPlotly({
    
    metric = "minutes"
    
    p <- player_data %>% filter(typeSeason == "Regular Season", slugSeason == current_season, namePlayer == input$player) %>% 
      ggplot(aes(.data[[metric]])) + geom_histogram(bins = input$slider)+ 
      scale_x_continuous(breaks = seq(0,90,2))+ ggtitle(toupper(paste(params$player,"Regular Season",metric,"histogram"))) + 
      theme_minimal()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    p
    
    
  })
  
}

shinyApp(ui, server)






