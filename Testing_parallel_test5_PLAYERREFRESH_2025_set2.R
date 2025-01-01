library(foreach)

foreach(i = all_players_previous_batch$namePlayer[((length(all_players_previous_batch$namePlayer)/2)+1):(length(all_players_previous_batch$namePlayer))], 
        j = all_players_previous_batch$idPlayer[((length(all_players_previous_batch$idPlayer)/2)+1):(length(all_players_previous_batch$idPlayer))]) %do% {
  
  rmarkdown::render(input = 'C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/ML_Parlay_TBRv15_2025.Rmd',
                    output_file = paste0(j,substr(j,start = 1,stop=3),".html"),
                    output_dir = file.path('C:/Users/CECRAIG/Desktop/Backironanalytics/my-site-test/sheets'),
                    params = list(id = j))
}
