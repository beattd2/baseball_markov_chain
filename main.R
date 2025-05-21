source('transition_functions.R')
source('download_pbp_data.R')
source('simulation_functions.R')

pbp <- download_pbp_data(2018) 
transition_matrix <- get_transition_matrix(pbp)

result <- simulate_game_with_lineups(transition_matrix,
                                     paste0("away_batter_", 1:9), 
                                     paste0("home_batter_", 1:9), 
                                     innings = 9)

# View final scores:
print(result$final_score)

# View box score by inning:
print(result$box_score)

# View play-by-play log:
head(result$play_by_play)

write.csv(result$final_score, 'output_files/final_score', row.names = FALSE)
write.csv(result$box_score, 'output_files/box_score', row.names = FALSE)
write.csv(result$play_by_play, 'output_files/play_by_play', row.names = FALSE)


