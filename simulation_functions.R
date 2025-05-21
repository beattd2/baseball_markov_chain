library(dplyr)
library(tibble)

simulate_half_inning_with_lineup <- function(transition_matrix, lineup, batter_idx, start_state = "000_0", max_plays = 100) {
  current_state <- start_state
  total_runs <- 0
  play_log <- tibble(current_state = character(), next_state = character(), runs_scored = integer(), prob = numeric(), batter = character())
  
  for (i in 1:max_plays) {
    options <- transition_matrix %>% filter(current_state == !!current_state)
    if (nrow(options) == 0) {
      warning("No valid transitions from state: ", current_state)
      break
    }
    
    # Randomly sample next state weighted by probabilities
    selected <- options %>% slice_sample(n = 1, weight_by = prob)
    
    # Add batter info using current batter index
    selected <- selected %>% mutate(batter = lineup[(batter_idx - 1) %% length(lineup) + 1])
    
    # Append to play log
    play_log <- bind_rows(play_log, selected %>% select(current_state, next_state, runs_scored, prob, batter))
    
    # Update runs and state
    total_runs <- total_runs + selected$runs_scored
    current_state <- as.character(selected$next_state[1])
    
    # If 3 outs reached, inning over
    if (grepl("_3$", current_state)) {
      batter_idx <- (batter_idx %% length(lineup)) + 1  # move to next batter for next inning half
      break
    }
    
    # Advance batter index for next play
    batter_idx <- (batter_idx %% length(lineup)) + 1
  }
  
  # Warning if inning ended early without 3 outs
  if (!grepl("_3$", current_state)) {
    warning("Half-inning ended early after max_plays (", max_plays, ") — possible loop.")
  }
  
  return(list(
    total_runs = total_runs,
    play_log = play_log,
    next_batter_idx = batter_idx
  ))
}

simulate_game_with_lineups <- function(transition_matrix, away_lineup, home_lineup, innings = 9) {
  away_batter_idx <- 1
  home_batter_idx <- 1
  
  away_score <- integer(innings)
  home_score <- integer(innings)
  game_log <- tibble()
  
  for (inning in 1:innings) {
    # Away half inning (top)
    away_half <- simulate_half_inning_with_lineup(transition_matrix, away_lineup, away_batter_idx)
    away_batter_idx <- away_half$next_batter_idx
    away_score[inning] <- away_half$total_runs
    game_log <- bind_rows(game_log, mutate(away_half$play_log, inning = inning, half = "top"))
    
    # Home half inning (bottom)
    home_half <- simulate_half_inning_with_lineup(transition_matrix, home_lineup, home_batter_idx)
    home_batter_idx <- home_half$next_batter_idx
    home_score[inning] <- home_half$total_runs
    game_log <- bind_rows(game_log, mutate(home_half$play_log, inning = inning, half = "bottom"))
  }
  
  # Final scores
  final_score <- tibble(team = c("away", "home"), runs = c(sum(away_score), sum(home_score)))
  box_score <- tibble(inning = 1:innings, away = away_score, home = home_score)
  
  return(list(
    final_score = final_score,
    box_score = box_score,
    play_by_play = game_log
  ))
}

adjust_transition_probabilities <- function(transitions, ability_level) {
  transitions %>%
    group_by(current_state) %>%
    mutate(
      # Weight transitions by how valuable they are, scaled by ability_level
      adjusted_weight = expected_transition_value ^ ability_level,
      
      # Normalize weights so they sum to 1 for each current_state
      adjusted_prob = adjusted_weight / sum(adjusted_weight)
    ) %>%
    ungroup() %>%
    select(-adjusted_weight)
}
