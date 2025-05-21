library(dplyr)
library(purrr)
library(tidyr)

encode_base_state <- function(base1, base2, base3) {
  paste0(as.integer(!is.na(base1)), as.integer(!is.na(base2)), as.integer(!is.na(base3)))
}

encode_state <- function(base1, base2, base3, outs) {
  paste0(encode_base_state(base1, base2, base3), "_", outs)
}

calculate_run_expectancy <- function(matrix) {
  matrix %>%
    group_by(current_state) %>%
    summarize(run_expectancy = sum(prob * runs_scored), .groups = "drop")
}

add_expectancy_to_transitions <- function(transitions, expectancy_table) {
  transitions %>%
    left_join(expectancy_table, by = c('next_state' = 'current_state')) %>%  # adds run_expectancy from lookup table
    mutate(run_expectancy = replace_na(run_expectancy, 0)) %>%
    # Add new column as sum of actual and expected future runs
    mutate(expected_transition_value = runs_scored + run_expectancy)
}

calculate_runs_scored <- function(current_state, new_state) {
  #DEPRECATED
  # Extract base runner strings
  current_bases <- strsplit(substr(current_state, 1, 3), "")[[1]]
  new_bases <- strsplit(substr(new_state, 1, 3), "")[[1]]
  # Calculate total players on base
  current_baserunners <- sum(as.integer(current_bases))
  new_baserunners <- sum(as.integer(new_bases))
  # Extract outs
  c_outs <- as.integer(substr(current_state, 5, 5))
  n_outs <- as.integer(substr(new_state, 5, 5))
  # Calculate runs scored
  runs_scored <- (current_baserunners + 1) - new_baserunners - (n_outs - c_outs)
  
  # Optional: debug print (uncomment for inspection)
  # message(sprintf("CB: %d, NB: %d, CO: %d, NO: %d, Runs: %d", current_baserunners, new_baserunners, c_outs, n_outs, runs_scored))
  
  return(runs_scored)
}

get_rbi <- function(new_state) {
  rbi <- as.integer(strsplit(substr(new_state, 6, 6), "")[[1]])
}

get_runs_scored <- function(bat_dest_id, run1_dest_id, run2_dest_id, run3_dest_id) {
  runs <- sum(c(as.integer(bat_dest_id) == 4,
                as.integer(run1_dest_id) == 4,
                as.integer(run2_dest_id) == 4,
                as.integer(run3_dest_id) == 4))
  return(runs)
}

get_transition_matrix <- function(data) {
  #TODO: should rbi actually be implemented here?
  data <- data %>%
    mutate(
      runs_scored = pmap_int(list(bat_dest_id, run1_dest_id, run2_dest_id, run3_dest_id), get_runs_scored),
      current_state = encode_state(base1_run_id, base2_run_id, base3_run_id, outs_ct),
      new_outs = outs_ct + event_outs_ct,
      next_state = paste0(encode_state(
        case_when(bat_dest_id == 1 ~ bat_id,
                  run1_dest_id == 1 ~ base1_run_id, 
                  run2_dest_id == 1 ~ base2_run_id,
                  run3_dest_id == 1 ~ base3_run_id,),
        case_when(bat_dest_id == 2 ~ bat_id,
                  run1_dest_id == 2 ~ base1_run_id, 
                  run2_dest_id == 2 ~ base2_run_id,
                  run3_dest_id == 2 ~ base3_run_id,),
        case_when(bat_dest_id == 3 ~ bat_id,
                  run1_dest_id == 3 ~ base1_run_id, 
                  run2_dest_id == 3 ~ base2_run_id,
                  run3_dest_id == 3 ~ base3_run_id,),
        new_outs
      #), rbi_ct)
      ))
    ) %>%
    filter(new_outs <= 3)
  
  data %>%
    count(current_state, next_state, runs_scored) %>%
    group_by(current_state) %>%
    mutate(prob = n / sum(n)) %>%
    #mutate(runs_scored = pmap_dbl(list(current_state, next_state), calculate_runs_scored)) %>%
    #mutate(rbi = map(next_state, get_rbi)) %>%
    #select(current_state, next_state, prob, runs_scored, rbi)
    select(current_state, next_state, prob, runs_scored)
}