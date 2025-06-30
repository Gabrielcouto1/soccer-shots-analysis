pre_process_data <- function(dataframe) {
    # Removing shots taken with body_part == "Other".
    # Removed it because we had too little data regarding these type of shots and the statistics may be misleading.
	# If you still wanna use it, comment lines 5 and 6
    dataframe <- dataframe %>%
      filter(body_part != "Other")
    

    dataframe <- dataframe %>% 
        mutate(
            # shot info
            competition = as.factor(competition),
            season = as.factor(season),
            attacking_team = as.factor(attacking_team),
            defending_team = as.factor(defending_team),

            # shot properties
            shooter_name = as.factor(shooter_name),
            shooter_position = as.factor(shooter_position),
            body_part = as.factor(body_part),
            shot_technique = as.factor(shot_technique),
            shot_type = as.factor(shot_type),
            goalkeeper_position = as.factor(goalkeeper_position),
            outcome = as.factor(outcome),

            # Target variable
            is_goal = as.factor(is_goal)
        )

    numeric_vars <- dataframe %>%
      select(is_goal, x_location, y_location, shot_dist, dist_to_goal, shot_angle,
             is_ca, is_from_cross, is_under_pressure, is_penalty, is_open_goal,
             is_first_time, is_one_on_one, is_aerial_win, teammates_in_frame,
             opponents_in_frame, closest_opponent_dist, goalkeeper_dist,
             opponents_in_penalty_area, opponents_in_goal_area, opponents_in_shot_path,
             statsbomb_xg) %>%
      na.omit() 
    
    return(list(
        dataframe = dataframe,
        numeric_vars = numeric_vars
    ))
}