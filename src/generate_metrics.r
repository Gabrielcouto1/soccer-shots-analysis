source("./src/utils/check_libraries.r")
source("./src/utils/pre_process_data.r")
source("./src/utils/get_metrics.r")

check_libraries()

data <- read_csv("./datasets/shots.csv",show_col_types = FALSE)

data <- pre_process_data(data)

predicting_vars1 <- data$dataframe %>%
        select(
            is_goal, 
            # x_location, 
            # y_location, 
            # x_end_location,
            # y_end_location,
            # shot_dist,
            dist_to_goal,
            shot_angle,
            # is_ca,
            # is_from_cross,
            # is_under_pressure,
            is_penalty,
            is_open_goal,
            is_first_time,
            # is_one_on_one,
            # is_aerial_win,
            # teammates_in_frame,
            opponents_in_frame,
            # closest_opponent_dist,
            # goalkeeper_dist,
            # opponents_in_penalty_area,
            # opponents_in_goal_area,
            opponents_in_shot_path,
            statsbomb_xg,
            # period,
            # seconds_since_previous_shot,  # Removing the comment of this line will result in omitting the first shot of each match (-2.441 shots).
            # timestamp
)

predicting_vars2 <- data$dataframe %>%
        select(
            is_goal, 
            # x_location, 
            # y_location, 
            # x_end_location, 
            # y_end_location, 
            # shot_dist,
            dist_to_goal, 
            shot_angle, 
            # is_ca,
            # is_from_cross,
            # is_under_pressure,
            is_penalty,
            is_open_goal,
            is_first_time,
            # is_one_on_one,
            # is_aerial_win,
            # teammates_in_frame,
            opponents_in_frame,
            # closest_opponent_dist,
            # goalkeeper_dist,
            opponents_in_penalty_area, #this 
            opponents_in_goal_area, #this 
            opponents_in_shot_path,
            statsbomb_xg,
            # period,
            # seconds_since_previous_shot,  
            # timestamp
)

# generate_metrics(predicting_vars1, "experiments1")
generate_metrics(predicting_vars2, "experiments2")
