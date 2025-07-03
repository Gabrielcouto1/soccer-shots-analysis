source("./src/utils/check_libraries.r")
source("./src/utils/pre_process_data.r")
source("./src/utils/plots/get_shots_by_match_id.r")
source("./src/utils/plots/draw_shots.r")

check_libraries()

df <- pre_process_data(read_csv("./datasets/shots.csv",show_col_types = FALSE))

shots <- df$dataframe

match_id <- sample(shots$match_id, 1)

shots_from_match <- get_shots_by_match_id(match_id=match_id, dataset=shots)

draw_shots(shots_from_match, save=TRUE)

