source("./src/utils/check_libraries.r")
source("./src/utils/pre_process_data.r")
source("./src/utils/plots/plot_empty_field.r")
source("./src/utils/plots/get_shots_by_match_id.r")

check_libraries()

df <- read_csv("./datasets/shots.csv",show_col_types = FALSE)

data <- pre_process_data(df)

shots <- data$dataframe

match_id <- 3890477

shots_from_match <- get_shots_by_match_id(match_id=3890477, dataset=shots)

empty_field <- plot_empty_field()
empty_field

