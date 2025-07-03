source("./src/utils/plots/exploratory_plots.r")
source("./src/utils/check_libraries.r")

check_libraries()

shots <- read_csv("./datasets/shots.csv",show_col_types = FALSE)

plot_shots_by_body_part(dataset=shots, save=FALSE)

plot_goals_by_shot_type(dataset=shots, save=FALSE)
