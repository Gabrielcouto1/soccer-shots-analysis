source("./src/utils/check_libraries.r")

check_libraries()

model <- readRDS("./model/xgboost_model.rds")

shot <- tibble( # Matheus Pereira vs Internacional Goal (23/08/2025)
    dist_to_goal=19, 
    shot_angle=16, 
    is_penalty=0,
    is_open_goal=0,
    is_first_time=0,
    opponents_in_frame=2,
    opponents_in_penalty_area=7,
    opponents_in_goal_area=1,
    opponents_in_shot_path=4
)

prediction <- predict(model, new_data = shot, type = "prob")

shot_xg <- prediction$.pred_1

print(paste0("The chance of this shot converting to a goal is: ", round(shot_xg * 100, 2), "%, xG = ", round(shot_xg, 2)))

