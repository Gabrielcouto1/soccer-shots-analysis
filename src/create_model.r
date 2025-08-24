source("./src/utils/check_libraries.r")
source("./src/utils/pre_process_data.r")

check_libraries()

data <- read_csv("./datasets/shots.csv",show_col_types = FALSE)

data <- pre_process_data(data)

predicting_vars <- data$dataframe %>%
        select(
            is_goal, 
            dist_to_goal, 
            shot_angle, 
            is_penalty,
            is_open_goal,
            is_first_time,
            opponents_in_frame,
            opponents_in_penalty_area, 
            opponents_in_goal_area,
            opponents_in_shot_path,
)

recipe <- recipe(is_goal ~ ., data = predicting_vars) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors())

model_spec <- boost_tree(mode = "classification") %>%
    set_engine("xgboost")

workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(model_spec)

model <- workflow %>%
    fit(data = predicting_vars)

saveRDS(model, file = "./model/xgboost_model.rds")