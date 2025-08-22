source("./src/utils/check_libraries.r")
source("./src/utils/pre_process_data.r")

check_libraries()

data <- read_csv("./datasets/shots.csv",show_col_types = FALSE)

data <- pre_process_data(data)

# creates the 10 folds
folds <- data$predicting_vars %>%
    vfold_cv(v=10, strata=is_goal)

algorithms <- tibble(algorithm = c("Logistic Regression", "Decision Tree", "Random Forest", "XGBoost"))

# creates the tibble that will be used to get the metrics
experiments <- crossing(folds,algorithms)

experiments <- experiments %>%
  mutate(
    recipe = map(splits, ~ {
      rec <- recipe(is_goal ~ ., data = training(.x)) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_normalize(all_numeric_predictors())
      
      rec
    }),
    
    untrained_model = case_when(
      algorithm == "Logistic Regression" ~ list(logistic_reg(mode = "classification") %>% set_engine("glm")),
      algorithm == "Decision Tree"       ~ list(decision_tree(mode = "classification") %>% set_engine("rpart")),
      algorithm == "Random Forest"       ~ list(rand_forest(mode = "classification") %>% set_engine("ranger")),
      algorithm == "XGBoost"             ~ list(boost_tree(mode = "classification") %>% set_engine("xgboost")),
      TRUE                               ~ list(NA) 
    )
  )

# Adds the recipe for each experiment
experiments <- experiments %>%
  mutate(
    pipeline = map2(untrained_model, recipe, ~ workflow() %>% 
                      add_model(.x) %>% 
                      add_recipe(.y))
  )

classification_metrics <- metric_set(accuracy, roc_auc, precision, recall)

# Executes the experiments with each of the 10 folds
experiments <- experiments %>%
  mutate(
    fit_results = map(pipeline, ~ .x %>% 
                        fit_resamples(
                          resamples = folds,
                          metrics = classification_metrics,
                          control = control_resamples(save_pred = TRUE) 
                        ))
  )


# Saves the experimentes tibble
if (!dir.exists("./results")) {
  dir.create("./results")
}

saveRDS(experiments, "./results/experiments.rds")

experiments2 <- readRDS("./results/experiments.rds")