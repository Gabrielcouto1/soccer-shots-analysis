source("./src/utils/check_libraries.r")
source("./src/utils/pre_process_data.r")

check_libraries()

data <- read_csv("./datasets/shots.csv",show_col_types = FALSE)

data <- pre_process_data(data)

folds <- data$dataframe %>%
    vfold_cv(v=10, strata=is_goal)

algorithms <- tibble(algorithm = c("Logistic Regression", "Decision Tree", "Random Forest", "XGBoost"))

experiments <- crossing(folds,algorithms)

experiments <- experiments %>%
  mutate(
    # --- Coluna da Receita (Recipe) ---
    recipe = map(splits, ~ {
      # A receita é criada com base nos dados de TREINO de cada fold
      rec <- recipe(is_goal ~ ., data = training(.x)) %>%
        # Seleciona apenas as variáveis que você definiu como preditoras no pre_process_data
        # Exemplo: step_rm(match_id, shooter_name) # Remova colunas que não são features
        step_dummy(all_nominal_predictors()) %>%
        step_normalize(all_numeric_predictors())
      
      rec
    }),
    
    # --- Coluna do Modelo não Treinado ---
    untrained_model = case_when(
      algorithm == "Logistic Regression" ~ list(logistic_reg(mode = "classification") %>% set_engine("glm")),
      algorithm == "Decision Tree"       ~ list(decision_tree(mode = "classification") %>% set_engine("rpart")),
      algorithm == "Random Forest"       ~ list(rand_forest(mode = "classification") %>% set_engine("ranger")),
      algorithm == "XGBoost"             ~ list(boost_tree(mode = "classification") %>% set_engine("xgboost")),
      TRUE                               ~ list(NA) # Boa prática para caso algum nome esteja errado
    )
  )

# Vamos inspecionar o resultado
print(experiments)

