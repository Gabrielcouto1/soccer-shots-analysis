
generate_metrics <- function(predicting_vars, experiment_name) {
	set.seed(42)
	split <- initial_split(predicting_vars, prop = 0.80, strata = is_goal)
	train_data <- training(split)
	test_data <- testing(split)

    # creates the 10 folds
	folds <- group_vfold_cv(
		train_data,
		group = match_id,
		v = 10
	)

	algorithms <- tibble(algorithm = c("Regressão Logística", "Árvore de Decisão", "Floresta Aleatória", "XGBoost"))

	# creates the tibble that will be used to get the metrics
	experiments <- crossing(folds,algorithms)

	experiments <- experiments %>%
	mutate(
		recipe = map(splits, ~ {
		rec <- recipe(is_goal ~ ., data = training(.x)) %>%
			step_rm(match_id) %>%
			step_dummy(all_nominal_predictors()) %>%
			step_normalize(all_numeric_predictors())
		
		rec
		}),
		
		untrained_model = case_when(
			algorithm == "Regressão Logística" ~ list(
				logistic_reg(mode = "classification") %>%
					set_engine("glm")
			),
			algorithm == "Árvore de Decisão" ~ list(
				decision_tree(mode = "classification") %>%
					set_engine("rpart")
			),
			algorithm == "Floresta Aleatória" ~ list(
				rand_forest(mode = "classification") %>%
					set_engine("ranger")
			),
			algorithm == "XGBoost" ~ list(
				boost_tree(
					mode           = "classification",
					trees          = 15,
					tree_depth     = 6,
					learn_rate     = 0.3,
					min_n          = 1,
					loss_reduction = 0,
					sample_size    = 1.0,
					mtry           = 1.0
				) %>%
					set_engine("xgboost")
			),
			TRUE ~ list(NA)
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

	best_pipeline <- experiments %>%
        filter(algorithm == "XGBoost") %>%
        slice(1) %>%
        pull(pipeline) %>%
        .[[1]]

    final_fit     <- best_pipeline %>% last_fit(split, metrics = classification_metrics)
    final_metrics <- collect_metrics(final_fit)

    print(paste0("Métricas finais no holdout — ", experiment_name))
    print(final_metrics)

	# Saves the experimentes tibble
	if (!dir.exists("./results")) {
	dir.create("./results")
	}

	saveRDS(experiments,   paste0("./results/", experiment_name, ".rds"))
    saveRDS(final_fit,     paste0("./results/", experiment_name, "_final_holdout.rds"))
    saveRDS(final_metrics, paste0("./results/", experiment_name, "_holdout_metrics.rds"))

	print(paste0("Experiment saved to ", "./results/", experiment_name, ".rds"))
}