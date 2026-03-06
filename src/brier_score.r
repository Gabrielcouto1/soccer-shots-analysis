source("./src/utils/check_libraries.r")

check_libraries()


experiment <- readRDS("./results/experiments2.rds")

experiment %>%
  select(algorithm, fit_results) %>%
  distinct(algorithm, .keep_all = TRUE) %>%
  mutate(predictions = map(fit_results, collect_predictions)) %>%
  select(algorithm, predictions) %>%
  unnest(predictions) %>%
  mutate(truth_num = as.numeric(as.character(is_goal))) %>%
  group_by(algorithm) %>%
  summarise(brier_score = mean((.pred_1 - truth_num)^2)) %>%
  arrange(brier_score) %>%
  print()
