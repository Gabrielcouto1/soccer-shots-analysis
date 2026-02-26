plot_metrics <- function(experiment, experiment_name) {
	summarized_metrics <- experiment %>%
		select(algorithm, fit_results) %>%
		distinct(algorithm, .keep_all = TRUE) %>%
		mutate(metrics = map(fit_results, collect_metrics)) %>%
		select(algorithm, metrics) %>%
		unnest(metrics)

	metrics_to_plot <- summarized_metrics %>% 
		filter(.metric %in% c("accuracy", "precision", "recall"))
	
	min_y_value <- min(metrics_to_plot$mean) - 0.05

	metrics_comparison_plot <- ggplot(metrics_to_plot, aes(x = reorder(algorithm, mean), y = mean, fill = algorithm)) +
		geom_col(show.legend = FALSE) +
		geom_text(aes(label = round(mean, 4)), vjust = -0.5, size = 3.5) +
		coord_cartesian(ylim = c(min_y_value, max(metrics_to_plot$mean) * 1.01 )) +
		facet_wrap(~.metric, scales = "free_y") +
		facet_wrap(~.metric, scales = "free_y", labeller = as_labeller(c(
			"accuracy" = "Acurácia",
			"precision" = "Precisão",
			"recall" = "Sensibilidade"
		)))+
		labs(
			title = "Comparação de Performance",
			x = "Algoritmo",
			y = "Valores médios"
		) +
		theme_fivethirtyeight() +
		scale_x_discrete(labels = c(
      		"Random Forest" = "Floresta Aleatória",
      		"Linear Regression" = "Regressão Linear",
      		"Decision Tree" = "Árvore de Decisão",
			"XGBoost" = "XGBoost"
    	))+
		scale_color_fivethirtyeight() +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))
		
	all_predictions <- experiment %>%
		select(algorithm, fit_results) %>%
		distinct(algorithm, .keep_all = TRUE) %>% #
		mutate(predictions = map(fit_results, collect_predictions)) %>%
		select(algorithm, predictions) %>%
		unnest(predictions)

	roc_curves <- all_predictions %>%
		group_by(algorithm) %>%
		roc_curve(truth = is_goal, .pred_1, event_level = "second") #

	auc_values <- all_predictions %>%
		group_by(algorithm) %>%
		roc_auc(truth = is_goal, .pred_1, event_level = "second") %>%
		mutate(label = paste0(algorithm, " (AUC = ", round(.estimate, 3), ")"))

	roc_curves <- roc_curves %>%
		left_join(auc_values, by = "algorithm")

	roc_curves <- roc_curves %>%
  		mutate(label = recode(label,
  		  	"Decision Tree (AUC = 0.583)"    = "Árvore de Decisão (AUC = 0.583)",
  		  	"Logistic Regression (AUC = 0.791)" = "Regressão Logística (AUC = 0.791)",
  		  	"Random Forest (AUC = 0.804)"    = "Floresta Aleatória (AUC = 0.804)",
  		  	"XGBoost (AUC = 0.804)"          = "XGBoost (AUC = 0.804)"
  		))

	roc_plot <- ggplot(roc_curves, aes(x = 1 - specificity, y = sensitivity, color = label)) +
		geom_path(linewidth = 1.2) + 
		geom_abline(linetype = "dashed") + 
		labs(
			title = "Curva ROC",
			x = "Taxa de Falsos Positivos",
			y = "Taxa de Verdadeiros Positivos",
			color = "Algoritmo"
		) +
		coord_equal() + 
		theme_fivethirtyeight() 

	if (!dir.exists("./plots/metrics")) {
		dir.create("./plots/metrics", recursive = TRUE)
	}

	ggsave(
		paste0("plots/metrics/", experiment_name, "_metrics_comparison.png"), 
		plot = metrics_comparison_plot, 
		width = 10, 
		height = 6, 
		dpi = 300   
	)

	ggsave(
		paste0("plots/metrics/", experiment_name, "_roc_curve.png"), 
		plot = roc_plot,
		width = 10,
		height = 8,
		dpi = 300
	)
}

