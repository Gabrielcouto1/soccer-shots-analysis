source("./src/utils/check_libraries.r")

check_libraries()

compare_xg_metrics <- function(experiment_name) {
    final_fit <- readRDS(paste0("./results/", experiment_name, "_final_holdout.rds"))
    test_data <- readRDS(paste0("./results/", experiment_name, "_test_data.rds"))

    predictions <- collect_predictions(final_fit) %>%
        rename(prob_model = .pred_1)

    combined <- predictions %>%
        mutate(y_real = as.numeric(is_goal == "1"))

    eps <- 1e-15
    logloss <- function(p, y) -mean(y * log(p + eps) + (1 - y) * log(1 - p + eps))

    brier_model   <- mean((combined$prob_model - combined$y_real)^2)
    logloss_model <- logloss(combined$prob_model, combined$y_real)

    results <- tibble(
        Métrica = c("Brier Score", "Log-Loss"),
        Modelo  = round(c(brier_model, logloss_model), 4)
    )

    print(paste0("── ", "Experimento 2", " ──"))
    print(results)

    n_bins <- 10

    calibration_data <- combined %>%
        mutate(
            prob = prob_model,
            bin  = cut(prob,
                       breaks         = seq(0, 1, length.out = n_bins + 1),
                       include.lowest = TRUE)
        ) %>%
        group_by(bin) %>%
        summarise(
            mean_predicted = mean(prob),
            mean_actual    = mean(y_real),
            n              = n(),
            .groups        = "drop"
        )

    calibration_plot <- ggplot(calibration_data,
                               aes(x = mean_predicted, y = mean_actual)) +
        geom_abline(linetype = "dashed", color = "gray40") +
        geom_line(linewidth = 1.1, color = "#E07B6A") +
        geom_point(aes(size = n), color = "#E07B6A", alpha = 0.8) +
        scale_size_continuous(range = c(2, 8), name = "Nº de chutes") +
        labs(
            title = "Curva de Calibração — Experimento 2",
            x     = "Probabilidade Prevista",
            y     = "Frequência Real de Gols"
        ) +
        theme_fivethirtyeight() +
        theme(legend.position = "bottom", axis.title = element_text())

    if (!dir.exists("./plots/xg_comparison")) {
        dir.create("./plots/xg_comparison", recursive = TRUE)
    }
    if (!dir.exists("./results/xg_comparison")) {
        dir.create("./results/xg_comparison", recursive = TRUE)
    }

    ggsave(
        paste0("./plots/xg_comparison/", experiment_name, "_calibration.png"),
        plot   = calibration_plot,
        width  = 8,
        height = 6,
        dpi    = 300
    )

    saveRDS(results,
            paste0("./results/xg_comparison/", experiment_name, "_metrics.rds"))

    print(paste0("Salvo em ./plots/xg_comparison/", experiment_name, "_calibration.png"))

    return(results)
}

compare_xg_metrics("experiments2")
