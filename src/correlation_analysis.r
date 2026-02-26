source("./src/utils/check_libraries.r")
source("./src/utils/pre_process_data.r")
check_libraries()

shots <- read_csv("./datasets/shots.csv",show_col_types = FALSE)

data <- pre_process_data(shots)

factor_variables <- data$dataframe
shots <- data$numeric_vars

# Criando a variavel is_goal_factor para ter labels personalizadas e ajudar nos plots

shots <- shots %>%
  mutate(is_goal_factor = factor(is_goal, levels = c(1, 0), labels = c("Gol", "Sem Gol")))

# ------------------------------------------------------------------------------
# Plot 1:  dist_angle_vs_goal.png

plot1 <- ggplot(shots, aes(x = shot_dist, y = shot_angle, color = is_goal_factor)) +
  geom_point(alpha = 0.2, size = 1) +
  scale_color_manual(values = c("Gol" = "#440154FF", "Sem Gol" = "#20fb33ff")) + # Usando cores do viridis
  labs(
    title = "Relação entre distância e ângulo do chute",
    subtitle = "Chutes resultantes em Gol estão em roxo",
    x = "Distância para o gol (em metros)",
    y = "Ângulo do chute (em graus)",
    color = "Resultado do Chute"
  ) +
  theme_wsj(base_size=8) +
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(override.aes = list(alpha = 1)))

ggsave(
  here("plots", "analysis", "dist_angle_vs_goal.png"),
  plot1,
  width = 8,
  height = 6
)

# ------------------------------------------------------------------------------
# Plot 2: dist_distribution_by_goal.png

plot2 <- ggplot(shots, aes(x = is_goal_factor, y = shot_dist, fill = is_goal_factor)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Gol" = "#35B779", "Sem Gol"= "#FDE725")) +
  labs(
    title = "Distribuição da distância dos chutes",
    x = "Resultado do Chute",
    y = "Distância para o gol (em metros)"
  ) +
  theme_economist() +
  coord_flip() + # Virar o gráfico para melhor leitura
  theme(legend.position = "none")

ggsave(
  here("plots", "analysis", "dist_distribution_by_goal.png"),
  plot2,
  width = 8,
  height = 5
)