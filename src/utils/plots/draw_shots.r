draw_shots(empty_field, shots_data){
    shots_data_processed <- shots_data %>%
    mutate(
      plot_x_location = ifelse(attacking_team == away_team_name, 120 - x_location, x_location),
      plot_y_location = ifelse(attacking_team == away_team_name, 80 - y_location, y_location), # 80 é a altura do campo
      plot_x_end_location = ifelse(attacking_team == away_team_name, 120 - x_end_location, x_end_location),
      plot_y_end_location = ifelse(attacking_team == away_team_name, 80 - y_end_location, y_end_location), # 80 é a altura do campo
      
      # Inverter Y para plotagem no ggplot (y=0 é o inferior)
      plot_y_location = height - plot_y_location,
      plot_y_end_location = height - plot_y_end_location,
      
      color_source = case_when(
        is_goal == 1 ~ "Goal",
        TRUE ~ "No Goal"
      ),
      color_segment = "Shot Trajectory",
      point_size = ifelse(is_goal == 1, statsbomb_xg * 7, 1.5) # Reutilizando a lógica de tamanho do ponto
    )
}