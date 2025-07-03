source("./src/utils/plots/plot_empty_field.r")

draw_shots <- function(shots_data){

	if((shots_data[1,]$is_home)==1){
		home_team_name <- shots_data[1,]$attacking_team
		away_team_name <- shots_data[1,]$defending_team
	}else{
		home_team_name <- shots_data[1,]$defending_team
		away_team_name <- shots_data[1,]$attacking_team
	}


	green <- "#7FFF00"

    shots_data_processed <- shots_data %>%
		mutate(
			plot_x_location = ifelse(attacking_team == away_team_name, 120 - x_location, x_location),
			plot_y_location = ifelse(attacking_team == away_team_name, 80 - y_location, y_location), # 80 é a altura do campo
			plot_x_end_location = ifelse(attacking_team == away_team_name, 120 - x_end_location, x_end_location),
			plot_y_end_location = ifelse(attacking_team == away_team_name, 80 - y_end_location, y_end_location), # 80 é a altura do campo
			
			# Inverter Y para plotagem no ggplot (y=0 é o inferior)
			plot_y_location = 80 - plot_y_location,
			plot_y_end_location = 80 - plot_y_end_location,
			
			color_source = case_when(
				is_goal == 1 ~ "Goal",
				TRUE ~ "No Goal"
			),
			segment_color = case_when(
        		is_goal == 1 ~ "Goal Trajectory", # Nova categoria para trajetória de gol
        		TRUE ~ "No Goal Trajectory"
      		),
			point_size = ifelse(is_goal == 1, (statsbomb_xg * 7) + 1, 1.5) # Reutilizando a lógica de tamanho do ponto
		)

    p <- plot_empty_field() +
		geom_point(data = shots_data_processed, 
				aes(x = plot_x_location, y = plot_y_location, color = color_source), 
				pch = 19, 
				size = shots_data_processed$point_size) +
		geom_segment(data = shots_data_processed, 
					aes(x = plot_x_location, y = plot_y_location, xend = plot_x_end_location, yend = plot_y_end_location, color = segment_color), 
					linewidth = 0.5,
					arrow = arrow(length = unit(0.2, "cm"))) +
		
		scale_color_manual(values = c("Goal" = green, "No Goal" = "red", "No Goal Trajectory" = "red", "Goal Trajectory"=green)) +
		labs(title = paste0(home_team_name, " vs ", away_team_name, " - Shots"),
			color = "Shot Outcome") +
		theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
			legend.position = "bottom")
  
  return(p)
}