plot_shots_by_body_part <- function(dataset, save=FALSE, path="./plots/shots_by_body_part.png"){
    if(save){
        png(file = path, width = 1600, height = 1200, res = 200)
    }

    body_part_counts <- dataset %>%
        filter(is_goal == 1) %>%
        mutate(body_part = as.character(body_part),
               body_part = case_when(
                 body_part == "Right Foot" ~ "Pé Direito",
                 body_part == "Left Foot"  ~ "Pé Esquerdo",
                 body_part == "Head"       ~ "Cabeça",
                 body_part == "Other"      ~ "Outra",
                 TRUE                      ~ body_part
               )) %>%
        group_by(body_part) %>%
        summarise(count = n()) %>%
        arrange(desc(count))

    plot <- ggplot(body_part_counts, aes(x = reorder(body_part, -count), y = count, fill = body_part)) +
        geom_col(show.legend = FALSE) + 
        labs(title = "Distribuição de Gols por Parte do Corpo",
             subtitle = "Apenas finalizações que resultaram em gol",
             x = "Parte do Corpo",
             y = "Número de Gols") +
        theme_minimal() +
        geom_text(aes(label = count), vjust = -0.5)

    print(plot)

    if(save){
        dev.off()
        print(plot)
        print(paste0("Plot saved to ", path))
    }
}

plot_goals_by_shot_type <- function(dataset, save=FALSE, path="./plots/goals_by_shot_type.png"){
    if(save){
        png(file = path, width = 1600, height = 1200, res = 200)
    }
    
    goals_by_shot_type <- dataset %>%
        filter(is_goal == 1) %>%
        mutate(shot_type = as.character(shot_type),
               shot_type = case_when(
                 shot_type == "Open Play" ~ "Bola Rolando",
                 shot_type == "Penalty"   ~ "Pênalti",
                 shot_type == "Free Kick" ~ "Falta",
                 shot_type == "Corner"    ~ "Escanteio",
                 TRUE                     ~ shot_type
               )) %>%
        group_by(shot_type) %>%
        summarise(goals = n()) %>%
        arrange(desc(goals))

    plot <- ggplot(goals_by_shot_type, aes(x = reorder(shot_type, -goals), y = goals, fill = shot_type)) +
        geom_col(show.legend = FALSE) + 
        geom_text(aes(label = goals), vjust = -0.5, size = 4) + # Adiciona o número sobre a barra
        labs(title = "Gols por Origem do Chute",
             x = "Origem do Chute",
             y = "Número de Gols") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    print(plot)

    if(save){
        dev.off()
        print(plot)
        print(paste0("Plot saved to ", path))
    }
}

plot_goal_prob_by_shot_dist <- function(dataset){

}