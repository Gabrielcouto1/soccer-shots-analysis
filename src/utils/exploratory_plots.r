source("./src/utils/check_libraries.r")
check_libraries()

plot_shots_by_body_part <- function(dataset, save=FALSE, path="./plots/shots_by_body_part.png"){
    if(save){
        png(file = path, width = 1600, height = 1200, res = 200)
    }

    body_part_counts <- dataset %>%
        filter(is_goal == 1) %>%
        group_by(body_part) %>%
        summarise(count = n()) %>%
        arrange(desc(count))

    plot <- ggplot(body_part_counts, aes(x = reorder(body_part, -count), y = count, fill = body_part)) +
    geom_bar(stat = "identity") +
    labs(title = "Distribution of Shots by Body Part",
        x = "Body Part",
        y = "Number of Shots") +
    theme_minimal()

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
    group_by(shot_type) %>%
    summarise(goals = n()) %>%
    arrange(desc(goals))

    # Create the bar plot
    plot <- ggplot(goals_by_shot_type, aes(x = reorder(shot_type, -goals), y = goals, fill = shot_type)) +
    geom_bar(stat = "identity") +
    labs(title = "Goals by Shot Type",
        x = "Shot Type",
        y = "Number of Goals") +
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