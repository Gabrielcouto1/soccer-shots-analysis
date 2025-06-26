source("./src/utils/check_libraries.r")
source("./src/utils/pre_process_data.r")
check_libraries()

shots <- read_csv("./datasets/shots.csv",show_col_types = FALSE)
str(shots) 

data <- pre_process_data(shots)

shots <- data$dataframe
numeric_vars <- data$numeric_vars

str(shots)
str(numeric_vars)


numeric_vars_for_corr <- numeric_vars_for_corr %>% 
    mutate(is_goal = as.numeric(as.character(is_goal))) 

cor_matrix <- cor(numeric_vars_for_corr)

png(file = "./plots/correlation.png", width = 1920, height = 1080, res = 150)

corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, 
         number.cex = 0.5, 
         diag = FALSE)
dev.off()
