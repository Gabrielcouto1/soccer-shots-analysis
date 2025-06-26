source("./src/utils/check_libraries.r")
source("./src/utils/pre_process_data.r")
check_libraries()

shots <- read_csv("./datasets/shots.csv",show_col_types = FALSE)

data <- pre_process_data(shots)

shots <- data$dataframe
numeric_vars <- data$numeric_vars

# str(shots)
# str(numeric_vars)

# Verifying how many columns contains NAs in numeric vars subset
# na_counts <- colSums(is.na(numeric_vars))
# na_counts[na_counts > 0]

# Mutating is_goal to be represented by a number to plot the correlation matrix
numeric_vars_for_corr <- numeric_vars %>% 
    mutate(is_goal = as.numeric(as.character(is_goal))) 

cor_matrix <- cor(numeric_vars_for_corr)
png(file = "./plots/correlation.png", width = 3840, height = 2160, res = 300)

corrplot(cor_matrix, method = "color", order = "hclust",
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, 
         number.cex = 0.5, 
         diag = FALSE)
         
dev.off()
