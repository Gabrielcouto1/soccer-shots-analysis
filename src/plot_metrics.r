source("./src/utils/check_libraries.r")
source("./src/utils/analyze_metrics.r")

check_libraries()

experiment1 <- readRDS("./results/experiments1.rds")
plot_metrics(experiment1, "experiment1")
rm(experiment1)

experiment2 <- readRDS("./results/experiments2.rds")
plot_metrics(experiment2, "experiment2")
rm(experiment2)

experiment3 <- readRDS("./results/experiments3.rds")
plot_metrics(experiment3, "experiment3")
rm(experiment3)


experiment4 <- readRDS("./results/experiments4.rds")
plot_metrics(experiment4, "experiment4")
rm(experiment4)

experiment5 <- readRDS("./results/experiments5.rds")
plot_metrics(experiment5, "experiment5")
rm(experiment5)
