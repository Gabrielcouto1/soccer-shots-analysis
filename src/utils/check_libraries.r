check_libraries <- function() {
    options("width"=200)

    required_packages <- c(
        "dplyr", 
        "tidyverse", 
        "ggplot2", 
        "corrplot",
        "caret"
    )

    for (pkg in required_packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            # message("Installing missing package: ", pkg)
            install.packages(pkg, dependencies = TRUE)
        } else {
            # message(pkg, " is already installed")
            suppressPackageStartupMessages(library(pkg,character.only = TRUE, quietly=TRUE, warn.conflicts=FALSE))
        }
    }
    print("Done loading all necessary packages!")
}