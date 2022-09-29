# packages
packages <- c("data.table", "jsonlite", "plumber", "tictoc", "dplyr")
lapply(packages, function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, repos = "https://cran.rstudio.com",
        dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
# global variables
host <- "127.0.0.1" # host
log_dir <- "../log/"
demo_dir <- "../demo"
log_init <- 0
log_file <- sprintf("%slog_%s.log", log_dir, log_init)
grenv <- new.env(parent = .GlobalEnv)
options(plumber.port = 6090) # port
