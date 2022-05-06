# packages
packages <- c("data.table", "jsonlite", "plumber", "tictoc")
lapply(packages, function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, repos = "https://cran.rstudio.com",
        dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
# global variables
grenv <- new.env(parent = .GlobalEnv) # set environment for this process
host <- "127.0.0.1" # host
log_dir <- "log/"
log_init <- 0
log_file <- sprintf("%slog_%s.log", log_dir, log_init)
options(plumber.port = 6090) # port
