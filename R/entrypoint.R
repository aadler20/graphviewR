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
host <- "127.0.0.1" # host
options(plumber.port = 6090) # port
gr <- pr("R/plumber.R") %>%
  pr_static("/", "../graphviewr/dist")
gr$run()