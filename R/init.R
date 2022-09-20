source("R/env.R") # set global values
source("R/filters.R")
source("R/utils.R") # utils
gr <- pr("R/plumber.R") %>%
  pr_static("/", "../files/static2") %>%
  pr_filter("api_filters", api_filters,
    serializer = serializer_unboxed_json())
gr$registerHooks(# logging ref. https://github.com/sol-eng/plumber-logging
  list(
    preroute = function() {
        tictoc::tic()
    },
    postroute = function(req, res) {
        end <- tictoc::toc(quiet = TRUE)
        end_time <- as.character(Sys.time())
        log_line <- c(null2char(req$HTTP_ACCESS_TOKEN), req$REMOTE_ADDR,
          req$PATH_INFO, res$status, end_time,
          round(end$toc - end$tic, digits = getOption("digits", 5)), "\n")
        append <- data.table::fifelse(file.exists(log_file), TRUE, FALSE)
        cat(log_line, file = log_file, sep = ",", append = append)
    }
  )
)
gr$run()
