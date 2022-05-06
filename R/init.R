source("R/env.R") # set global values
source("R/utils.R") # utils
source("R/filters.R") # add filters
gr <- pr() # Plumber instance
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
gr$setSerializer(serializer_unboxed_json())
lapply(names(specific_filters), function(x) gr$filter(x, specific_filters[[x]]))
gr$run() # run server
