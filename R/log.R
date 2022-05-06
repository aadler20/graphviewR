log_columns <- c("address", "path", "status", "res_date", "proc_time")
users <- get("users") # get user info from env settings
log_file <- get("log_file")
list_log <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  log_period <- post_body$"logPeriod"
  token <- req$HTTP_ACCESS_TOKEN
  log_list <- data.frame()
  if (file.exists(log_file)) {
    log_list <- data.table::fread(log_file)
    names(log_list) <- c("token", "address",
        "path", "status", "res_date", "proc_time", "X")
    log_list <- log_list[log_list$token == token, log_columns, with = F]
    log_list <- select_log(log_list, log_period)
  }
  res$status <- 200 # success
  list(data = log_list)
}
hours_log <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  log_period <- post_body$"logPeriod"
  token <- req$HTTP_ACCESS_TOKEN
  log_list <- data.frame()
  log_hours <- data.frame()
  if (file.exists(log_file)) {
    log_list <- data.table::fread(log_file, select = c(1, 5)) # token res_date
    names(log_list) <- c("token", "res_date")
    log_list <- log_list[log_list$token == token, ]
    log_list <- select_log(log_list, log_period)
    log_hours <- data.frame(table(format(log_list$res_date, "%H")))
    names(log_hours) <- c("item", "percent")
    log_hours$percent <- round(log_hours$percent / sum(log_hours$percent), 4)
  }
  res$status <- 200 # success
  list(data = log_hours)
}
trend_log <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  category <- post_body$"category"
  time_unit <- post_body$"timeUnit"
  start_date <- post_body$"startDate"
  end_date <- post_body$"endDate"
  token <- req$HTTP_ACCESS_TOKEN
  log_list <- data.frame()
  log_trend <- data.frame()
  api_calls <- 0
  if (file.exists(log_file)) {
    log_list <- data.table::fread(log_file, select = c(1, 5, 6))
    api_calls <- nrow(log_list)
    names(log_list) <- c("token", "res_date", "proc_time")
    log_list <- log_list[log_list$token == token, ]
    log_list <- select_log(log_list, log_period = NULL, start_date, end_date)
    if (nrow(log_list) > 0) {
      time_unit <- switch(time_unit, "hour" = "%Y-%m-%d %H",
      "day" = "%Y-%m-%d", "week" = "%Y-%W")
      if (category == "Frequency (time)") {
        log_trend <- data.frame(table(format(log_list$res_date, time_unit)))
        names(log_trend) <- c("x", "y")
      } else {
        log_pt <- sapply(split(log_list$proc_time,
          format(log_list$res_date, time_unit)), sum)
        log_trend <- data.frame(x = names(log_pt),
          y = as.numeric(log_pt))
      }
    }
  }
  res$status <- 200 # success
  list(data = log_trend, apiCalls = api_calls)
}
select_log <- function(
  log_list, log_period = NULL, start_date = NULL, end_date = NULL) {
  if (!is.null(log_period)) {# priority: log_period, dates
    log_list <- switch(log_period,
      "day" = log_list[log_list$res_date >= Sys.time() - 24 * 3600, ],
      "week" = log_list[log_list$res_date >= Sys.time() - 7 * 24 * 3600, ],
      "month" = log_list[log_list$res_date >= Sys.time() - 30 * 24 * 3600, ]
    )
  } else if (!is.null(start_date) & !is.null(end_date)) {
    log_list <- log_list[log_list$res_date > start_date &
      log_list$res_date < as.POSIXct(end_date) + 24 * 60 * 60, ]
  }
  return(log_list)
}