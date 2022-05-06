users <- get("users") # get user info from env settings
tbl_columns <- c("id", "name", "startAt", "endAt",
    "description", "directory", "remark")
list_task <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  status <- post_body$"status"
  start_date <- post_body$"startDate"
  token <- req$HTTP_ACCESS_TOKEN
  userid <- users[users$token == token, "id"]
  task_list <- data.frame()
  if (file.exists("data/tasks.csv")) {
    task_all <- data.table::fread("data/tasks.csv")
    task_all <- task_all[task_all$startAt > start_date, ]
    task_all <- switch(status,
      "todo" = task_all[task_all$startAt > Sys.time(), ],
      "processing" = task_all[task_all$startAt < Sys.time() &
        task_all$endAt > Sys.time(), ],
      "finished" = task_all[task_all$endAt < Sys.time(), ],
      task_all
    )
    task_list <- task_all[task_all$createBy == userid, tbl_columns, with = F]
  }
  res$status <- 200 # success
  list(data = task_list)
}
save_task <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  new_task <- as.data.frame(post_body)
  token <- req$HTTP_ACCESS_TOKEN
  userid <- users[users$token == token, "id"] # user
  new_task <- data.frame(new_task, createBy = userid, createTime = Sys.time(),
    updateBy = userid, updateTime = Sys.time()) # new task
  if_exist <- grep("tasks.csv", dir("data"))
  if (length(if_exist) == 0) {
    task_id <- 1
    append <- FALSE
  } else {
    ids <- data.table::fread("data/tasks.csv", select = "id")
    task_id <- data.table::fifelse(nrow(ids) > 0, max(ids$id) + 1, 1)
    append <- TRUE
  }
  new_task <- cbind(id = task_id, new_task)
  data.table::fwrite(new_task, "data/tasks.csv", append = append)
  res$status <- 200 # success
  list(data = list(taskId = task_id))
}
update_task <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  update_task <- as.data.frame(post_body)
  update_task <- update_task[, match(tbl_columns, names(update_task))]
  task_id <- update_task$id
  token <- req$HTTP_ACCESS_TOKEN
  userid <- users[users$token == token, "id"] # user
  task_all <- data.table::fread("data/tasks.csv")
  update_row <- task_all[task_all$id == task_id, ] # task to update
  create_by <- update_row$createBy
  create_time <- update_row$createTime
  task_all <- data.frame(rbind(task_all[task_all$id != task_id, ],
      cbind(update_task,
      createBy = create_by, createTime = create_time,
      updateBy = userid, updateTime = Sys.time())))
  data.table::fwrite(task_all, "data/tasks.csv")
  res$status <- 200 # success
  list(data = update_task)
}
remove_task <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  task_id <- post_body$"id"
  task_all <- data.table::fread("data/tasks.csv")
  task_all <- task_all[task_all$id != task_id, ]
  data.table::fwrite(task_all, "data/tasks.csv")
  res$status <- 200 # success
  list(data = task_id)
}
