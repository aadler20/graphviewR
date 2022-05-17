source("R/env_prod.R")
source("R/task.R")
source("R/log.R")
source("R/getPackageGraph.R") # overview of installed packages
source("R/getPackageInfo.R") # info of selected package
source("R/getFunctionInfo.R") # info of selected function

auth_user <- function(req, res) {
  if (is.null(req$user)) {
    # User isn"t logged in
    res$status <- 401 # Unauthorized
    list(error = "You must login to access this resource.")
  } else {
    # user is logged in. Move on...
    plumber::forward()
  }
}
show_args <- function(req) {
  req_attrs <- ls(req)
  # print arguments
  lapply(req_attrs, function(x) {
    if (x != "pr") print(paste(x, mget(x, req), sep = ":"))
  })
  # continue on
  plumber::forward()
}
# eval Code
unjson_classes <- c("lm", "qr", "summary.lm", "try-error")
eval_code <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  code <- post_body[["code"]]
  code <- gsub("\r", "", code)
  print(code)
  result <- try(lapply(parse(text = code), eval), TRUE)
  result <- lapply(result, function(x) {
    if (class(x) %in% prim_classes) {
      return(data.frame(Class = class(x),
        Value = paste(as.character(x), collapse = ",")))
    } else return(data.frame(Class = names(x),
      Value = as.character(x)))
  })
  list(data = result)
}
# context-specific filters
get_function_help <- get("get_function_help")
function_help <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  pkg <- post_body[["packageName"]]
  func <- post_body[["functionName"]]
  help_data <- get_function_help(pkg = pkg, func = func)
  list(helpData = help_data)
}
get_function_info <- get("get_function_info")
function_info <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  pkg <- post_body[["packageName"]]
  func <- post_body[["functionName"]]
  list(htmlData = get_function_info(pkg = pkg, func = func))
}
get_function_graph <- get("get_function_graph")
function_graph <- function(req, res) {
  pkg <- "stats" # default
  if (nchar(req$QUERY_STRING) > 0)
    pkg <- sub("\\?packageName=", "", req$QUERY_STRING)
  graph_data <- get_function_graph(pkg = pkg)
  list(packages = graph_data$packages, nodes = graph_data$nodes,
    edges = graph_data$edges)
}
get_package_info <- get("get_package_info")
package_info <- function(req, res) {
  pkg <- "stats" # default
  if (nchar(req$QUERY_STRING) > 0)
    pkg <- sub("\\?packageName=", "", req$QUERY_STRING)
  package_info <- get_package_info(pkg)
  list(packageInfo = package_info)
}
get_local_packages <- get("get_local_packages")
get_package_graph <- get("get_package_graph")
pkg_relations <- get("pkg_relations")
package_graph <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  priorities <- post_body$"priorities"
  s_letters <- post_body$"letters"
  local_packages <- get_local_packages(priorities, s_letters)
  graph_data <- get_package_graph(pkgs = local_packages[, "Package"],
    localPkgs = local_packages, relations = pkg_relations)
  list(nodes = graph_data$nodes, edges = graph_data$edges)
}
package_simple <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  priority <- post_body$"priority"
  if (priority == "all") {
    dat <- installed.packages()[, ]
  } else {
    dat <- installed.packages(priority = priority)[, ]
  }
  list(data = data.frame(dat[, c("Package", "Version", "Depends")]))
}
pkg_columns <- c("Package", "Version", "Priority", "Depends", "Imports",
  "Built")
package_table <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  priorities <- post_body$"priorities"
  s_letters <- post_body$"letters"
  dat0 <- data.frame(get_local_packages(priorities, s_letters)[, pkg_columns])
  dat <- data.frame(dat0, Description = sapply(dat0$Package, function(x) {
      packageDescription(x)$Description
    }))
  list(data = dat)
}
first_letters <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  priorities <- post_body$"priorities"
  if (!"non" %in% priorities) {
    dat <- installed.packages(priority = priorities)[, "Package"]
  } else {
    dat <- installed.packages()[, c("Package", "Priority")]
    priorities <- priorities[priorities != "non"]
    dat0 <- dat[is.na(dat[, "Priority"]), "Package"]
    if (length(priorities) < 2) {
      dat0 <- c(dat0, dat[!is.na(dat[, "Priority"])
        & dat[, "Priority"] == priorities, "Package"])
      dat <- dat0
    } else {
      dat <- dat[, "Package"]
    }
  }
  s_letters <- unique(toupper(unique(substr(dat, 1, 1))))
  list(data = data.frame(key = s_letters, value = s_letters,
    checked = TRUE), sLetters = s_letters)
}
users <- get("users") # get user info from env settings
user_login <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  userid <- post_body$"username"
  password <- post_body$"password"
  if (userid %in% users$id &&
    password == users[users$id == userid, ]$password) {
    res$status <- 200 # success
    list(result = list(token = userid))
  } else {
    res$status <- 401 # Unauthorized
    list(error = "password not correct.")
  }
}
user_register <- function(req, res) {
  if (req$REQUEST_METHOD != "POST") {
    res$status <- 401 # error
    list(error = "Suggestion: register should be defined as a POST method.")
  } else {
    post_body <- jsonlite::fromJSON(req$postBody)
    userid <- post_body$"email"
    username <- post_body$"username"
    password <- post_body$"password"
    if (userid %in% users$id) {
      list(error = "Suggestion: The email already exists.")
    } else {
      new_user <- data.frame(id = userid, name = username,
      username = username, password = password, role = "graphviewr",
      token = userid, avatar = username, create_time = Sys.time())
      data.table::fwrite(new_user, "data/users.csv", append = TRUE)
      users <<- rbind(users, new_user)
      res$status <- 200 # success
      list(data = list(username = userid, password = password))
    }
  }
}
user_info <- function(req, res) {
  param <- req$HTTP_ACCESS_TOKEN
  if (length(param) == 0) {
    res$status <- 401 # error
    list(error = "Please login to get user info.")
  } else {
    res$status <- 200 # success
    list(result = users[users$token == param, ])
  }
}

router_filter <- function(req, res) {
  switch(req$PATH_INFO,
    "/graphviewR/auth/login" = user_login(req, res),
    "/graphviewR/auth/register" = user_register(req, res),
    "/graphviewR/user/info" = user_info(req, res),
    "/graphviewR/package/getFirstLetters" = first_letters(req, res),
    "/graphviewR/package/getPackageTable" = package_table(req, res),
    "/graphviewR/package/getPackageSimple" = package_simple(req, res),
    "/graphviewR/package/getPackageGraph" = package_graph(req, res),
    "/graphviewR/package/getPackageInfo" = package_info(req, res),
    "/graphviewR/package/getFunctionGraph" = function_graph(req, res),
    "/graphviewR/function/getFunctionInfo" = function_info(req, res),
    "/graphviewR/function/getFunctionHelp" = function_help(req, res),
    "/graphviewR/task/list" = list_task(req, res),
    "/graphviewR/task/remove" = remove_task(req, res),
    "/graphviewR/task/save" = save_task(req, res),
    "/graphviewR/task/update" = update_task(req, res),
    "/graphviewR/log/list" = list_log(req, res),
    "/graphviewR/log/hours" = hours_log(req, res),
    "/graphviewR/log/trend" = trend_log(req, res),
    "/graphviewR/editor/eval" = eval_code(req, res)
  )
}
specific_filters <- list(
  # auth_user = auth_user,
  # show_args = show_args,
  router_filter = router_filter
)
