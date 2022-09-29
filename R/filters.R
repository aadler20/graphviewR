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

# user
users <- get("users") # get user info from env settings
auth_login <- function(req, res) {
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
auth_register <- function(req, res) {
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

# package
first_letters <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  priorities <- post_body$"priorities"
  dat <- NULL
  s_letters <- NULL
  if (length(priorities) > 0) {
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
    dat <- data.frame(key = s_letters, value = s_letters, checked = TRUE)
  }
  list(data = dat, sLetters = s_letters)
}
get_local_packages <- get("get_local_packages")
get_package_graph <- get("get_package_graph")
pkg_relations <- get("pkg_relations")
package_graph <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  local_packages <- post_body$localPackages
  col_names <- post_body$colNames
  graph_data <- list(nodes = character(0), edges = character(0))
  if (length(local_packages) > 0) {
    if (is.null(colnames(local_packages))) {
      colnames(local_packages) <- col_names
    }
    if (is.null(rownames(local_packages))) {
      rownames(local_packages) <- local_packages[, "Package"]
    }
    if (nrow(local_packages) > 0) {
      graph_data <- get_package_graph(pkgs = local_packages[, "Package"],
        localPkgs = local_packages, relations = pkg_relations)
    }
   }
  list(nodes = graph_data$nodes, edges = graph_data$edges)
}
get_package_info <- get("get_package_info")
package_info <- function(req, res) {
  pkg <- "stats" # default
  if (nchar(req$QUERY_STRING) > 0)
    pkg <- sub("\\?packageName=", "", req$QUERY_STRING)
  package_info <- get_package_info(pkg)
  list(packageInfo = package_info)
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
get_local_packages <- get("get_local_packages")
local_packages <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  priorities <- post_body$"priorities"
  s_letters <- post_body$"letters"
  dat <- get_local_packages(priorities, s_letters)
  list(data = dat)
}
pkg_columns <- c("Package", "Version", "Priority", "Depends", "Imports",
  "Built")
package_table <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  local_packages <- post_body$localPackages
  col_names <- post_body$colNames
  dat <- data.frame()
  if (length(local_packages) > 0) {
    if (is.null(colnames(local_packages))) {
      colnames(local_packages) <- col_names
    }
    if (is.null(rownames(local_packages))) {
      rownames(local_packages) <- local_packages[, "Package"]
    }
    if (nrow(local_packages) > 0) {
      local_packages <- data.frame(local_packages)
      dat0 <- data.frame(local_packages[, pkg_columns])
      dat <- data.frame(dat0, Description = sapply(dat0$Package, function(x) {
          packageDescription(x)$Description
        }))
    }
  }
  list(data = dat)
}

get_package_demo <- get("get_package_demo")
package_demo <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  local_packages <- post_body$localPackages
  col_names <- post_body$colNames
  dat <- data.frame()
  if (nrow(local_packages) > 0) {
    if (is.null(colnames(local_packages))) {
      colnames(local_packages) <- col_names
    }
    if (is.null(rownames(local_packages))) {
      rownames(local_packages) <- local_packages[, "Package"]
    }
    dat <- lapply(rownames(local_packages), function(x) {
      demo <-  get_package_demo(x)
      if (nrow(demo) > 0) {
        return(list(Package = x,
          Description = packageDescription(x)$Description, demo = demo))
      }
    })
  }
  # only return package with demos
  dat <- dat[!sapply(dat, is.null)]
  print(dat)
  list(data = dat)
}

demo_code <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  print(post_body)
  print(post_body[["Item"]])
  print(post_body[["Title"]])
  list(demo = "1 + 1", fileDir = "C:/R")
}

# function
get_function_examples <- get("get_function_examples")
function_examples <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  pkg <- post_body[["packageName"]]
  func <- post_body[["functionName"]]
  examples_data <- get_function_examples(pkg = pkg, func = func)
  list(examples = examples_data$examples, fileDir = examples_data$fileDir)
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
  func_info <- get_function_info(pkg = pkg, func = func)
  list(htmlData = func_info$func_lines, demoData = func_info$demo_lines)
}

# editor
unjson_classes <- c("lm", "qr", "summary.lm", "try-error")
eval_code <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  code <- post_body[["code"]]
  code <- gsub("\r", "", code)
  code_exp <- parse(text = code)
  print(code_exp)
  result <- vector("list", length(code_exp))
  for (i in seq_along(code_exp)) {
    result[[i]] <- try(eval(code_exp[i]), TRUE)
  }
  result <- lapply(seq_along(result), function(x) {
    rx <- result[[x]]
    if (class(rx) %in% prim_classes) {
      return(list(RN = x, Class = class(rx),
      structure = data.frame(names = typeof(rx),
        value = paste(as.character(rx), collapse = ","))))
    } else {
      if (!is.null(names(rx))) {
        str_df <- data.frame(names = names(rx), value = as.character(rx))
      } else {
        str_df <- data.frame(names = typeof(rx), value = as.character(rx))
      }
      return(list(RN = x, Class = class(rx), structure = str_df))
    }
  })
  print(result)
  list(data = result)
}
save_code <- function(req, res) {
  post_body <- jsonlite::fromJSON(req$postBody)
  code <- post_body[["code"]]
  code_dir <- post_body[["codeDir"]]
  code <- gsub("\r", "", code)
  res <- "success saving code"
  if (file.exists(code_dir)) {
    writeLines(code, code_dir)
  } else {
    res <- "failure saving code"
  }
  return(res)
}
has_internet <- function(req, res) {
  res <- curl::has_internet()
  return(res)
}

api_filters <- function(req, res) {
  print(req$PATH_INFO)
  # user
  if (req$PATH_INFO == "/graphviewR/auth/login") {
    auth_login(req, res)
  } else if (req$PATH_INFO == "/graphviewR/auth/register") {
    auth_register(req, res)
  } else if (req$PATH_INFO == "/graphviewR/user/info") {
    user_info(req, res)
  # package
  } else if (req$PATH_INFO == "/graphviewR/package/getFirstLetters") {
    first_letters(req, res)
  } else if (req$PATH_INFO == "/graphviewR/package/getLocalPackages") {
    local_packages(req, res)
  } else if (req$PATH_INFO == "/graphviewR/package/getPackageDemo") {
    package_demo(req, res)
  } else if (req$PATH_INFO == "/graphviewR/package/getDemoCode") {
    demo_code(req, res)
  } else if (req$PATH_INFO == "/graphviewR/package/getPackageGraph") {
    package_graph(req, res)
  } else if (req$PATH_INFO == "/graphviewR/package/getPackageInfo") {
    package_info(req, res)
  } else if (req$PATH_INFO == "/graphviewR/package/getPackageSimple") {
    package_simple(req, res)
  } else if (req$PATH_INFO == "/graphviewR/package/getPackageTable") {
    package_table(req, res)
  # function
  } else if (req$PATH_INFO == "/graphviewR/function/getFunctionExamples") {
    function_examples(req, res)
  } else if (req$PATH_INFO == "/graphviewR/function/getFunctionHelp") {
    function_help(req, res)
  } else if (req$PATH_INFO == "/graphviewR/function/getFunctionInfo") {
    function_info(req, res)
  # task
  } else if (req$PATH_INFO == "/graphviewR/task/list") {
    list_task(req, res)
  } else if (req$PATH_INFO == "/graphviewR/task/remove") {
    remove_task(req, res)
  } else if (req$PATH_INFO == "/graphviewR/task/save") {
    save_task(req, res)
  } else if (req$PATH_INFO == "/graphviewR/task/update") {
    update_task(req, res)
  # log
  } else if (req$PATH_INFO == "/graphviewR/log/hours") {
    hours_log(req, res)
  } else if (req$PATH_INFO == "/graphviewR/log/list") {
    list_log(req, res)
  } else if (req$PATH_INFO == "/graphviewR/log/trend") {
    trend_log(req, res)
  } else if (req$PATH_INFO == "/graphviewR/editor/eval") {
    eval_code(req, res)
  } else if (req$PATH_INFO == "/graphviewR/editor/save") {
    save_code(req, res)
  } else if (req$PATH_INFO == "/graphviewR/utils/hasInternet") {
    has_internet(req, res)
  } else if (
    req$PATH_INFO == "/"
    || length(grep("/js/.*\\.js", req$PATH_INFO)) > 0
    || length(grep("/css/.*\\.css", req$PATH_INFO)) > 0
    || length(grep("/assets/.*\\.svg", req$PATH_INFO) > 0)) {
    print(paste("forward", req$PATH_INFO))
    plumber::forward()
  }
}