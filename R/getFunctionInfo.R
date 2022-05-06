grenv <- get("grenv")
trim_rd_exp <- get("trim_rd_exp")
info404 <- get("info404")
#' @param pkg package name
#' @return directory of help file
lazy_load_db <- function(pkg) {
  assign(pkg, new.env(parent = grenv), envir = grenv) # in case pkg not exists
  path <- find.package(pkg, quiet = TRUE)
  if (length(path) > 0) {
    lazyLoad(paste(path, "help", pkg, sep = .Platform$file.sep),
      envir = grenv[[pkg]])
  }
  return(paste(path, "help", sep = .Platform$file.sep))
}

#' @param pkg package name
#' @param func function name
#' @return function help in pkg/html/'func'.html
#'
get_function_info <- function(pkg, func) {
  funcfile <- paste0(func, ".html")
  path <- find.package(pkg, quiet = TRUE)
  if (length(path) > 0) {
    if (funcfile %in% dir(paste(path, "html", sep = .Platform$file.sep))) {
      func_lines <- readLines(paste(path, "html", funcfile,
        sep = .Platform$file.sep))
      pre_starts <- grep("^<pre>", func_lines)
      pre_ends <- grep("^</pre>", func_lines)
      if (length(pre_starts) > 0 && length(pre_starts) == length(pre_ends)) {
        for (i in seq_along(pre_starts)) {
          if (pre_starts[i] + 1 < pre_ends[i])
            func_lines[(pre_starts[i] + 1):(pre_ends[i] - 1)] <-
              paste0(func_lines[(pre_starts[i] + 1):(pre_ends[i] - 1)], "</br>")
        }
      }
      return(paste(func_lines, collapse = ""))
    } else {
        return(list(flag = info404, tips = sprintf(
        "The help file for function '%s' can not be found in '%s' directory.
        please try to use ??('%s') to get more information.",
        func, paste(path, "html", sep = .Platform$file.sep), func)))
    }
  }
  return(list(flag = info404, tips = sprintf(
    "The %s package is not installed yet!
    please use install.packages(%s) to install this package.", pkg, pkg)))
}
#' @param pkgfunc array with 2 elements: [1] package name [2] function name
#' @return function help in json format
#'
get_function_help <- function(pkg, func, req_type = "info") {
  # lazy load to create promises for later access
  help_path <- lazy_load_db(pkg)
  if (!exists(func, envir = grenv[[pkg]]))
    return(list(flag = info404, tips = sprintf("
    The help file for function '%s' can not be found in '%s' directory.
    please try to use ??('%s') to get more information.",
    func, help_path, func)))
  # if exists then parse Rd file
  frd <- get(func, envir = grenv[[pkg]])
  if (typeof(frd) != "list") { # TODO: aliases
    return(list(flag = info404, tips = sprintf("
    The type of help data for function '%s' is closure.
    This problem is to be solved by checking aliases.", func)))
  }
  ftags <- tools:::RdTags(frd)
  children <- vector("list", length(ftags))
  lapply(seq_along(ftags), function(i) {
    frd_i <- frd[[i]]
    ftags_i <- tools:::RdTags(frd_i)
    fitems <- frd_i[which(ftags_i == "\\item")]
    if (length(fitems) > 0) { # get items
      item_name <- as.character(sapply(fitems, function(x) return(x[[1]])))
      item_desc <- sapply(fitems, function(x) paste(x[[-1]], collapse = ""))
      children_i <- vector("list", length(item_name))
      lapply(seq_along(item_name), function(i) {
        children_i[[i]] <<- list(id = item_name[i], desc = item_desc[i])
      })
      children[[i]] <<- list(id = sub("\\\\", "", ftags[i]),
        desc = sub("\\\\", "", ftags[i]), children = children_i)
      } else {
      desc <- trim_rd_exp(unlist(frd_i))
      csep <- ifelse(ftags[i] == "\\examples", "</br>", " ")
      desc <- paste(desc, collapse = csep)
      if (i > 1 && ftags[i] == ftags[i - 1]) { # in case tags with same name
        children[[i]] <<- list(
          id = paste(sub("\\\\", "", ftags[i]), i, sep = "_"), desc = desc)
      } else {
        children[[i]] <<- list(id = sub("\\\\", "", ftags[i]), desc = desc)
      }
    }
  })
  help_data <- list(id = func, desc = func, children = children)
  return(help_data)
}
