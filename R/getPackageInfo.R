grenv <- get("grenv")
pkg_rds_files <- get("pkg_rds_files")
demo_dir <- get("demo_dir")

#' @param pkg package name
#' @return package info in .rds files
#'
get_package_info <- function(pkg) {
  path <- find.package(pkg, quiet = TRUE)
  if (length(path) > 0) {
    assign(pkg, new.env(parent = grenv), envir = grenv)
    metaPath <- paste(path, "Meta", sep = .Platform$file.sep)
    fs <- list.files(metaPath)
    packageInfo <- vector("list", length(pkg_rds_files))
    names(packageInfo) <- pkg_rds_files
    for (pfn in pkg_rds_files) {
      if (pfn %in% fs) {
        if (pfn == "package.rds") {
          pkgDesc <- readRDS(file.path(metaPath, pfn))$DESCRIPTION
          packageInfo[[pfn]] <- data.frame(Name = names(pkgDesc),
            Value = pkgDesc)
        }
        if (pfn == "Rd.rds") {
          packageInfo[[pfn]] <- readRDS(file.path(metaPath, pfn))[,
            c("Name", "Title", "Type", "Concepts", "Aliases", "Keywords")]
          for (i in c("Concepts", "Aliases", "Keywords")) {
            packageInfo[[pfn]][[i]] <- sapply(packageInfo[[pfn]][[i]],
              function(x) paste(x, collapse = ", "))
          }
        }
        if (pfn == "vignette.rds")
          packageInfo[[pfn]] <- readRDS(file.path(metaPath, pfn))
      }
    }
    names(packageInfo) <- gsub(".rds", "", names(packageInfo))
    return(packageInfo)
  }
  return(list(flag = info404, tips = sprintf("
    The %s package is not installed yet!
    please use install.packages(%s) to install this package.", pkg, pkg)))
}

#' @param pkg package name
#' @return packages, nodes for functions; edges for function dependencies
#'
make_function_graph <- function(pkg, merge = FALSE, ...) {
  # get functions in pkg
  envns <- loadNamespace(pkg) # if pkg exists then no effect
  objs <- objects(envns)
  funs <- lapply(objs, function(x) {
    f <- get(x, envns)
    if (is.function(f)) f else NULL
  })
  if (length(funs) == 0)  # in case some packages do not have any functions
    return(list(packages = c(pkg, "base"), nodes = data.frame(),
    edges = data.frame()))
  names(funs) <- objs
  funs <- funs[!sapply(funs, is.null)]
  funs <- unlist(funs, recursive = FALSE)
  fnames <- names(funs)
  # Use id to identify nodes
  edges <- data.frame() # initial value for edges
  fn1 <- character(0)
  lapply(seq_along(funs), function(f) {
    calls <- codetools::findGlobals(funs[[f]], merge = merge)$functions
    if (length(calls) > 0) {
      fn1 <<- c(fn1, calls)
      edges <<- rbind(edges, data.frame(source = calls, target = fnames[f]))
    }
  })
  if (nrow(edges) > 0) {
    edges$source <- as.character(edges$source)
    edges$target <- as.character(edges$target)
  }
  fn1 <- setdiff(unique(fn1), fnames)
  pn1 <- sapply(fn1, function(x) {
      if (exists(x)) {
        pn <- environmentName(environment(get(x)))
        ifelse(pn == "", "base", pn)
      } else "unclear"
  })
  nodes <- data.frame(id = c(fnames, fn1), label = c(fnames, fn1),
    package = c(rep(pkg, length(fnames)), pn1))
  return(list(packages = unique(c(pkg, unique(pn1))), nodes = nodes,
    edges = edges))
}

#' @param pkg package name
#' @return nodes for functions; edges for function dependencies
#'
get_function_graph <- function(pkg) {
  assign(pkg, new.env(parent = grenv), envir = grenv)
  return(make_function_graph(pkg))
}


#' @param pkg package name
#' @return package demo code
#'
get_package_demo <- function(pkg) {
  examples_dir <- paste(demo_dir, pkg, sep = .Platform$file.sep)
  file_dir <- paste(examples_dir, "demo.r", sep = .Platform$file.sep)
  if (file.exists(file_dir)) {
    examples <- paste(readLines(file_dir), collapse = "\n")
  } else {
    examples <- paste(sprintf("# demo for package %s do not exist. ", pkg),
    sprintf("# please use help(package = '%s') for more info.", pkg),
      sep = "\n")
  }
  return(list(fileDir = file_dir, examples = examples))
}