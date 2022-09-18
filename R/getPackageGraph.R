# Copy of tools:::split_op_version.
split_op_version <- function(x) {
  pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
  x1 <- sub(pat, "\\1", x)
  x2 <- sub(pat, "\\2", x)
  if (x2 != x1) {
    pat <- "[[:space:]]*([[<>=!]+)[[:space:]]+(.*)"
    version <- sub(pat, "\\2", x2)
    if (!grepl("^r", version))
      version <- package_version(version)
    list(name = x1, op = sub(pat, "\\1", x2), version = version)
  }
  else list(name = x1)
}
# Copy of tools:::.split_dependencies.
split_dependencies <- function(x) {
  if (!length(x)) return(list())
  x <- unlist(strsplit(x, ","))
  x <- sub("[[:space:]]+$", "", x)
  x <- unique(sub("^[[:space:]]*(.*)", "\\1", x))
  names(x) <- sub("^([[:alnum:].]+).*$", "\\1", x)
  lapply(x, split_op_version)
}

# base packages
base_pkgs <- function()
  names(which(installed.packages()[, "Priority"] == "base"))

# Clean package fields.
# Given the value from a field like 'Depends' in a package's DESCRIPTION file,
# return a character vector of package names with the version restrictions
# stripped and \R~removed.

# @param val Value from a field like 'Depends' in a package's DESCRIPTION file
clean_pkg_field <- function(val) {
  if (is.na(val)) return(character(0))
  val <- names(split_dependencies(val))
  if (is.null(val)) return(character(0))
  val <- val[!val %in% "R"]
  if (length(val)) return(val)
  character(0)
}

fastdf <- function(list) {
  rows <- unique(unlist(lapply(list, NROW)))
  stopifnot(length(rows) == 1)
  class(list) <- "data.frame"
  attr(list, "row.names") <- c(NA_integer_, -rows)
  list
}

# Imports: needed by users at runtime
# Depends: = Imports prior to 2.14.0,
#   now can also be used to state a minimum version for R itself
# LinkingTo: packages listed here rely on C or C++ code in another package
# Suggests: either needed for development tasks or
#   might unlock optional functionality for users
# Enhances: packages listed here are “enhanced” by your package

add_dep_type <- function(p,
  type = c("Imports", "Depends", "LinkingTo", "Suggests"), pdb) {
  if (!p %in% rownames(pdb)) {
    fastdf(list(
      target = character(0),
      source = character(0),
      elabel = character(0)
    ))

  } else {
    x <- clean_pkg_field(pdb[p, type])
    fastdf(list(
      target = x,
      source = rep(p, length(x)),
      elabel = rep(type, length(x))
    ))
  }
}

# get local packages given priorities and s_letters
get_local_packages <- function(priorities, s_letters) {
  print(priorities)
  print(s_letters)
  if (length(priorities) == 0 | length(s_letters) == 0) {
     return(list(localPackages = NULL, colNames = NULL))
  }
  if (!"non" %in% priorities) {
    dat0 <- installed.packages(priority = priorities)
  } else {
    dat <- installed.packages()
    priorities <- priorities[priorities != "non"]
    dat0 <- dat[is.na(dat[, "Priority"]), ]
    if (length(priorities) < 2) {
      dat0 <- rbind(dat0, dat[!is.na(dat[, "Priority"]) &
        dat[, "Priority"] == priorities, ])
    }
  }
  # first letters
  if (length(s_letters) > 0) {
    if ("all" %in% s_letters) {
      return(dat0)
    } else {
      dat <- character(0)
      lapply(dat0[, "Package"], function(x) {
        if (tolower(substr(x, 1, 1)) %in% s_letters |
          toupper(substr(x, 1, 1)) %in% s_letters) {
            dat <<- rbind(dat, dat0[x, ])
        }
      })
      rownames(dat) <- dat[, "Package"]
    }
  }
  return(list(localPackages = dat, colNames = colnames(dat)))
}

#' Create dependency graph from local installed packages.
#'
#' Each package is a node, and edges stand for dependencies
#'
#' @inheritParams installed.packages
#'
#' @param pkgs Character vector of packages interested
#'
#' @param localPkgs result from installed.packages
#'
#' @param depends If TRUE, retrieves `Depends`, `Imports` and `LinkingTo` 
#' dependencies (non-recursively)
#'
#' @param suggests If TRUE, retrieves Suggests dependencies (non-recursively)
#'
#' @param enhances If TRUE, retrieves Enhances dependencies (non-recursively)
#'
#' @param ... Other arguments passed to [installed.packages()]
#'
#' @export
#' @family dependency functions
#'
#' @return getPackageGraph() returns package names and graph data
#'
get_package_graph <- function(
  pkgs = NULL, localPkgs = NULL, relations = c("depends"), recursive = FALSE,
  includeBasePkgs = TRUE, repos = "https://cran.rstudio.com",
  types = dep_types, ...) {
  if (is.null(localPkgs)) {
    localPkgs <- installed.packages(lib.loc = lib.loc, priority = priority,
      noCache = noCache, fields = fields, subarch = subarch, )
  }
  pkgEdge <- function(p, type = types, pdb) {
    do.call(rbind, lapply(type, function(t)add_dep_type(p, t, pdb = pdb)))
  }
  pkgEdges <- function(pp, type = types, pdb) {
    do.call(rbind, lapply(pp, pkgEdge, type = type, pdb = pdb))
  }
  # Build nodes and edges
  if (!recursive) {
    edges <- pkgEdges(pkgs, type = types, localPkgs)
    pkgs_n <- unique(edges$target)
  } else {
    availPkgs <- available.packages(repos = repos)
    pkgs_n <- unique(unlist(tools::package_dependencies(pkgs,
      db = availPkgs, which = types,
      recursive = TRUE)
    ))
    pkgs_r <- unique(c(pkgs_n, pkgs))
    edges <- pkgEdges(pkgs_r,
      type = types, availPkgs)
  }
  if ("suggests" %in% relations) {
    edges1 <- pkgEdges(pkgs, type = c("Suggests"), localPkgs)
    edges <- rbind(edges, edges1)
    pkgs_n <- unique(c(edges1$target, pkgs_n))
  }
  if ("enhances" %in% relations) {
    edges2 <- pkgEdges(pkgs, type = c("Enhances"), localPkgs)
    edges <- rbind(edges, edges2)
    pkgs_n <- unique(c(edges2$target, pkgs_n))
  }
  nedges <- nrow(edges)
  if (nedges && !includeBasePkgs) {
    edges <- edges[!(edges[["target"]] %in% base_pkgs()), ]
  }
  pkgs_u <- setdiff(pkgs_n, pkgs) # uninstalled packages
  combo_pkgs <- c(pkgs, pkgs_u)
  nodes <- data.frame(id = combo_pkgs, label = combo_pkgs,
      installed = rep(c("yes", "no"), c(length(pkgs), length(pkgs_u))))
  g6json <- list(packages = pkgs, nodes = nodes, edges = edges)
  return(g6json)
}
