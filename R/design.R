# 2022-02-25
# 1 Build server using httpuv
# 2 Write R package using Visual Studio Code
# graphviewr should wrap node module in inst foler
# see https://colinfay.me/node-r-package/
# 3 Set all global values in env.R

# server.R: build R server
# api_....R: deal with request and response
# servie_....R: fulfill the tasks to support api_...
# dao.R: data access object

# run time
ptm <- proc.time()
pkg <- "stats"
graph_data <- get_function_graph(pkg = pkg)
proc.time() - ptm

# test namespaces
rm(list = ls())
gc()
source("R/env.R") # set global values
source("R/env_prod.R")
source("R/utils.R")
source("R/getPackageInfo.R") # info of selected package
source("R/getFunctionInfo.R") # info of selected function
merge <- FALSE
pkg <- "KernSmooth"
result <- get_function_graph(pkg)
pkg <- "stats"
func <- "dbeta"
result <- get_function_help(pkg = pkg, func = func)
