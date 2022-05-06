# users
usernames <- c("admin", "graphviewr")
passwords <- c("admin", "graphviewr")
# create basic users list each time restarting R backend
users <- data.frame(id = usernames, name = usernames, username = usernames,
password = passwords, role = usernames, token = usernames, avatar = usernames,
create_time = Sys.time())
rownames(users) <- users$id
# save users list in .csv file for backup
data.table::fwrite(users, "data/users.csv")
# workspace module
# package module
pkg_rds_files <- c("package.rds", "Rd.rds", "vignette.rds")
pkg_relations <- c("depends", "suggests", "enhances")
dep_types <- c("Imports", "Depends", "LinkingTo")
# function module
# symbols
info404 <- "INFO404"