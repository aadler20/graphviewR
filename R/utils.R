trim_rd_exp <- function(rd_exp) {
  if (length((ind <- grep("^[\n]", rd_exp))) > 0)
    rd_exp <- rd_exp[-ind]
  rd_exp <- rd_exp[rd_exp != " "]
  if (length((ind <- grep("[\n]$", rd_exp))) > 0)
    rd_exp[ind] <-  gsub("\n", "", rd_exp[ind])
  if (length((ind <- grep(",$", rd_exp))) > 0)
    rd_exp[ind] <-  gsub(",", "", rd_exp[ind])
  rd_exp
}

null2char <- function(string) {
  ifelse(is.null(string), "-", string)
}