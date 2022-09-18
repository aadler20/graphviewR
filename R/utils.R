prim_classes <- c("integer", "double", "character", "numeric",
  "matrix", "data.frame")
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

lt2sign <- function(txt) {
  return(gsub("&lt;", "<", txt))
}

href2vue <- function(func_lines, pkg) {
  re_exp <- "(href=\")([A-Za-z0-9]+)(\\.html\")"
  re_exp2 <- "(\\.\\./\\.\\./)([A-Za-z0-9]+)(/html/)([A-Za-z0-9]+)(\\.html)"
  # find lines
  lines_no <- grep(re_exp, func_lines)
  if (length(lines_no) > 0) {
    for (line_no in lines_no) {
      # find position
      line_pos0 <- as.integer(gregexpr(re_exp, func_lines[line_no])[[1]])
      # divide into substring
      line_pos <- c(line_pos0, nchar(func_lines[line_no]))
      for (i in rev(seq_along(line_pos0))) {
        line_sub <- substring(func_lines[line_no],
          line_pos[i] - 1, line_pos[i + 1] - 1)
        # substitute
        sub_pos <- as.integer(regexec(re_exp, line_sub)[[1]])
        to_sub <- substring(line_sub, sub_pos[3], sub_pos[4] - 1)
        func_lines[line_no] <- gsub(paste0(to_sub, ".html\""),
          sprintf("#/function/listview/%s/%s\" target = \"_blank\"",
          pkg, to_sub),
          func_lines[line_no])
      }
    }
  }
  # find lines with pattern re_exp2
  lines_no <- grep(re_exp2, func_lines)
  if (length(lines_no) > 0) {
    for (line_no in lines_no) {
      # find position
      line_pos0 <- as.integer(gregexpr(re_exp2, func_lines[line_no])[[1]])
      # divide into substring
      line_pos <- c(line_pos0, nchar(func_lines[line_no]))
      for (i in rev(seq_along(line_pos0))) {
        line_sub <- substring(func_lines[line_no],
          line_pos[i] - 1, line_pos[i + 1] - 1)
        # substitute
        sub_pos <- as.integer(regexec(re_exp2, line_sub)[[1]])
        to_sub <- paste0(substring(line_sub, sub_pos[1],
          sub_pos[6] - 1), ".html")
        pkg_name <- substring(line_sub, sub_pos[3], sub_pos[4] - 1)
        func_name <- substring(line_sub, sub_pos[5], sub_pos[6] - 1)
        func_lines[line_no] <- gsub(to_sub,
          sprintf("/function/listview/%s/%s", pkg_name, func_name),
          func_lines[line_no])
      }
    }
  }
  return(func_lines)
}
