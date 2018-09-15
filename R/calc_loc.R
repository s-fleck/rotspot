calc_loc <- function(
  files
){
  vapply(files, calc_loc_single, integer(1))
}



calc_loc_single <- function(
  file
){
  x <- readLines(file)
  sum(!grepl("(^\\s*#.*$)|(^\\s*$)", x))
}




