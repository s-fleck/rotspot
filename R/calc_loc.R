calc_loc <- function(
  file
){
  x <- readLines(file)
  sum(!grepl("(^\\s*#.*$)|(^\\s*$)", x))
}




