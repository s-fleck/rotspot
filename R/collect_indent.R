#' Collect Indent Levels for a Set of Files
#'
#' The number of different indention levels is a natuarl metric for code
#' complexity
#'
#' @param files
#'
#' @return
#' @export
#' @family code complexity metrics
#'
#' @examples
collect_indent <- function(
  files
){
  setNames(
    lapply(files, function(x) count_indent(readLines(x))),
    files
  )
}
# TODO: guess indent spaces
# TODO: analysis based on parsed R output



count_indent <- function(
  x
){
  x <- keep_loc(x)

  vapply(
    gregexpr("^\\s*", x),
    function(x) attr(x, "match.length"),
    integer(1)
  )
}
