# TODO: support multiline comments (for C?)

#' Collect Lines of Code for a Set of Files
#'
#' @description
#' A *line of code* is defined as a line of text that is not either pure
#' whitespace or only comments. No further checks to verify if the input is
#' code are applied.
#' The number of lines of code can be used as a simple measure for code
#' complexity. It is not a very reliable measure, but easy to understand and
#' analyse.
#'
#' `collect_loc()` counts the number of lines of code in one or more source
#'  files
#'
#'
#' @param x a `character` vector of text (the contents of a source code file)
#' @param files a `character` vector of source code files (all files must exist)
#' @param language `character` scalar. The programming language. Currenlty only
#'   used to determine `comment_char`.
#' @param comment_char the character that can be used to denote comments
#'   for `language` (can also be a regex pattern)
#'
#' @return
#'   `collect_loc` returns an `integer` vector of the same length as `files`,
#'   containing the number of lines per file. The vector is named and the names
#'   correspond to the names of the source files.
#'
#' @export
#'
#' @examples
#' src <- c("#add foo to bar", "foo + bar")
#' is_loc(src)
#' keep_loc(src)
#' count_loc(src)
#'
#' tf <- tempfile()
#' writeLines(src, tf)
#' collect_loc(tf)
#'
collect_loc <- function(
  files
){
  assert(all(file.exists(files)))
  vapply(files, function(x) count_loc(readLines(x)), integer(1))
}



#' @description
#' `count_loc()` counts the number of elements in a character vector that are
#'   lines of code.
#'
#' @rdname collect_loc
#' @return `count_loc()` returns an `integer` scalar of the lines of code in `x`
#' @export
count_loc <- function(
  x,
  language = "R",
  comment_char = get_comment_char(language)
){
  assert(is.character(x))
  sum(is_loc(x))
}



#' @description
#' `is_loc()` checks if the elements of a `character` vector are lines of code
#'
#' @rdname collect_loc
#' @return `is_loc()` returns a `logical` vector of the same length as `x`
#' @export
is_loc <- function(
  x,
  language = "R",
  comment_char = get_comment_char(language)
){
  stopifnot(
    is.character(x),
    is_scalar_character(comment_char)
  )
  pat <- paste0("(^\\s*$)|(^\\s*", comment_char, ".*$)")
  !grepl(pat, x)
}




#' @description
#' `keep_loc()` keeps only lines of code and removes all lines of whitespace
#'   and comments from a character vector
#'
#' @rdname collect_loc
#' @return keep_loc returns `x` without pure whitespace and comment lines
#' @export
keep_loc <- function(
  x,
  language = "R",
  comment_char = get_comment_char(language)
){
  x[is_loc(x, language = language, comment_char = comment_char)]
}




# utils -------------------------------------------------------------------

get_comment_char <- function(
  x
){
  switch(
    tolower(x),
    "c" = "//",
    "r" = "#",
    "cpp" = "//"
  )
}
