#' Title
#'
#' @param x any \R Object
#' @param output_file `character` scalar. A file path to the desired output file
#' @param view `logical` scalar. Should the result be opened in the RStudio
#'   Viewer? (defaults to `TRUE` in RStudio)
#' @param ... passed on to [rmarkdown::render]
#' 
#' @param side
#'
#' @return `x` (invisibly)
#' @export
#'
#' @examples
report <- function(
  x,
  output_file = tempfile(),
  view = requireNamespace("rstudioapi", quietly = TRUE),
  ...
){
  assert_that(
    is_scalar_character(output_file),
    is_scalar_tf(view)
  )
  
  UseMethod("report")
}




#' @rdname report 
#' @param template `character` scalar. Path to the template file.
#' @export
report.rotspot_metrics <- function(
  x,
  output_file = tempfile(),
  view = requireNamespace("rstudioapi", quietly = TRUE),
  ...,
  template = system.file("templates/report_rotspot_metrics.rmd", package = "rotspot", mustWork = TRUE)
){
  assert_that(
    is_scalar_character(template) && file.exists(template)
  )
  
  out <- rmarkdown::render(template, params = list(dat = x))

  if (view){
    rstudioapi::viewer(out)
  }
  
  x
}
