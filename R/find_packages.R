find_packages <- function(
  dir = ".",
  pattern = "(.R$)|(.r$)"
){
  if (is_r_package(dir)){
    pkgs <- dir

  } else {
    dirs <- list.dirs(dir, recursive = FALSE)
    pkgs <- vapply(dirs, is_r_package, logical(1))
    pkgs <- names(pkgs)[pkgs]
  }

  if (identical(length(pkgs), 0L)){
    stop("'dir' must be an R package, or a directory containing R packages (packages in subdirectories are not supported)")
  }

  path_real(pkgs)
}




list_source_files <- function(
  indir = ".",
  pattern = "(.R$)|(.r$)"
){
  list.files(".", recursive = TRUE, pattern = pattern)
}




is_r_package <- function(
  dir = "."
){
  all(file.exists(file.path(dir, c("DESCRIPTION", "NAMESPACE", "R"))))
}
