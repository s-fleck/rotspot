#' Collect Metrics
#'
#' Collect code-quality metrics for R packages from files and git repositories
#'
#' @param dir an \R-Package, or a directory that contains several \R-Packages.
#'
#' @return A `data.table` with additional subclass `rotspot_metrics`
#' @export
#'
collect_metrics <- function(
  dir = "."
){
  if (is_r_package(dir))
    res <- collect_metrics_single(dir)
  else
    res <- collect_metrics_multi(dir)

  as_rotspot_metrics(res)
}




collect_metrics_single <- function(
  dir = "."
){
  withr::with_dir(dir, {
    log <- collect_history(dir)

    src_files <- log[is_r_file(entity)]$entity
    src_files <- unique(src_files[file.exists(src_files)])

    loc <- collect_loc(unique(src_files))
    loc <- data.table(
      entity = names(loc),
      loc = unname(loc)
    )

    ind <- collect_indent(src_files)
    ind <- data.table(
      entity = names(ind),
      indent = unname(ind)
    )
  })

  res <- merge(log, loc, by = "entity", all.x = TRUE)
  res <- merge(res, ind, by = "entity", all.x = TRUE)
  res[, pkg := basename(fs::path_real(dir))]
  data.table::setcolorder(res, union("pkg", names(res)))

  as_rotspot_metrics(res)
}




collect_metrics_multi <- function(
  dir = "."
){
  dirs <- list.dirs(dir, recursive = FALSE)
  sel  <- is_r_package(dirs)
  pkgs <- dirs[sel]


  flog.info("Found %s R packages in directory", sum(sel))
  pb   <- progress::progress_bar$new(format = ":pkg - [:bar] :percent", total = sum(sel))
  pb$tick(0)


  res <- list()
  for (i in seq_along(pkgs)){
    res[[i]] <- tryCatch(
      collect_metrics_single(pkgs[[i]]),
      error = function(e) { cat("\n"); flog.fatal(e); NULL }
    )
    pb$tick(tokens = list(pkg = pkgs[[i]]))
  }

  ok <- vapply(res, function(x) !is.null(x), logical(1))

  flog.info(
    "Succesfully collected metrics for %s of %s packages", sum(ok), length(ok)
  )

  res <- data.table::rbindlist(res[ok])

  as_rotspot_metrics(res)
}




is_r_file <- function(x){
  grepl("(.*R$)|(.*r$)", x)
}
