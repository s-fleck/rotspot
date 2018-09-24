as_rotspot_metrics <- function(
  x
){
  
}


collect_metrics <- function(
  dir = "."
){
  if (is_r_package(dir))
    res <- collect_metrics_single(dir)
  else
    res <- collect_metrics_multi(dir)
  
  structure(
    res,
    class = union("rotspot_metrics", class(res)),
    dir = basename(fs::path_real(dir))
  )
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
  res
}




collect_metrics_multi <- function(
  dir = "."
){
  dirs <- list.dirs(dir, recursive = FALSE)
  dirs <- dirs[is_r_package(dirs)]
  res <- lapply(dirs, function(x) try(collect_metrics(x)))
  
  browser()

  res <- data.table::rbindlist(
     res[!vapply(res, inherits, logical(1), "try-error")],
     idcol = "pkg"
  )
}




summary.rotspot_metrics <- function(
  x
){
  authors <- x[, .(author = unique(author)), by = "hash"][, .(commits = .N), by = "author"]
  data.table::setorderv(authors, "commits", order = -1L)


  languages <- unique(x, by = "hash")
  languages[, language := tolower(tools::file_ext(languages$entity))]
  languages[, sum(loc, na.rm = TRUE), by = "language"]


  cat("Summary for '", attr(x, "dir"), "':", sep = "")

  cat(sprintf(""))
}




plot.rotspot_metrics <- function(x){
  dd <- x[, .(commits = length(unique(hash))), by = c("date", "pkg")]

  ggplot2::ggplot(
    dd,
    ggplot2::aes(
      x = date,
      y = commits    )
  ) +
    ggplot2::geom_bar(stat = "identity", color = "#FDE725FF") +
    ggplot2::theme_dark() +
    ggplot2::facet_grid(pkg ~ .)

}




is_r_file <- function(x){
  grepl("(.*R$)|(.*r$)", x)
}




plot_commits <- function(
  x,
  template = system.file("templates", "report_commits.Rmd", package = "rotspot")
){
    dd <- x[, .(commits = length(unique(hash))), by = c("date", "pkg")]
    
    p <- ggplot2::ggplot(
      dd,
      ggplot2::aes(
        x = date,
        y = commits
      )
    ) + 
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::facet_grid(pkg ~ .) +
      ggplot2::scale_color_viridis_c() +
      ggplot2::theme_dark()
}




report.rotspot_metrics <- function(
  x,
  output_file = tempfile(),
  view = requireNamespace("rstudioapi", quietly = TRUE),
  ...,
  template = system.file("templates/rotspot.rmd", package = "rotspot", mustWork = TRUE)
){
  out <- rmarkdown::render(template, params = list(dat = x))

  if (view){
    rstudioapi::viewer(out)
  }

}
