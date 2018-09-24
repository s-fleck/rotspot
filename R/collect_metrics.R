as_rotspot_metrics <- function(
  x
){
  x <- data.table::copy(x)
  data.table::setattr(x, "class", union("rotspot_metrics", class(x) ))
  x
}




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




summary.rotspot_metrics <- function(
  x
){
  authors <- x[, .(author = unique(author)), by = "hash"][, .(commits = .N), by = "author"]
  data.table::setorderv(authors, "commits", order = -1L)
  
  pkgs <- x[, .(commits = length(unique(hash))), by = "pkg"]
  data.table::setorderv(pkgs, "commits", order = -1L)

  languages <- unique(x, by = "hash")
  languages[, language := tolower(tools::file_ext(languages$entity))]
  languages[, sum(loc, na.rm = TRUE), by = "language"]


  cat(
    sprintf("Rotspot metrics for %s package(s) with %s contributor(s)\n\n", 
    nrow(authors), 
    nrow(pkgs))
  )
  
  pad_matrix <- function(x){
    x[, 1] <- stringi::stri_pad_right(x[, 1], max(nchar(x[, 1])) + 2)
    x[, 2] <- stringi::stri_pad_left(x[, 2],  max(nchar(x[, 2])) + 2 )
    x
  }
  
  res_pkgs <- as.matrix(head(pkgs, 10))
  res_pkgs <- rbind(c("Package", "Commits"), res_pkgs)
  res_pkgs <- pad_matrix(res_pkgs)
  
  res_auth  <- as.matrix(head(authors, 10))
  res_auth  <- rbind(c("Author", "Commits"), res_auth)
  res_auth <- pad_matrix(res_auth)
  
  res <- cbind(res_pkgs, "       ", res_auth, "\n")
  res <- apply(res, 1, paste, collapse = "")
  
  cat(res, sep = "")
    
  
}




plot.rotspot_metrics <- function(x){
  dd <- x[, .(commits = length(unique(hash))), by = c("date", "pkg")]
  
  order <- dd[, .(commits = sum(commits)), by = "pkg"]
  data.table::setkeyv(order, "commits")
  dd[, pkg := factor(pkg, levels = rev(order$pkg))]
  

  ggplot2::ggplot(
    dd,
    ggplot2::aes(
      x = date,
      y = commits)
  ) +
    ggplot2::geom_bar(stat = "identity", color = "#FDE725FF") +
    ggplot2::theme_dark() +
    ggplot2::facet_grid(pkg ~ .) 
}




is_r_file <- function(x){
  grepl("(.*R$)|(.*r$)", x)
}




report.rotspot_metrics <- function(
  x,
  output_file = tempfile(),
  view = requireNamespace("rstudioapi", quietly = TRUE),
  ...,
  template = system.file("templates/report_rotspot_metrics.rmd", package = "rotspot", mustWork = TRUE)
){
  out <- rmarkdown::render(template, params = list(dat = x))

  if (view){
    rstudioapi::viewer(out)
  }
}
