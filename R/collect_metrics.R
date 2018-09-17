collect_metrics <- function(
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

  structure(
    res,
    class = union("rotspot_metrics", class(res)),
    dir = basename(fs::path_real(dir))
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


is_r_file <- function(x){
  grepl("(.*R$)|(.*r$)", x)
}


