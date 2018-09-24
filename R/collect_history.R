collect_history <- function(
  dir = "."
){
  withr::with_dir(
    dir,
    log <- system2(
      "git",
      "log --pretty=format:'{{header}}\t%ad\t%h\t%an\t%s' --date=short --numstat",
      stdout = TRUE
    )
  )
  
  if (length(log) == 0)  stop("No git directory found")
  
  res <- data.table(
    date = as.Date(NA),
    hash = vector("character", length(log)),
    author = NA_character_,
    entity = NA_character_,
    lines_added = NA_integer_,
    lines_deleted = NA_integer_
  )

  i_out = 1L

  for (i_in in seq_along(log)){
    if (log[[i_in]] == ""){
      next

    } else if (grepl("{{header}}\t", log[[i_in]], fixed = TRUE)){
      header <- parse_commit_header(log[[i_in]])

    } else {
      line <- strsplit(log[[i_in]], "\t", fixed = TRUE)[[1]]
      data.table::set(res, i = i_out, "date", header$date)
      data.table::set(res, i = i_out, "hash", header$hash)
      data.table::set(res, i = i_out, "author", header$author)
      data.table::set(res, i = i_out, "lines_added",   if (line[[1]] == "-") NA_integer_ else as.integer(line[[1]]) )
      data.table::set(res, i = i_out, "lines_deleted", if (line[[2]] == "-") NA_integer_ else as.integer(line[[2]]) )
      data.table::set(res, i = i_out, "entity",  line[[3]])
      i_out <- i_out + 1L
    }
  }

  res <- structure(
    res[1:(i_out - 1L)],
    class = c("git_log_numstats", "data.table", "data.frame")
  )

  data.table::setkeyv(res, c("date", "hash"))

  res
}




summary.git_log_numstats <- function(x, exclude_man = TRUE, ...){
  res <- x[grep("^man", entity, invert = TRUE), .(
    lines_added = sum(lines_added),
    lines_deleted  = sum(lines_deleted)
  ), by = "entity"]

  res[, lines_mod := lines_added + lines_deleted]

  data.table::setorderv(res, "lines_mod", order = -1L)
  print(res)
}



parse_commit_header <- function(x){
  x <- strsplit(x, "\t", fixed = TRUE)[[1]]

  list(
    date = as.Date(x[[2]]),
    hash = x[[3]],
    author = x[[4]]
  )
}
