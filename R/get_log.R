get_log <- function(
  indir = "."
){
  oldwd <- getwd()
  setwd(indir)
  on.exit(setwd(oldwd))
  log <- system2(
    "git",
    "log --pretty=format:'%ad %h [%an]  %s' --date=short --numstat",
    stdout = TRUE
  )

  res <- data.table(
    entity = vector("character", length(log)),
    date = as.Date(NA),
    lines_added = NA_integer_,
    lines_deleted = NA_integer_
  )

  i_out = 1L

  for (i_in in seq_along(log)){
    if (log[[i_in]] == ""){
      next

    } else if (grepl("[", log[[i_in]], fixed = TRUE)){
      header <- list(
        date = as.Date(substr(log[[i_in]], 1, 10)),
        hash = substr(log[[i_in]], 12, 18)
      )

    } else {
      line <- strsplit(log[[i_in]], "\t", fixed = TRUE)[[1]]
      data.table::set(res, i = i_out, "date", header$date)
      data.table::set(res, i = i_out, "lines_added",   if (line[[1]] == "-") NA_integer_ else as.integer(line[[1]]) )
      data.table::set(res, i = i_out, "lines_deleted", if (line[[2]] == "-") NA_integer_ else as.integer(line[[2]]) )
      data.table::set(res, i = i_out, "entity",  line[[3]])
      i_out <- i_out + 1L
    }
  }

  structure(
    res[1:(i_out - 1L)],
    class = c("git_log", "data.table", "data.frame")
  )
}




summary.git_log <- function(x, exclude_man = TRUE, ...){
  res <- x[grep("^man", entity, invert = TRUE), .(
    lines_added = sum(lines_added),
    lines_deleted  = sum(lines_deleted)
  ), by = "entity"]

  res[, lines_mod := lines_added + lines_deleted]

  data.table::setorderv(res, "lines_mod", order = -1L)
  print(res)
}
