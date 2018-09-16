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
  res
}




is_r_file <- function(x){
  grepl("(.*R$)|(.*r$)", x)
}


