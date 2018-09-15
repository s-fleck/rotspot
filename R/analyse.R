analyse_package <- function(
  dir = "."
){

  withr::with_dir(dir, {
    log <- get_log(dir)
    src <- log[is_r_file(entity)]$entity
    loc <- calc_loc(unique(src[file.exists(src)]))
    loc <- data.table(
      entity = names(loc),
      loc = unname(loc)
    )
  })

  res <- merge(log, loc, by = "entity", all = TRUE)
}




is_r_file <- function(x){
  grepl("(.*R$)|(.*r$)", x)
}
