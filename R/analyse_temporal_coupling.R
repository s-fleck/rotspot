analyse_temporal_coupling <- function(
  x
){
  x

  y <- x[, .(entity = unique(entity)), by = "hash"]
  res <- merge(y, y, by = "hash", allow.cartesian = TRUE)

  res <- res[entity.x != entity.y]

  res <- res[, .(shared_commits = .N), by = c("entity.x", "entity.y")]
  data.table::setorderv(res, "shared_commits", order = -1L)
  res[is_r_file(entity.x) & is_r_file(entity.y)]





}



plot_temporal_coupling <- function(
  x
){

  sel_entities <- unique(x, by = "entity")[loc %in% sort(loc)[1:10]]$entity

  ggplot2::ggplot(
    x[entity %in% sel_entities],
    ggplot2::aes(
      x = date,
      y = entity,
      height = lines_added
    )
  ) + ggridges::geom_ridgeline()


}


