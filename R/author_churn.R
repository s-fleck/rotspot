plot_author_churn <- function(
  x
){
  sel_authors <- x[, .(churn = sum(loc, na.rm = TRUE)), by = "author"]
  data.table::setkeyv(sel_authors, "churn")
  sel_authors <- tail(sel_authors, 10)$author

  ggplot2::ggplot(
    x[author %in% sel_authors],
    ggplot2::aes(
      x = date,
      y = lines_added,
      group = author,
      color = author
    )
  ) + ggplot2::geom_line()


}
