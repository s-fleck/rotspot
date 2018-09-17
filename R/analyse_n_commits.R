analyze_commits_x_author <- function(
  x
){
  x[, .(commits = length(unique(hash))), by = c("date", "author")]


}



plot_commits_x_author <- function(
  x,
  n = 10L
){
  dd <- analyze_commits_x_author(x)
  sel_authors <- head(dd[, sum(commits), by = "author"], n)$author

  dd <- dd[author %in% sel_authors]

  ggplot2::ggplot(
    dd,
    ggplot2::aes(
      x = date,
      y = commits,
      color = author,
      group = author
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~author, scales = "free_y")



}
