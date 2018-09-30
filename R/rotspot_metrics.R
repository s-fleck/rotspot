as_rotspot_metrics <- function(
  x
){
  x <- data.table::copy(x)
  data.table::setattr(x, "class", union("rotspot_metrics", class(x) ))
  x
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
    ggplot2::geom_bar(stat = "identity", color = "#FDE725FF", fill = "#FDE725FF") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::theme_dark() +
    ggplot2::facet_grid(pkg ~ .)
}

