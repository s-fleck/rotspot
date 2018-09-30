#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_rotspot_metrics <- function(
  x
){
  x <- data.table::copy(x)
  data.table::setattr(x, "class", union("rotspot_metrics", class(x) ))
  x
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
summary.rotspot_metrics <- function(
  x
){
  res <- list()
  dd <- data.table::copy(x)
  dd[, language := tolower(tools::file_ext(entity))]

  res$authors <- x[, .(commits = n_distinct(hash)), by = "author"]
  data.table::setorderv(res$authors, "commits", order = -1L)

  res$pkgs <- x[, .(commits = n_distinct(hash)), by = "pkg"]
  data.table::setorderv(res$pkgs, "commits", order = -1L)

  res$languages <- dd[, .(commits = n_distinct(hash)), by = "language"]
  data.table::setorderv(res$languages, "commits", order = -1L)

  res$dates <- x[, .(commits = n_distinct(hash)), by = "date"]
  data.table::setorderv(res$dates, "commits", order = -1L)

  structure(
    res,
    class = c("rotspot_metrics_summary", "list")
  )
}



print.rotspot_metrics_summary <- function(
  x,
  n = 6L
){
  assert(is_scalar_integerish(n) || (is.null(n)))
  xf <- list()
  dd <- data.table::copy(x)

  pad_matrix <- function(x){
    x[, 1] <- stringi::stri_pad_right(x[, 1], max(nchar(x[, 1])) + 2)
    x[, 2] <- stringi::stri_pad_left(x[, 2],  max(nchar(x[, 2])) + 2 )
    x
  }

  if (!is.null(n)){

    ol <- function(.x) paste("...", n_distinct(.x) - n, "more ...")

    dd$pkgs[,
      pkg := forcats::fct_lump(pkg, n = n, w = commits, other_level = ol(pkg))
    ]
    dd$authors[,
      author := forcats::fct_lump(author, n = n, w = commits, other_level = ol(author))
    ]
    dd$languages[,
      language := forcats::fct_lump(language, n = n, w = commits, other_level = ol(language))
    ]

    dd$pkgs <- dd$pkgs[, .(commits = sum(commits)), by = "pkg"]
    dd$authors <- dd$authors[, .(commits = sum(commits)), by = "author"]
    dd$languages <- dd$languages[, .(commits = sum(commits)), by = "language"]
  }

  xf$pkgs <- as.matrix(dd$pkgs)
  xf$pkgs <- rbind(c("Package", "Commits"), xf$pkgs)
  xf$pkgs <- pad_matrix(xf$pkgs)

  xf$authors <- as.matrix(dd$authors)
  xf$authors <- rbind(c("Author", "Commits"), xf$authors)
  xf$authors <- pad_matrix(xf$authors)

  xf$languages <- as.matrix(dd$languages)
  xf$languages <- rbind(c("Language", "Commits"), xf$languages)
  xf$languages <- pad_matrix(xf$languages)

  vspace <- paste(rep(" ", 6), collapse = "")

  xf <- cbind(xf$pkgs, vspace, xf$authors, vspace, xf$languages, "\n")
  xf <- apply(xf, 1, paste, collapse = "")

  cat(
    sprintf(
      "%s R-Packages by %s contributors [%s - %s]",
      n_distinct(x$pkgs$pkg),
      n_distinct(x$authors$author),
      min(x$dates$date),
      max(x$dates$date)
    ),

    "\n\n"
  )

  cat(xf, sep = "")

  invisible(x)
}



plot.summary_rotspot_metrics <- function(
  x,
  ...
){
  dd <- lapply(
    names(x),
    function(nm)  data.table(
      variable = nm,
      value    = as.character(x[[nm]][[1]]),
      commits   = x[[nm]][[2]]
    )
  ) %>%
    data.table::rbindlist()

  ggplot(
    dd,
    aes(
      x = value,
      y = commits
    )
  ) +
    geom_col() +
    facet_grid(variable ~ ., scales = "free")
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
plot.rotspot_metrics <- function(
  x,
  n = 6
){
  assert(is_scalar_integerish(n) || is.null(n))
  dd <- x[, .(commits = length(unique(hash))), by = c("date", "pkg")]

  order <- dd[, .(commits = sum(commits)), by = "pkg"]
  data.table::setkeyv(order, "commits")
  dd[, pkg := factor(pkg, levels = rev(order$pkg))]

  if (!is.null(n)){
    ol <- function(.x) paste(n_distinct(.x) - n, "others")
    dd[, pkg := forcats::fct_lump(pkg, n = n, w = commits, other_level = ol(pkg))]
    dd <- dd[, .(commits = sum(commits)), by = c("date", "pkg")]
  }

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
