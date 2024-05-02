#' Plot theme for knitted documents
#'
#' @inheritParams ggeo::ggeotheme
#'
#' @seealso [`ggeo::ggeotheme()`]
#'
#' @return A ggplot2 theme
#' @export
#' @examples
#' cars |>
#'   ggplot2::ggplot(ggplot2::aes(x = speed, y = dist)) +
#'   ggplot2::geom_point() +
#'   ggeo_knit_theme()
#'
ggeo_knit_theme <- function(..., theme = "doc", mode = "light") {
  if(knitr::is_latex_output()) {
    main <- "main_latex"
    plot <- "plot_latex"
  } else if(knitr::is_html_output()) {
    main <- "main_html"
    plot <- "plot_html"
  } else {
    main <- "main_latex"
    plot <- "plot_latex"
    message("This function is only intended to be used with knit output LaTeX or html! Defaulting to LaTeX setup.")
  }

  ggeo::ggeotheme(
    theme = "doc",
    main = main,
    plot = plot,
    mode = mode,
    plot.title.position = "plot",
    ...
  )
}

#' Document ggeo theme
#'
#' @inheritDotParams ggeo::ggeotheme
#'
#' @seealso [`ggeo::ggeotheme()`]
#'
#' @return A ggplot2 theme
#' @export
#' @examples
#' cars |>
#'   ggplot2::ggplot(ggplot2::aes(x = speed, y = dist)) +
#'   ggplot2::geom_point() +
#'   gvz_doc_theme()
gvz_doc_theme <- function(...) {
  if(knitr::is_html_output()) {
    ggeo::ggeotheme(
      theme = "doc", main = "main_latex", plot = "plot_latex",
      plot.background =ggplot2::element_rect(fill = "white", color = NA),
      ...
    )
  } else {
    ggeo::ggeotheme(theme = "doc", main = "main_latex", plot = "plot_latex", ...)
  }
}

#' Theme function for reveal presentations
#'
#' @inheritDotParams ggeo::ggeotheme
#'
#' @return A ggplot2 theme
#' @export
#' @examples
#' cars |>
#'   ggplot2::ggplot(ggplot2::aes(x = speed, y = dist)) +
#'   ggplot2::geom_point() +
#'   gvz_reveal_theme()
#'
gvz_reveal_theme <- function(...) {
  ggeo::ggeotheme(..., main = "main_svg", plot = "plot_svg")
}
