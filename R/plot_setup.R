#' Plot theme for knitted documents
#'
#' @inheritParams ggplot2::theme_minimal
#' @param theme Override [ggeo::ggeotheme()] theme parameter
#' @param mode Override [ggeo::ggeotheme()] mode parameter
#'
#' @seealso [ggeo::ggeotheme()]
#'
#' @return A ggplot2 theme
#' @export
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
    warning("This function is only intended to be used with knit output LaTeX or html! Defaulting to LaTeX setup.")
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
#' @seealso [ggeo::ggeotheme()]
#'
#' @return A ggplot2 theme
#' @export
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
#' @param ... See [ggeo::ggeotheme()]
#'
#' @return An object of class [ggplot2::theme()].
#' @export
gvz_reveal_theme <- function(...) {
  ggeo::ggeotheme(..., main = "main_svg", plot = "plot_svg")
}
