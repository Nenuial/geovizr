#' Knit child for MF oral exams
#'
#' @param ...
#'
#' @return Knitted document
#' @export
gvz_knit_child_matu_oraux <- function(...) {
  data <- tibble::tibble(...)

  child <- knitr::knit_child(
    input = system.file("rmarkdown/templates/Matu_oraux/resources/matu_oraux_fragment.Rmd",
                        package = "geovizr"),
    quiet = TRUE,
    envir = environment()
  )

  cat(child)
}
