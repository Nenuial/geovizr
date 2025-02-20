#' Knit child for MF oral exams
#'
#' Knits the child using `cat()` to
#' print the output.
#'
#' This function should be used in a Quarto document inside
#' a code chunk with the `asis` option.
#'
#' @param input Input file
#' @param ... Data that is passed to the fragment
#'   Must have *ID*, *Name*, *Subject* and *Question*
#'
#' @return NULL
#' @export
gvz_knit_child <- function(input, ...) {
  out <- rlang::list2(...)
  data <- tibble::tibble(!!!out)

  child <- knitr::knit_child(
    input = input,
    quiet = TRUE,
    envir = environment()
  )

  cat(child)
}

#' Knit child for MF oral exams
#'
#' Knits the child for the Matu oral exams
#' using `cat()` to print the output.
#'
#' @param ... Data that is passed to the fragment
#'   Must have *ID*, *Name*, *Subject* and *Question*
#'
#' @return NULL
#' @export
gvz_knit_child_matu_oraux <- function(...) {
  out <- rlang::list2(...)

  gvz_knit_child(
    input = system.file("rmarkdown/templates/Matu_oraux/resources/matu_oraux_fragment.Rmd",
      package = "geovizr"
    ),
    !!!out
  )
}
