#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' A shortcut function for the system.file to the geovizr package
#'
#' @return A path
#'
#' @export
gvz_file = function(...) {
  system.file(..., package = 'geovizr', mustWork = TRUE)
}
