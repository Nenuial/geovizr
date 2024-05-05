#' A shortcut function for the system.file to the geovizr package
#'
#' @inheritDotParams base::system.file
#'
#' @return A path
#'
#' @export
#' @keywords internal
gvz_file <- function(...) {
  system.file(..., package = "geovizr", mustWork = TRUE)
}
