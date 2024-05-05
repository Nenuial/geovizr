#' Install book files
#'
#' @param path The path to intall the ressources to
#'
#' @export
#' @return TRUE
#' @examplesIf interactive()
#' book_skeleton("./")
#'
book_skeleton <- function(path) {
  # ensure directory exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # copy 'resources' folder to path
  resources <- gvz_file("rstudio", "templates", "project", "skeleton")

  fs::dir_ls(resources, type = "directory") %>%
    purrr::walk(
      .f = ~ fs::dir_copy(path = .x, new_path = path)
    )

  fs::dir_ls(resources, type = "file") %>%
    purrr::walk(
      .f = ~ fs::dir_copy(path = .x, new_path = path)
    )

  # add book_filename to _bookdown.yml and default to the base path name
  f <- file.path(path, "book", "_bookdown.yml")
  x <- xfun::read_utf8(f)
  xfun::write_utf8(c(sprintf('book_filename: "%s"', basename(path)), x), f)

  TRUE
}
