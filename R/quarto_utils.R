#' Render multiple files
#'
#' @param template A quarto template
#' @param data A tibble
#'   Each row will generate a new document, the data in the columns
#'   is passed as variables to be used in the template.
#' @param output_dir The directory to render the files to
#' @param merge If a string, merges the multiple document in document
#'   with the corresponding name. Default is false.
#'
#' @return Generate pdf files
#' @export
#' @examplesIf interactive()
#' # Read a file containing the data used in the template
#' readr::read_csv("data_file.csv") |>
#'   gvz_render_multiple("template.qmd", "./")
#'
gvz_render_multiple <- function(data, template, output_dir, merge = FALSE) {
  data |>
    purrr::pmap(\(...) gvz_walk_multiple(template, output_dir, ...)) -> files

  if (merge) {
    pdftools::pdf_combine(files, output = fs::path(output_dir, merge, ext = "pdf"))
  }
}

#' Walk the folders
#'
#' @param template The template
#' @param ... The parameter for the rest
#'
#' @return NULL
#' @keywords internal
gvz_walk_multiple <- function(template, output_dir, ...) {
  data <- list(...)

  data |>
    purrr::imap(
      \(x, y) rmarkdown::pandoc_metadata_arg(y, x)
    ) |>
    unname() |>
    unlist() -> metadata

  quarto::quarto_render(
    input = template,
    output_file = paste0(data$`file`, ".pdf"),
    pandoc_args = metadata
  )

  return(fs::file_move(paste0(data$`file`, ".pdf"), output_dir))
}

#' Quarto chunk options setup
#'
#' Set useful chunk options in Quarto documents.
#'
#' @return NULL
#' @export
#' @examples
#' gvz_quarto_setup()
#'
gvz_quarto_setup <- function() {
  knitr::opts_chunk$set(dev.args = c(bg = "transparent"))
}
