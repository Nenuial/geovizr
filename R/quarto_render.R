#' Render multiple files
#'
#' @param students A tibble of values for each file
#' @param template A quarto template
#'
#' @return Generate pdf files
#' @export
gvz_render_multiple <- function(data, template, output_dir, merge = F) {
  data |>
    purrr::pmap(\(...) gvz_walk_multiple(template, output_dir, ...)) -> files

  if(merge) {
    pdftools::pdf_combine(files, output = fs::path(output_dir, merge, ext = "pdf"))
  }
}

#' Walk the folders
#'
#' @param template The template
#' @param ... The parameter for the rest
#'
#' @return Nothing
#' @keywords internal
gvz_walk_multiple <- function(template, output_dir, ...) {
  data <- list(...)

  data |>
    purrr::imap(
      \(x, y) rmarkdown::pandoc_metadata_arg(y, x)
    ) |> unname() |> unlist() -> metadata

  quarto::quarto_render(
    input = template,
    output_file = paste0(data$`file`, ".pdf"),
    pandoc_args = metadata
  )

  return(fs::file_move(paste0(data$`file`, ".pdf"), output_dir))
}
