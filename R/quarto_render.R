#' Render Test folders
#'
#' @param students A tibble of students
#' @param template A quarto template
#'
#' @return Generate pdf files
#' @export
gvz_render_test_folders <- function(students, template, output_dir) {
  students |>
    purrr::pwalk(\(...) gvz_walk_test_folders(template, output_dir, ...))
}

#' Walk the folders
#'
#' @param template The template
#' @param ... The parameter for the rest
#'
#' @return Nothing
#' @keywords internal
gvz_walk_test_folders <- function(template, output_dir, ...) {
  student_data <- list(...)

  student_data |>
    purrr::imap(
      \(x, y) rmarkdown::pandoc_metadata_arg(y, x)
    ) |> unname() |> unlist() -> student_metadata

  quarto::quarto_render(
    input = template,
    output_file = paste0(student_data$`student-name`, ".pdf"),
    pandoc_args = student_metadata
  )

  fs::file_move(paste0(student_data$`student-name`, ".pdf"), output_dir)
}
