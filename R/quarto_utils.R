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

  fs::file_move(paste0(data$`file`, ".pdf"), output_dir)
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


#' Make a simple yaml file for all presentations
#'
#' @returns NULL
#' @export
#'
#' @examplesIf interactive()
#' # Not run: only work in quarto projects
#'
#' gvz_quarto_make_presentation_yaml()
#'
gvz_quarto_make_presentation_yaml <- function() {
  get_title <- function(path) {
    title <- rmarkdown::yaml_front_matter(path)$title
    url <- fs::path(fs::path_ext_remove(path), ext = "html")
    tibble::tibble(url = url, title = title)
  }

  fs::dir_ls(".", recurse = TRUE, regexp = "\\d{1,2}-.*?/index.qmd") |>
    purrr::map(get_title) |>
    purrr::list_rbind() |>
    jsonlite::toJSON() |>
    jsonlite::parse_json() |>
    yaml::as.yaml() |>
    readr::write_file("presentations.yml")
}

#' Gets presentation yaml file from GeoKey
#'
#' @param path Path where the yaml file gets downloaded to
#' @param prefix Prefix for the path on GeoKey
#'
#' @returns NULL
#' @export
#'
#' @examplesIf interactive()
#' # Not run: only work in quarto projects
#'
#' gvz_quarto_get_presentation_yaml("Presentations/", "DF")
gvz_quarto_get_presentation_yaml <- function(path, prefix) {
  download.file(
    paste0("http://key.geoviews.ch/", prefix, "-", path, "/presentations.yml"),
    paste0(path, "/Presentations/presentations.yml")
  )
}
