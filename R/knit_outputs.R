#' Standard pdf document
#'
#' @param metadata Additional pandoc metadata
#' @export
gvz_document <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Document/resources/document_template.tex")

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Book pdf
#'
#' @param metadata Additional pandoc metadata
#' @export
gvz_book <- function(..., metadata = c()) {
  template <- gvz_file("rstudio/templates/project/resources/tex/template.tex")

  gvz_render_pdf_book(..., template_path = template, metadata = metadata)
}

#' Book website
#'
#' @export
gvz_bs4_book <- function(...) {
  fun_args <- list(...)

  fun_args$css <- gvz_file("rstudio/templates/project/resources/css/style.css")
  fun_args$theme$base_font$google$family <- "Fira Sans"
  fun_args$theme$base_font$google$wght <- 300
  fun_args$theme$heading_font$google$family <- "Fira Sans"
  fun_args$theme$heading_font$google$wght <- 500
  fun_args$theme$code_font$google$family <- "Fira Code"

  bs4_book <- bookdown::bs4_book
  rlang::exec("bs4_book", !!!fun_args)
}

#' Test pdf
#'
#' @param metadata Additional pandoc metadata
#' @export
gvz_test <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Test/resources/test_template.tex")

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Test with cover page pdf
#'
#' @param metadata Additional pandoc metadata
#' @export
gvz_test_folder <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Test_folder/resources/test_folder_template.tex")

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Ski camp pdf document
#'
#' @param metadata Additional pandoc metadata
#' @export
gvz_ski <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Ski_camp/resources/ski_camp_template.tex")

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Letter pdf
#'
#' @param metadata Additional pandoc metadata
#' @export
gvz_letter <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Letter/resources/letter_template.tex")

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Oraux matu fédérale
#'
#' @param metadata Additional pandoc metadata
#' @export
gvz_matu_oraux <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Matu_oraux/resources/matu_oraux_template.tex")

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Render pdf documents
#'
#' @param template_path Path of the latex template
#' @param metadata Vector with pandoc metadata
#' @keywords internal
gvz_render_pdf_document <- function(..., template_path, metadata) {
  project_metadata <- gvz_metadata(fs::path_dir(template_path))
  project_metadata <- c(project_metadata, metadata)

  bookdown::pdf_document2(
    ...,
    template = template_path,
    latex_engine = "xelatex",
    citation_package = "biblatex",
    pandoc_args = project_metadata
  )
}

#' Render pdf books
#'
#' @param template_path Path of the latex template
#' @param metadata Vector with pandoc metadata
#' @keywords internal
gvz_render_pdf_book <- function(..., template_path, metadata) {
  project_metadata <- gvz_metadata(fs::path_dir(template_path))
  project_metadata <- c(project_metadata, metadata)

  bookdown::pdf_book(
    ...,
    template = template_path,
    latex_engine = "xelatex",
    citation_package = "biblatex",
    pandoc_args = project_metadata
  )
}

#' Generate pandoc metadata
#'
#' @param path Path of template files
#'
#' @return A vector with pandoc metadata arguments
#' @keywords internal
gvz_metadata <- function(path) {
  metadata <- c()

  if(fs::file_exists(here::here("_document.yaml"))) {
    yaml::read_yaml(here::here("_document.yaml")) %>%
      purrr::imap(~rmarkdown::pandoc_metadata_arg(
        name = .y,
        value = .x
      )) %>% purrr::as_vector() %>% unname() -> metadata
  }

  c(metadata,
    rmarkdown::pandoc_metadata_arg(name = "globalpath", value = tex_global_path()),
    rmarkdown::pandoc_metadata_arg(name = "templatepath",
                                   value = paste0(path, "/")),
    rmarkdown::pandoc_metadata_arg(name = "csquotes")) -> metadata

  return(metadata)
}
