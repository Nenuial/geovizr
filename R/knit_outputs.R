#' Standard pdf document
#'
#' @param metadata Additional pandoc metadata
#' @export
gvz_document <- function(..., metadata = c()) {
  template <- system.file(
    paste0("rmarkdown/templates/Document/resources/document_template.tex"),
    package="geovizr"
  )

  gvz_render(..., template_path = template, metadata = metadata)
}

#' Test pdf
#'
#' @param metadata Additional pandoc metadata
#' @export
gvz_test <- function(..., metadata = c()) {
  template <- system.file(
    paste0("rmarkdown/templates/Test/resources/test_template.tex"),
    package = "geovizr"
  )

  gvz_render(..., template_path = template, metadata = metadata)
}

#' Letter pdf
#'
#' @param metadata Additional pandoc metadata
#' @export
gvz_letter <- function(..., metadata = c()) {
  template <- system.file(
    paste0("rmarkdown/templates/Letter/resources/letter_template.tex"),
    package = "geovizr"
  )

  gvz_render(..., template_path = template, metadata = metadata)
}

#' Render documents
#'
#' @param template_path Path of the latex template
#' @param metadata Vector with pandoc metadata
#' @keywords internal
gvz_render <- function(..., template_path, metadata) {
  project_metadata <- gvz_metadata(fs::path_dir(template_path))
  project_metadata <- c(project_metadata, metadata)

  bookdown::pdf_document2(
    ...,
    template = template_path,
    latex_engine = "xelatex",
    pandoc_args = project_metadata
  )
}

#' Generate pandoc metadata
#'
#' @param path Path of template files
#' @param knit_key Boolean: knit key file?
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
                                   value = paste0(
                                     system.file(path, package="geovizr"),
                                     "/")),
    rmarkdown::pandoc_metadata_arg(name = "csquotes")) -> metadata

  return(metadata)
}
