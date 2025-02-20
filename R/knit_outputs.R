#' Standard pdf document
#'
#' @inheritDotParams gvz_render_pdf_document
#' @param metadata Additional pandoc metadata
#'
#' @export
#' @examplesIf interactive()
#' # Not run: should be used as Rmd format in yaml frontmatter
#' #
#' # format: geovizir::gvz_document
#'
gvz_document <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Document/resources/document_template.tex")

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Book pdf
#'
#' @inheritDotParams gvz_render_pdf_book
#' @param metadata Additional pandoc metadata
#'
#' @export
#' @examplesIf interactive()
#' # Not run: should be used as Rmd format in yaml frontmatter
#' #
#' # format: geovizir::gvz_book
#'
gvz_book <- function(..., metadata = c()) {
  template <- gvz_file("rstudio/templates/project/resources/tex/template.tex")

  gvz_render_pdf_book(..., template_path = template, metadata = metadata)
}

#' Book website
#'
#' @inheritDotParams bookdown::bs4_book
#'
#' @export
#' @examplesIf interactive()
#' # Not run: should be used as Rmd format in yaml frontmatter
#' #
#' # format: geovizir::gvz_bs4_book
#'
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
#' @inheritDotParams gvz_render_pdf_document
#' @param metadata Additional pandoc metadata
#'
#' @export
#' @examplesIf interactive()
#' # Not run: should be used as Rmd format in yaml frontmatter
#' #
#' # format: geovizir::gvz_test
#'
gvz_test <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Test/resources/test_template.tex")

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Test with cover page pdf
#'
#' @inheritDotParams gvz_render_pdf_document
#' @param metadata Additional pandoc metadata
#'
#' @export
#' @examplesIf interactive()
#' # Not run: should be used as Rmd format in yaml frontmatter
#' #
#' # format: geovizir::gvz_test_folder
#'
gvz_test_folder <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Test_folder/resources/test_folder_template.tex")

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Ski camp pdf document
#'
#' @inheritDotParams gvz_render_pdf_document
#' @param metadata Additional pandoc metadata
#'
#' @export
#' @examplesIf interactive()
#' # Not run: should be used as Rmd format in yaml frontmatter
#' #
#' # format: geovizir::gvz_ski
#'
gvz_ski <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Ski_camp/resources/ski_camp_template.tex")

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' LDDR pdf letter
#'
#' @inheritDotParams gvz_letter_standard
#' @param metadata Additional pandoc metadata
#'
#' @export
#' @examplesIf interactive()
#' # Not run: should be used as Rmd format in yaml frontmatter
#' #
#' # format: geovizir::gvz_letter
gvz_letter <- function(..., metadata = c()) {
  metadata <- c(
    metadata,
    rmarkdown::pandoc_metadata_arg(
      name = "logo",
      value = gvz_file("rmarkdown/resources/images/LDDR_blue.pdf")
    ),
    rmarkdown::pandoc_metadata_arg(
      name = "logo-size",
      value = "width=3.5cm"
    ),
    rmarkdown::pandoc_metadata_arg(
      name = "author",
      value = "Pascal Burkhard"
    ),
    rmarkdown::pandoc_metadata_arg(
      name = "return-email",
      value = "pascal.burkhard@rpn.ch"
    ),
    rmarkdown::pandoc_metadata_arg(
      name = "return-url",
      value = "www.lddr.ch"
    )
  )

  gvz_letter_standard(..., metadata = metadata)
}

#' Standard pdf letter
#'
#' @inheritDotParams inherit_pdf_document
#' @param metadata Additional pandoc metadata
#'
#' @keywords internal
gvz_letter_standard <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Letter/resources/letter_template.tex")

  lco_default <- gvz_file("rmarkdown/templates/Letter/resources/swiss.lco")
  lco_default <- sub("\\.[^.]*$", "", lco_default)

  metadata <- c(
    metadata,
    rmarkdown::pandoc_metadata_arg(name = "csquotes"),
    rmarkdown::pandoc_metadata_arg(
      name = "lco_default",
      value = lco_default
    ),
    rmarkdown::pandoc_metadata_arg(
      name = "papersize",
      value = "a4"
    )
  )

  base <- inherit_pdf_document(...,
    template = template,
    latex_engine = "xelatex",
    md_extensions = c("-autolink_bare_uris"),
    pandoc_args = metadata
  )

  base
}

#' Rmarkdown pdf document
#'
#' Call rmarkdown::pdf_document and mark the return
#' value as inheriting pdf_document
#'
#' @inheritDotParams rmarkdown::pdf_document
#' @keywords internal
inherit_pdf_document <- function(...) {
  fmt <- rmarkdown::pdf_document(...)
  fmt$inherits <- "pdf_document"

  fmt
}

#' Examen matu LDDR
#'
#' @inheritDotParams gvz_render_pdf_document
#' @param metadata Additional pandoc metadata
#'
#' @export
#' @examplesIf interactive()
#' # Not run: should be used as Rmd format in yaml frontmatter
#' #
#' # format: geovizir::gvz_matu
#'
gvz_matu <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Matu_lddr/resources/matu_lddr_template.tex")

  metadata <- c(
    metadata,
    rmarkdown::pandoc_lua_filter_args(
      gvz_file("rmarkdown/templates/Matu_lddr/resources/matu_lddr.lua")
    )
  )

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Oraux matu fédérale
#'
#' @inheritDotParams gvz_render_pdf_document
#' @param metadata Additional pandoc metadata
#'
#' @export
#' @examplesIf interactive()
#' # Not run: should be used as Rmd format in yaml frontmatter
#' #
#' # format: geovizir::gvz_matu_oraux
#'
gvz_matu_oraux <- function(..., metadata = c()) {
  template <- gvz_file("rmarkdown/templates/Matu_oraux/resources/matu_oraux_template.tex")

  gvz_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Render pdf documents
#'
#' @inheritDotParams bookdown::pdf_document2
#' @param template_path Path of the latex template
#' @param metadata Vector with pandoc metadata
#'
#' @keywords internal
gvz_render_pdf_document <- function(..., template_path, metadata) {
  project_metadata <- gvz_metadata(fs::path_dir(template_path))
  project_metadata <- c(
    project_metadata,
    metadata,
    rmarkdown::pandoc_lua_filter_args(gvz_file("rmarkdown/lua/global.lua"))
  )

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
#' @inheritDotParams bookdown::pdf_book
#' @param template_path Path of the latex template
#' @param metadata Vector with pandoc metadata
#'
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

  if (fs::file_exists(here::here("_document.yaml"))) {
    yaml::read_yaml(here::here("_document.yaml")) %>%
      purrr::imap(~ rmarkdown::pandoc_metadata_arg(
        name = .y,
        value = .x
      )) %>%
      purrr::as_vector() %>%
      unname() -> metadata
  }

  c(
    metadata,
    rmarkdown::pandoc_metadata_arg(name = "globalpath", value = tex_global_path()),
    rmarkdown::pandoc_metadata_arg(
      name = "templatepath",
      value = paste0(path, "/")
    ),
    rmarkdown::pandoc_metadata_arg(name = "csquotes")
  ) -> metadata

  metadata
}
