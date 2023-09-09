#' Global chunk options
#'
#' @export
gvz_global_opts_chunk <- function() {
  knitr::opts_chunk$set(warning = FALSE)
  knitr::opts_chunk$set(message = FALSE)
  knitr::opts_chunk$set(fig.retina = 2)
  knitr::opts_chunk$set(dev.args = c(bg = 'transparent'))

  knitr::opts_template$set(
    fig = list(echo = FALSE, out.width = '100%'),
    fig.eighty = list(echo = FALSE, out.width = '80%'),
    fig.half = list(echo = FALSE, out.width = '50%'),
    table = list(echo = FALSE),
    geo.graph = list(
      echo = FALSE, message = FALSE, out.width = '100%',
      fig.retina = 2,
      fig.ext = if (knitr:::is_latex_output()) 'pdf' else 'png',
      dev = if (knitr:::is_latex_output()) 'cairo_pdf' else 'png'
    ),
    geo.tikz = list(
      echo = FALSE, out.width='100%',
      fig.retina = 2,
      fig.ext=if (knitr:::is_latex_output()) 'pdf' else 'png',
      engine='tikz',
      engine.opts = list(
        template = geovizr::tikz(),
        classoption = geotools::gtl_opt_long_language(),
        extra.preamble = paste0(r"[\graphicspath{{]", geovizr::gvz_book_resources(), "/}}")
      )
    ),
    geo.cosmo.box = list(
      echo = FALSE,
      results = 'asis'
    ),
    geo.graph = list(
      echo=FALSE,
      fig.width=17.5,
      fig.height=8,
      out.width="100%"
    )
  )

  chunkhooks::hook_figure_unit(unit = "cm")
}

#' Quarto chunk options setup
#'
#' @export
gvz_quarto_setup <- function() {
  knitr::opts_chunk$set(dev.args = c(bg = 'transparent'))
}

#' Get the path for the book resources
#'
#' @return A full path with the book resources, defaults to project root
#' @export
gvz_book_resources <- function() {
  here::here(geotools::gtl_options("book_resources"))
}

#' Render all tikz diagram in PDF format
#'
#' @param source Source file with tikz chunks
#' @param out Path for diagram's to export to
#'
#' @export
gvz_render_diagrams <- function(source, out) {
  knitr::read_chunk(source)

  knitr::all_labels() |>
    purrr::walk(~ gvz_tikz_diagram(.x, out))
}

#' Generate tikz diagram
#'
#' @param label Tikz chunk label
#' @param out Path for figure
#'
#' @return
#' @keywords internal
gvz_tikz_diagram <- function(label, out) {
  tikz_options <- list()
  tikz_options$label <- label
  tikz_options$code <- knitr::knit_code$get(label)
  tikz_options$echo <- TRUE
  tikz_options$eval <- TRUE
  tikz_options$out.width <- '100%'
  tikz_options$fig.retina <- 2
  tikz_options$fig.ext <- 'pdf'
  tikz_options$fig.path <- out
  tikz_options$engine.opts <- list()
  tikz_options$engine.opts$template <- geovizr::tikz()
  tikz_options$engine.opts$classoption <- geotools::gtl_opt_long_language()
  tikz_options$engine.opts$extra.preamble <- paste0(r"[\graphicspath{{]", geovizr::gvz_book_resources(), "/}}")
  knitr:::eng_tikz(tikz_options)
}

#' Helper to convert from Markdown to LaTeX
#'
#' @param content String to convert
#'
#' @return A string in LaTeX
#' @export
gvz_md_to_latex <- function(content) {
  knitr:::pandoc_fragment(text = content, from = "markdown", to = "latex")
}

#' Generate latex code for table from tibble
#'
#' @param tbl A tibble
#' @param md_cols Columns to treat as markdown
#'
#' @return A collapsed string to use `asis`
#' @export
gvz_latex_table <- function(tbl, md_cols = c()) {
  tbl |>
    # Ensure all columns are character based and replace NAs with empty strings
    dplyr::mutate(dplyr::across(where(is.numeric), as.character)) |>
    dplyr::mutate(dplyr::across(.fns = ~tidyr::replace_na(.x, ""))) |>

    # Mutate the "formated" columns (goes through pandoc and replaces \\ with \newline)
    dplyr::mutate(dplyr::across(dplyr::any_of(md_cols), ~stringr::str_replace_all(.x, "\n", "  \n"))) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(md_cols), ~purrr::map_chr(.x, geovizr::gvz_md_to_latex))) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(md_cols), ~stringr::str_replace_all(.x, r"(\\\\\n)", r"(\\newline )"))) |>

    # Collapse the whole thing
    dplyr::rowwise() |>
    dplyr::transmute(text = stringr:::str_c(dplyr::c_across(), collapse = " & ")) |>
    dplyr::mutate(text = glue::glue("{text}\\\\")) |>
    dplyr::pull(text) |>
    stringr::str_c(collapse = "\n") |>

    # Ensure raw latex protection
    knitr::raw_latex()
}

#' Custom Knit function for RStudio
#'
#' @export
knit_quiet <- function(input, ...) {
  rmarkdown::render(
    input,
    quiet = TRUE,
    clean = TRUE,
    envir = globalenv()
  )

  yaml <- rmarkdown::yaml_front_matter(input)

  if (!is.null(yaml$subfile)) {
    yaml$subfile %>%
      purrr::iwalk(
        .f = ~ knit_subfile(input = input, output = yaml[["output"]], key = .y, value = .x)
      )
  }

  knit_cleanup(input)
}

#' Custom knit function for multiple letters
#'
#' @export
knit_letters <- function(input, ...) {
  yaml <- rmarkdown::yaml_front_matter(input)

  if (length(yaml[["recipients"]]) == 0) stop("No letter recipients defined")

  render_letter <- function(recipient = .x, input) {
    recipient %>%
      purrr::imap(~rmarkdown::pandoc_metadata_arg(
        name = .y,
        value = .x
      )) %>% purrr::as_vector() %>% unname() -> metadata

    rmarkdown::render(
      input = input,
      output_options = list(metadata = metadata),
      output_file = paste0(
        fs::path_ext_remove(input),
        "_", janitor::make_clean_names(
          paste0(recipient[["last"]], "_", recipient[["address"]])
        )
      ),
      quiet = TRUE,
      clean = TRUE,
      envir = globalenv()
    )
  }

  yaml[["recipients"]] %>%
    purrr::walk(~ render_letter(recipient = .x, input = input))

  knit_cleanup(input)
}

#' Knit subfiles
#'
#' @param input The input file
#' @param keyword The subfile head as a named vector
#' @keywords internal
knit_subfile <- function(input, output, key, value) {
  output_args <- list(
    metadata = rmarkdown::pandoc_metadata_arg(
      name = "subclass",
      value = value
    )
  )

  rmarkdown::render(
    input = input,
    output_options = output_args,
    output_file = paste0(
      fs::path_ext_remove(input),
      "_", key
    ),
    quiet = TRUE,
    clean = TRUE,
    envir = globalenv()
  )
}

#' Remove all temporary files
#'
#' @param path A folder to check for files
#' @keywords internal
knit_cleanup <- function(input) {
  fs::path_dir(path = input) %>%
    fs::dir_ls(regexp = "[.](log|ent|tns)") %>%
    fs::file_delete()
}

#' Knit all Rmd files in the Project
#'
#' @param path The path to start at
#'
#' @export
knit_all <- function(path = here::here()) {
  fs::dir_ls(path = path, recurse = TRUE, regexp = ".*\\.Rmd") %>%
    purrr::walk(~geovizr::knit_quiet(.x, encoding = "UTF-8"))
}

#' Return full path to geovizr ressources
#'
#' @return A string with the full path
#' @keywords internal
tex_global_path <- function() {
  paste0(system.file("rmarkdown/resources", package = "geovizr"), "/")
}

#' Return latex sizes
#'
#' @param x The size in percents
#' @param which One of `width` or `height`
#' @param width The width measure to use
#'
#' @return Latex size
#' @keywords internal
latex_percent_size <- function(x, which = c("width", "height"),
                               width = "\\linewidth") {
  if (!is.character(x))
    return(x)
  i <- grep("^[0-9.]+%$", x)
  if (length(i) == 0)
    return(x)
  xi <- as.numeric(sub("%$", "", x[i]))
  if (any(is.na(xi)))
    return(x)
  which <- match.arg(which)
  x[i] <- paste0(formatC(xi / 100, decimal.mark = "."),
                if (which == "width") width else "\\textheight")
  x
}

#' Get the path for the tikz template stub
#'
#' @return A full path to the tikz template stub
#' @export
tikz <- function() {
  gvz_file("rstudio/templates/project/resources/tex/tikz.tex")
}

#' Set chapter image in for LaTeX book
#'
#' @param path The path to the image to use
#'
#' @export
chapter_image <- function(path) {
  if(!knitr::is_latex_output()) return()

  knitr::raw_latex(paste0(
    r"(\chapterimage{)",
    path,
    "}"
  ))
}
