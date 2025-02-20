# General purpose ---------------------------------------------------------

#' Raw LaTeX
#'
#' @param options Chunk options
#'   Only the code part is used
#'
#' @export
#' @examplesIf interactive()
#' # Not run: knitr engine for Rmd/Quarto documents
#' eng_latex_raw(list(code = "Some text with \\emph{emphasis}"))
eng_latex_raw <- function(options) {
  options$code %>%
    knitr::raw_latex()
}


# Tikz --------------------------------------------------------------------

eng_xetikz <- function(options) {
  if (!options$eval) {
    return(engine_output(options, options$code, ""))
  }

  lines <- xfun::read_utf8(
    options$engine.opts$template %||% system.file("misc", "tikz2pdf.tex", package = "knitr")
  )
  # add class options to template
  lines <- insert_template(
    lines, "%% TIKZ_CLASSOPTION %%", options$engine.opts$classoption %||% "tikz", TRUE
  )
  # insert code into preamble
  lines <- insert_template(
    lines, "%% EXTRA_TIKZ_PREAMBLE_CODE %%", options$engine.opts$extra.preamble, TRUE
  )
  # insert tikz code into the tex template
  s <- insert_template(lines, "%% TIKZ_CODE %%", options$code)
  xfun::write_utf8(s, texf <- wd_tempfile("tikz", ".tex"))
  on.exit(unlink(texf), add = TRUE)

  ext <- dev2ext(options)

  to_svg <- ext == "svg"
  outf <- if (to_svg) tinytex::latexmk(texf, "latex") else tinytex::latexmk(texf)

  fig <- knitr::fig_path(if (to_svg) ".dvi" else ".pdf", options)
  dir.create(dirname(fig), recursive = TRUE, showWarnings = FALSE)
  file.rename(outf, fig)

  fig2 <- xfun::with_ext(fig, ext)
  if (to_svg) {
    # dvisvgm needs to be on the path
    # dvisvgm for windows needs ghostscript bin dir on the path also
    if (Sys.which("dvisvgm") == "") tinytex::tlmgr_install("dvisvgm")
    if (system2("dvisvgm", c(
      options$engine.opts$dvisvgm.opts, "-o", shQuote(fig2), fig
    )) != 0) {
      stop("Failed to compile ", fig, " to ", fig2)
    }
  } else {
    # convert to the desired output-format using magick
    if (ext != "pdf") {
      magick::image_write(do.call(magick::image_convert, c(
        list(magick::image_read_pdf(fig), ext), options$engine.opts$convert.opts
      )), fig2)
    }
    if (options$transparency %||% FALSE) {
      magick::image_read(fig2) |>
        magick::image_transparent("white", fuzz = 0) |>
        magick::image_write(fig2)
    }
  }
  fig <- fig2

  options$fig.num <- 1L
  options$fig.cur <- 1L
  extra <- run_hook_plot(fig, options)
  knitr::engine_output(options, options$code, "", extra)
}



# Formatting --------------------------------------------------------------

#' Center text with width option
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param options Chunk options
#'
#' @export
#' @examplesIf interactive()
#' # Not run: knitr engine for Rmd/Quarto documents
#' eng_center_text(list(code = "Some text"))
#'
eng_center_text <- function(options) {
  lifecycle::deprecate_warn("1.0.0", "eng_center_text()")

  options$code <- options$code %>% knitr:::pandoc_fragment()
  options$type <- "center"

  if (!is.null(options$out.width)) {
    options$out.width <- knitr:::latex_percent_size(options$out.width, which = "width")
    options$code <- glue::glue(r"[\parbox{(options$out.width)}{(options$code)}]",
      .open = "(", .close = ")"
    )
  }

  knitr:::eng_block2(options)
}


# Boxes -------------------------------------------------------------------

#' Classic box environment
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param options Chunk options
#'
#' @export
#' @examplesIf interactive()
#' # Not run: knitr engine for Rmd/Quarto documents
#' eng_classic_box(list(code = "Some text"))
#'
eng_classic_box <- function(options) {
  lifecycle::deprecate_warn("1.0.0", "eng_classic_box()")

  if (is.null(options$raw)) {
    options$code <- options$code %>% knitr:::pandoc_fragment()
  } else {
    options$code <- options$code %>% knitr::raw_latex()
  }
  options$type <- "cBox"

  knitr:::eng_block2(options)
}

#' Provide a knit engine for AlXxx LaTeX commands
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param options Chunk options
#'
#' @return A string
#' @export
#' @examplesIf interactive()
#' # Not run: knitr engine for Rmd/Quarto documents
#' eng_document_ref(list(type = "article", author = "Some author", code = "Some text"))
#'
eng_document_ref <- function(options) {
  lifecycle::deprecate_warn("1.0.0", "eng_document_ref()")

  if (!knitr::is_latex_output()) {
    return()
  }

  if (options$type == "article") {
    output <- glue::glue(r"[\AlArticle{(options$code)}{(options$author)}]",
      .open = "(", .close = ")"
    ) %>%
      knitr::raw_latex()

    output
  } else if (options$type == "book") {
    output <- glue::glue(r"[\AlBook{(options$code)}{(options$author)}]",
      .open = "(", .close = ")"
    ) %>%
      knitr::raw_latex()

    output
  } else if (options$type == "press") {
    output <- glue::glue(r"[\AlPress{(options$code)}{(options$author)}]",
      .open = "(", .close = ")"
    ) %>%
      knitr::raw_latex()

    output
  } else if (options$type == "page") {
    output <- glue::glue(r"[\AlPage[(options$author)]{(options$code)}{(options$date)}]",
      .open = "(", .close = ")"
    ) %>%
      knitr::raw_latex()

    output
  } else if (options$type == "interview") {
    output <- glue::glue(r"[\AlInterview{(options$code)}{(options$author)}{(options$inteviewer)}]",
      .open = "(", .close = ")"
    ) %>%
      knitr::raw_latex()

    output
  }
}

#' Wrap image
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param options Chunk options
#'
#' @export
#' @examplesIf interactive()
#' # Not run: knitr engine for Rmd/Quarto documents
#' eng_wrap_figure(list(
#'   fig.cap = "Image caption",
#'   out.width = "80%",
#'   wrap.width = "20%"
#' ))
#'
eng_wrap_figure <- function(options) {
  lifecycle::deprecate_warn("1.0.0", "eng_wrap_figure()")

  if (!knitr::is_latex_output()) {
    warning("The image wrap engine isn't support in output formats other than pdf.")
    return()
  }

  options$code <- rlang::parse_expr(options$code) %>% eval()
  options$fig.cap <- knitr:::pandoc_fragment(options$fig.cap, from = "markdown", to = "latex")
  options$out.width <- latex_percent_size(options$out.width, which = "width")
  options$wrap.width <- latex_percent_size(options$wrap.width, which = "width", width = "\\textwidth")
  if (is.null(options$wrap.margin)) options$wrap.margin <- ""
  glue::glue(r"[
    \begin{wrapfigure}{(options$fig.align)}{(options$wrap.width)}
      \vspace{-2\intextsep}
      \begin{center}
        \includegraphics[width=(options$out.width)]{(options$code)}\\\
        {\small\emph{(options$fig.cap)}}
      \end{center}
      \vspace{(options$wrap.margin)\intextsep}
    \end{wrapfigure}
  ]",
    .open = "(", .close = ")"
  ) %>% knitr::raw_latex()
}

#' Insert figure environment
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param options Chunk options
#'
#' @export
#' @examplesIf interactive()
#' # Not run: knitr engine for Rmd/Quarto documents
#' eng_image_legend(list(
#'   img.cap = "Image caption",
#'   img.width = 80,
#'   img.author = "Someone"
#' ))
#'
eng_image_legend <- function(options) {
  lifecycle::deprecate_warn("1.0.0", "eng_image_legend()")

  if (!knitr::is_latex_output()) {
    warning("The question engine isn't support in output formats other than pdf.")
    return()
  }

  options$code <- rlang::parse_expr(options$code) %>% eval()
  options$img.cap <- knitr:::pandoc_fragment(options$img.cap, from = "markdown", to = "latex")
  options$leg.width <- latex_percent_size(
    paste0(100 - readr::parse_number(options$img.width), "%"),
    which = "width"
  )
  options$img.width <- latex_percent_size(options$img.width, which = "width")


  if (!is.null(options$img.author)) {
    glue::glue(r"[(options$img.cap)\newline
               (tufte::quote_footer(options$img.author))]",
      .open = "(", .close = ")"
    ) -> options$leg
  } else {
    options$leg <- options$img.cap
  }

  right_align <- ""
  if (options$img.align == "right") right_align <- "\\raggedleft"

  # nolint start: line_length_linter
  glue::glue(r"[\parbox[t]{(options$img.width)}{(right_align)\strut\vspace*{-\baselineskip}\newline\includegraphics[width=.95\linewidth]{(options$code)}}]",
    .open = "(", .close = ")"
  ) -> img
  # nolint end

  glue::glue(r"[\parbox[t]{(options$leg.width)}{(options$leg)}]",
    .open = "(", .close = ")"
  ) -> leg

  if (options$img.align == "left") {
    latex_return <- paste0(img, "\n", leg) %>% knitr::raw_latex()
  } else if (options$img.align == "right") {
    latex_return <- paste0(leg, "\n", img) %>% knitr::raw_latex()
  }

  latex_return
}

# Examdoc -----------------------------------------------------------------

#' Question in exam document
#'
#' `r lifecycle::badge("deprecated")`
#' Use quarto extension with solution filter.
#'
#' @param options Chunk options
#'
#' @export
#' @examplesIf interactive()
#' # Not run: knitr engine for Rmd/Quarto documents
#' eng_exam_questions(code = "Some text")
#'
eng_exam_questions <- function(options) {
  lifecycle::deprecate_warn("1.0.0", "eng_exam_questions()")

  if (!knitr::is_latex_output()) {
    warning("The question engine isn't support in output formats other than pdf.")
    return()
  }

  tibble::tibble(lines = options$code) %>%
    tidyr::extract(
      lines,
      into = c("code", "arg", "content"),
      regex = r"(([^|]*)\|?([^|]*)?\|?(.*))"
    ) %>%
    dplyr::mutate(dplyr::across(.fns = ~ dplyr::na_if(.x, y = ""))) %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(
      print = purrr::pmap_chr(
        .l = list(code, arg, content),
        .f = function(code, arg, content) {
          arg <- ifelse(is.na(arg), " ", paste0("[", arg, "] "))
          dplyr::case_when(
            code == "bq" ~ paste0(knitr::raw_latex(r"(\begin{questions})")),
            code == "rq" ~ paste0(knitr::raw_latex(r"(\begin{questions}\setcounter{question}{\qstcounter})")),
            code == "eq" ~ paste0(knitr::raw_latex(r"(\xdef\qstcounter{\arabic{question}}\end{questions})")),
            code == "ql" ~ paste0(knitr::raw_latex(paste0(r"(\question)", arg, content))),
            code == "qm" ~ paste0(
              knitr::raw_latex(paste0(r"(\question)", arg)),
              knitr:::pandoc_fragment(content)
            ),
            code == "bp" ~ paste0(knitr::raw_latex(r"(\begin{parts})")),
            code == "ep" ~ paste0(knitr::raw_latex(r"(\end{parts})")),
            code == "pl" ~ paste0(knitr::raw_latex(paste0(r"(\part)", arg, content))),
            code == "pm" ~ paste0(
              knitr::raw_latex(paste0(r"(\part)", arg)),
              knitr:::pandoc_fragment(content)
            ),
            code == "bs" ~ paste0(knitr::raw_latex(paste0(r"(\begin{solutionorlines})", arg))),
            code == "es" ~ paste0(knitr::raw_latex(paste0(r"(\end{solutionorlines})", arg))),
            code == "bb" ~ paste0(knitr::raw_latex(paste0(r"(\begin{solutionorbox})", arg))),
            code == "eb" ~ paste0(knitr::raw_latex(paste0(r"(\end{solutionorbox})", arg))),
            code == "np" ~ paste0(knitr::raw_latex(r"(\clearpage)")),
            code == "rl" ~ paste0(knitr::raw_latex(content)),
            code == "rm" ~ paste0(knitr:::pandoc_fragment(content))
          )
        }
      )
    ) %>%
    dplyr::pull(print) %>%
    stringr::str_c(collapse = "\n")
}
# Lists -------------------------------------------------------------------

#' Create paralist
#'
#' @param options Chunk options
#'
#' @export
#' @examplesIf interactive()
#' # Not run: knitr engine for Rmd/Quarto documents
#' eng_legal_list(code = "Some text")
#'
eng_legal_list <- function(options) {
  options$code %>%
    stringr::str_c(collapse = "\n") %>%
    stringr::str_replace_all(
      stringr::regex("^", multiline = TRUE), "  \\\\item "
    ) -> options$code

  glue::glue(r"[
    \begin{paralist}
      (options$code)
    \end{paralist}
  ]",
    .open = "(", .close = ")"
  ) %>% knitr::raw_latex()
}


# Internal functions -----------------------------------------------------

insert_template <- function(text, token, value, ignore = FALSE) {
  if (is.null(value)) {
    return(text)
  }
  i <- grep(token, text)
  n <- length(i)
  if (n > 1) stop("There are multiple tokens in the template: '", token, "'")
  if (n == 0) {
    if (ignore) {
      return(text)
    }
    stop("Couldn't find the token '", token, "' in the template.")
  }
  append(text, value, i)
}

wd_tempfile <- function(...) basename(tempfile(tmpdir = ".", ...))

dev2ext <- function(options) {
  if (length(ext <- options$fig.ext)) {
    return(ext)
  }
  x <- options$dev
  res <- auto_exts[x]
  if (any(idx <- is.na(res))) {
    for (i in x[idx]) dev_get(i)
    stop(
      "cannot find appropriate filename extensions for device ", x[idx], "; ",
      "please use chunk option 'fig.ext' (https://yihui.org/knitr/options/)"
    )
  }
  unname(res)
}

run_hook_plot <- function(x, options) {
  knitr::opts_knit$append(plot_files = x)
  hook <- knitr::knit_hooks$get("plot")
  hook(x, options)
}

auto_exts <- c(
  bmp = "bmp", postscript = "eps", pdf = "pdf", png = "png", svg = "svg",
  jpeg = "jpeg", pictex = "tex", tiff = "tiff", win.metafile = "wmf",
  cairo_pdf = "pdf", cairo_ps = "eps",
  quartz_pdf = "pdf", quartz_png = "png", quartz_jpeg = "jpeg",
  quartz_tiff = "tiff", quartz_gif = "gif", quartz_psd = "psd",
  quartz_bmp = "bmp",
  CairoJPEG = "jpeg", CairoPNG = "png", CairoPS = "eps", CairoPDF = "pdf",
  CairoSVG = "svg", CairoTIFF = "tiff",
  svglite = "svg", gridSVG = "svg",
  ragg_png = "png",
  tikz = "tex"
)
