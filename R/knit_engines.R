# General purpose ---------------------------------------------------------

#' Raw LaTeX
#'
#' @param options
#'
#' @export
eng_latex_raw <- function(options) {
  options$code %>%
    knitr::raw_latex()
}


# Formatting --------------------------------------------------------------

#' Center text with width option
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param options
#'
#' @export
eng_center_text <- function(options) {
  lifecycle::deprecate_warn("0.1.0", "eng_center_text()")

  options$code <- options$code %>% knitr:::pandoc_fragment()
  options$type <- "center"

  if(!is.null(options$out.width)) {
    options$out.width <- knitr:::latex_percent_size(options$out.width, which = "width")
    options$code <- glue::glue(r"[\parbox{(options$out.width)}{(options$code)}]",
                               .open = "(", .close = ")")
  }

  knitr:::eng_block2(options)
}


# Boxes -------------------------------------------------------------------

#' Classic box environment
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param options
#'
#' @export
eng_classic_box <- function(options) {
  lifecycle::deprecate_warn("0.1.0", "eng_classic_box()")

  if(is.null(options$raw)) {
    options$code <- options$code %>% knitr:::pandoc_fragment()
  } else {
    options$code <- options$code %>% knitr::raw_latex()
  }
  options$type <- "cBox"

  knitr:::eng_block2(options)
}

#' Provide a knit engine for AlXxx LaTeX commands
#'
#' @param options knitr engine options
#'
#' @return A string
#' @export
eng_document_ref <- function(options) {
  if(!knitr::is_latex_output()) return()

  if(options$type == "article") {
    output <- glue::glue(r"[\AlArticle{(options$code)}{(options$author)}]",
                         .open = "(", .close = ")") %>%
      knitr::raw_latex()

    return(output)
  } else if (options$type == "book") {
    output <- glue::glue(r"[\AlBook{(options$code)}{(options$author)}]",
                         .open = "(", .close = ")") %>%
      knitr::raw_latex()

    return(output)
  } else if (options$type == "press") {
    output <- glue::glue(r"[\AlPress{(options$code)}{(options$author)}]",
                         .open = "(", .close = ")") %>%
      knitr::raw_latex()

    return(output)
  } else if (options$type == "page") {
    output <- glue::glue(r"[\AlPage[(options$author)]{(options$code)}{(options$date)}]",
                         .open = "(", .close = ")") %>%
      knitr::raw_latex()

    return(output)
  } else if (options$type == "interview") {
    output <- glue::glue(r"[\AlInterview{(options$code)}{(options$author)}{(options$inteviewer)}]",
                         .open = "(", .close = ")") %>%
      knitr::raw_latex()

    return(output)
  } else return()
}

#' Wrap image
#'
#' @param options
#'
#' @export
eng_wrap_figure <- function(options) {
  options$code <- rlang::parse_expr(options$code) %>% eval()
  options$out.width <- latex_percent_size(options$out.width, which = "width")
  options$wrap.width <- latex_percent_size(options$wrap.width, which = "width", width = "\\textwidth")
  if(is.null(options$wrap.margin)) options$wrap.margin <- ""
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
  .open = "(", .close = ")") %>% knitr::raw_latex()
}

#' Insert figure environment
#'
#' @param options
#'
#' @export
eng_image_legend <- function(options) {
  if(!knitr::is_latex_output()) {
    warning("The question engine isn't support in output formats other than pdf.")
    return()
  }

  options$code <- rlang::parse_expr(options$code) %>% eval()
  options$leg.width <- latex_percent_size(
    paste0(100 - readr::parse_number(options$img.width), "%"),
    which = "width"
  )
  options$img.width <- latex_percent_size(options$img.width, which = "width")


  if(!is.null(options$img.author)) {
    glue::glue(r"[(options$img.cap)\newline
               (tufte::quote_footer(options$img.author))]",
    .open = "(", .close = ")") -> options$leg
  } else {
    options$leg <- options$img.cap
  }

  glue::glue(r"[\parbox[t]{(options$img.width)}{\strut\vspace*{-\baselineskip}\newline\includegraphics[width=.95\linewidth]{(options$code)}}]",
  .open = "(", .close = ")") -> img

  glue::glue(r"[\parbox[t]{(options$leg.width)}{(options$leg)}]",
  .open = "(", .close = ")") -> leg

  if (options$img.align == "left") {
    latex_return <- paste0(img, "\n", leg) %>% knitr::raw_latex()
  } else if (options$img.align == "right") {
    latex_return <- paste0(leg, "\n", img) %>% knitr::raw_latex()
  }

  return(latex_return)
}

# Examdoc -----------------------------------------------------------------

#' Question in exam document
#'
#' @param options
#'
#' @export
eng_exam_questions <- function(options) {
  if(!knitr::is_latex_output()) {
    warning("The question engine isn't support in output formats other than pdf.")
    return()
  }

  tibble::tibble(lines = options$code) %>%
    tidyr::extract(
      lines, into = c("code", "arg", "content"),
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
            code == "bq"  ~ paste0(knitr::raw_latex(r"(\begin{questions})")),
            code == "rq"  ~ paste0(knitr::raw_latex(r"(\begin{questions}\setcounter{question}{\qstcounter})")),
            code == "eq"  ~ paste0(knitr::raw_latex(r"(\xdef\qstcounter{\arabic{question}}\end{questions})")),

            code == "ql"  ~ paste0(knitr::raw_latex(paste0(r"(\question)", arg, content))),
            code == "qm"  ~ paste0(knitr::raw_latex(paste0(r"(\question)", arg)),
                                   knitr:::pandoc_fragment(content)),

            code == "bp"  ~ paste0(knitr::raw_latex(r"(\begin{parts})")),
            code == "ep"  ~ paste0(knitr::raw_latex(r"(\end{parts})")),
            code == "pl"  ~ paste0(knitr::raw_latex(paste0(r"(\part)", arg, content))),
            code == "pm"  ~ paste0(knitr::raw_latex(paste0(r"(\part)", arg)),
                                   knitr:::pandoc_fragment(content)),

            code == "bs"  ~ paste0(knitr::raw_latex(paste0(r"(\begin{solutionorlines})", arg))),
            code == "es"  ~ paste0(knitr::raw_latex(paste0(r"(\end{solutionorlines})", arg))),
            code == "bb"  ~ paste0(knitr::raw_latex(paste0(r"(\begin{solutionorbox})", arg))),
            code == "eb"  ~ paste0(knitr::raw_latex(paste0(r"(\end{solutionorbox})", arg))),

            code == "np"  ~ paste0(knitr::raw_latex(r"(\clearpage)")),
            code == "rl"  ~ paste0(knitr::raw_latex(content)),
            code == "rm"  ~ paste0(knitr:::pandoc_fragment(content))
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
#' @param options
#'
#' @export
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
  .open = "(", .close = ")") %>% knitr::raw_latex()
}
