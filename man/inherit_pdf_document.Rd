% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knit_outputs.R
\name{inherit_pdf_document}
\alias{inherit_pdf_document}
\title{Rmarkdown pdf document}
\usage{
inherit_pdf_document(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[rmarkdown:pdf_document]{rmarkdown::pdf_document}}
  \describe{
    \item{\code{toc}}{\code{TRUE} to include a table of contents in the output}
    \item{\code{toc_depth}}{Depth of headers to include in table of contents}
    \item{\code{number_sections}}{\code{TRUE} to number section headings}
    \item{\code{fig_width}}{Default width (in inches) for figures}
    \item{\code{fig_height}}{Default height (in inches) for figures}
    \item{\code{fig_crop}}{Whether to crop PDF figures with the command
\command{pdfcrop}. This requires the tools \command{pdfcrop} and
\command{ghostscript} to be installed. By default, \code{fig_crop = TRUE}
if these two tools are available.}
    \item{\code{fig_caption}}{\code{TRUE} to render figures with captions}
    \item{\code{dev}}{Graphics device to use for figure output (defaults to pdf)}
    \item{\code{df_print}}{Method to be used for printing data frames. Valid values
include "default", "kable", "tibble", and "paged". The "default" method
uses a corresponding S3 method of \code{print}, typically
\code{print.data.frame}. The "kable" method uses the
\code{\link[knitr:kable]{knitr::kable}} function. The "tibble" method uses
the \pkg{tibble} package to print a summary of the data frame. The "paged"
method creates a paginated HTML table (note that this method is only valid
for formats that produce HTML). In addition to the named methods you can
also pass an arbitrary function to be used for printing data frames. You
can disable the \code{df_print} behavior entirely by setting the option
\code{rmarkdown.df_print} to \code{FALSE}. See
\href{https://bookdown.org/yihui/rmarkdown/html-document.html#data-frame-printing}{Data
frame printing section} in bookdown book for examples.}
    \item{\code{highlight}}{Syntax highlighting style passed to Pandoc.

 Supported built-in styles include "default", "tango", "pygments", "kate",
 "monochrome", "espresso", "zenburn", "haddock", and "breezedark".

  Two custom styles are also included, "arrow", an accessible color scheme,
  and "rstudio", which mimics the default IDE theme. Alternatively, supply a
  path to a \samp{.theme} file to use
  \href{https://pandoc.org/MANUAL.html#syntax-highlighting}{a custom Pandoc
  style}. Note that custom theme requires Pandoc 2.0+.

  Pass \code{NULL} to prevent syntax highlighting.}
    \item{\code{template}}{Pandoc template to use for rendering. Pass "default" to use
the rmarkdown package default template; pass \code{NULL} to use pandoc's
built-in template; pass a path to use a custom template that you've
created.  See the documentation on
\href{https://pandoc.org/MANUAL.html}{pandoc online documentation} for
details on creating custom templates.}
    \item{\code{keep_tex}}{Keep the intermediate tex file used in the conversion to PDF.
Note that this argument does not control whether to keep the auxiliary
files (e.g., \file{.aux}) generated by LaTeX when compiling \file{.tex} to
\file{.pdf}. To keep these files, you may set \code{options(tinytex.clean =
FALSE)}.}
    \item{\code{keep_md}}{Keep the markdown file generated by knitting.}
    \item{\code{latex_engine}}{LaTeX engine for producing PDF output. Options are
"pdflatex", "lualatex", "xelatex" and "tectonic".}
    \item{\code{citation_package}}{The LaTeX package to process citations, \code{natbib}
or \code{biblatex}. Use \code{default} if neither package is to be used,
which means citations will be processed via the command
\command{pandoc-citeproc}.}
    \item{\code{includes}}{Named list of additional content to include within the
document (typically created using the \code{\link[rmarkdown]{includes}} function).}
    \item{\code{md_extensions}}{Markdown extensions to be added or removed from the
default definition of R Markdown. See the \code{\link[rmarkdown]{rmarkdown_format}} for
additional details.}
    \item{\code{output_extensions}}{Pandoc extensions to be added or removed from the
output format, e.g., \code{"-smart"} means the output format will be
\code{latex-smart}.}
    \item{\code{pandoc_args}}{Additional command line options to pass to pandoc}
    \item{\code{extra_dependencies}}{A LaTeX dependency \code{latex_dependency()}, a
list of LaTeX dependencies, a character vector of LaTeX package names (e.g.
\code{c("framed", "hyperref")}), or a named list of LaTeX package options
with the names being package names (e.g. \code{list(hyperef =
c("unicode=true", "breaklinks=true"), lmodern = NULL)}). It can be used to
add custom LaTeX packages to the .tex header.}
  }}
}
\description{
Call rmarkdown::pdf_document and mark the return
value as inheriting pdf_document
}
\keyword{internal}
