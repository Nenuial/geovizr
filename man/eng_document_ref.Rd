% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knit_engines.R
\name{eng_document_ref}
\alias{eng_document_ref}
\title{Provide a knit engine for AlXxx LaTeX commands}
\usage{
eng_document_ref(options)
}
\arguments{
\item{options}{Chunk options}
}
\value{
A string
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Not run: knitr engine for Rmd/Quarto documents
eng_document_ref(list(type = "article", author = "Some author", code = "Some text"))
\dontshow{\}) # examplesIf}
}
