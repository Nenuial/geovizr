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
        "_", recipient[["last"]], "_", recipient[["first"]]
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
