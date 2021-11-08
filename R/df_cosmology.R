#' Format cosmology object box
#'
#' @return
#' @export
gvz_cosmo_solar_system_object <- function(name) {
  geodata::df_cosmology_solar_system_objects |>
    dplyr::filter(english == name) -> object_data

  child_env <- rlang::env()
  rlang::env_bind(child_env, object = object_data)

  if (knitr::is_html_output()) {
    knitr::knit_child(
      gvz_file("resources/markdown/cosmo_solar_system_object_html.Rmd"),
      options = list(results = 'asis'),
      envir = child_env
    ) |> knitr::raw_html() -> child

    cat(child)
  } else if (knitr::is_latex_output()) {
   knitr::knit_child(
      gvz_file("resources/markdown/cosmo_solar_system_object_latex.Rmd"),
      envir = child_env
    ) |> knitr::raw_latex() -> child

    cat(child)
  } else {
    return()
  }
}
