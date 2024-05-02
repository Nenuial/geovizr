#' Get exam data for Matu fédérale
#'
#' @param start_date String Y-M-D start date for filter
#' @param end_date String Y-M-D end date for filter
#'
#' @return A tibble with exam data
#' @export
#' @keywords internal
gvz_matu_data <- function(start_date, end_date) {
  rnotion::rni_api_client()$databases$query(
    database_id = "5ced5c6f-8242-4ba6-8e75-5dbc26eddc68",
    filter = list(
      and = list(
        list(
          property = "Passage",
          date = list(
            on_or_after = start_date
          )
        ),
        list(
          property = "Passage",
          date = list(
            on_or_before = end_date
          )
        ),
        list(
          property = "Question",
          relation = list(
            is_not_empty = TRUE
          )
        )
      )
    ),
    sorts = list(
      list(
        property = "Passage",
        direction = "ascending"
      ),
      list(
        property = "Num candidat",
        direction = "ascending"
      )
    )
  ) |>
    purrr::pluck("results") |>
    purrr::map_df(gvz_matu_data_assemble)
}

#' Assemble exam data into a tibble
#'
#' @param page Notion page object
#'
#' @return A tibble
#' @keywords internal
gvz_matu_data_assemble <- function(page) {
  candidate <- rnotion::rni_api_client()$pages$retrieve(page_id = page$id)
  subject_page <- rnotion::rni_api_client()$pages$retrieve(page_id = page$properties$`Sujet retenu`$relation[[1]]$id)

  id <- candidate$properties$`Num candidat`$title[[1]]$plain_text
  name <- paste(
    candidate$properties$Prenom$rich_text[[1]]$plain_text,
    candidate$properties$Nom$rich_text[[1]]$plain_text
  )
  subject <- subject_page$properties$Name$title[[1]]$plain_text
  question <- rnotion::rni_render_blocks(candidate$properties$Question$relation[[1]]$id)

  tibble::tibble(
    ID = id,
    Name = name,
    Subject = subject,
    Question = question
  )
}
