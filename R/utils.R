#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

.onLoad = function(lib, pkg) {
  # Load engines ------------------------------------------------------------
  # General purpose
  knitr::knit_engines$set(latex = eng_latex_raw)
  # Formatting
  knitr::knit_engines$set(center = eng_center_text)
  # Boxes
  knitr::knit_engines$set(cbox = eng_classic_box)
  knitr::knit_engines$set(ref = eng_document_ref)
  knitr::knit_engines$set(wrap = eng_wrap_figure)
  knitr::knit_engines$set(img = eng_image_legend)
  # Examdoc
  knitr::knit_engines$set(exam = eng_exam_questions)
  # Lists
  knitr::knit_engines$set(legal = eng_legal_list)
}

#' Install fonts
#'
#' @return Installs the fonts using sysfonts
#' @export
gvz_install_fonts <- function() {
  # DIN Alternate
  sysfonts::font_add("DIN Alternate",
                     regular = system.file("fonts/DIN Alternate Bold.ttf", package = "geovizr"),
                     bold = system.file("fonts/DIN Alternate Bold.ttf", package = "geovizr"),)

  # DIN Condensed
  sysfonts::font_add("DIN Condensed",
                     regular = system.file("fonts/DIN Condensed Bold.ttf", package = "geovizr"),
                     bold = system.file("fonts/DIN Condensed Bold.ttf", package = "geovizr"))

  # Fira Sans
  sysfonts::font_add("FiraSans",
                     regular = system.file("fonts/FiraSans-Light.ttf", package = "geovizr"),
                     italic = system.file("fonts/FiraSans-LightItalic.ttf", package = "geovizr"),
                     bold = system.file("fonts/FiraSans-Regular.ttf", package = "geovizr"),
                     bolditalic = system.file("fonts/FiraSans-Italic.ttf", package = "geovizr"))
}
