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

#' Install fonts on MacOS
#'
#' @return Move fonts to MacOS font folder
#' @export
gvz_install_fonts_macos <- function() {
  fs::file_copy(
    path = fs::dir_ls(path = gvz_file("fonts/")),
    new_path = "~/Library/Fonts/",
    overwrite = TRUE
  )
}
