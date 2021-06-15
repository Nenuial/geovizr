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


  # Check latex packages ----------------------------------------------------
  installed <- tinytex::tl_pkgs()
  needed <- c("babel-french", "babel-english", "datetime2-french", "datetime2-english", "pdfcrop", "firamath")

  check_latex_pkg <- function(pkg) {
    if (!(pkg %in% installed)) tinytex::tlmgr_install(pkg)
  }

  needed %>%
    purrr::walk(check_latex_pkg)
}
