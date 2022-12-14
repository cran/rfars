#' (Internal) Takes care of basic CSV reading
#'
#' @param x The cleaned name of the data table (CSV).
#' @param wd The working directory for these files
#' @param rawfiles The data frame connecting raw filenames to cleaned ones.

read_basic_csv <- function(x, wd, rawfiles){

  readr::read_csv(
    paste0(wd, rawfiles$filename[rawfiles$cleaned==x]),
    col_types = cols(),
    show_col_types = FALSE
    ) %>%
  janitor::clean_names() %>%
  usenames() %>%
  return()

}
