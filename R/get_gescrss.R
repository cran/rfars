#' Get GES/CRSS data
#'
#' Bring GES/CRSS data into the current environment, whether by downloading it anew
#'     or by using pre-existing files.
#'
#' @export
#'
#' @param years Years to be downloaded, in yyyy (character or numeric formats),
#'     currently limited to 2011-2021.
#' @param regions (Optional) Regions to keep: mw=midwest, ne=northeast, s=south, w=west.
#' @param dir Directory in which to search for or save a 'GESCRSS data' folder. If
#'     NULL (the default), files are downloaded and unzipped to temporary
#'     directories and prepared in memory.
#' @param proceed Logical, whether or not to proceed with downloading files without
#'     asking for user permission (defaults to FALSE, thus asking permission)
#' @param cache The name of an RDS file to save or use. If the specified file (e.g., 'myFARS.rds')
#'    exists in 'dir' it will be returned; if not, an RDS file of this name will be
#'    saved in 'dir' for quick use in subsequent calls.
#'
#' @return A GESCRSS data object (a list with six tibbles: flat, multi_acc,
#'     multi_veh, multi_per, events, and codebook).
#'
#' @details
#'    This function downloads raw data from the GES and CRSS crash databases.
#'    If no directory (dir) is specified, raw CSV files are downloaded into a
#'    tempdir(), where they are also prepared, combined, and then brought into
#'    the current environment. If you specify a directory (dir), the function will
#'    look there for a 'GESCRSS data' folder. If not found, it will be created and
#'    populated with raw and prepared SAS and RDS files. If the directory is found, the
#'    function makes sure all requested years are present and asks permission
#'    to download any missing years.
#'
#'    The object returned is a list with class 'GESCRSS'. It contains six tibbles:
#'    flat, multi_acc, multi_veh, multi_per, events, and codebook.
#'
#'    Flat files are wide-formatted and presented at the person level.
#'    All \emph{crashes} involve at least one motor \emph{vehicle}, each of
#'    which may contain one or multiple \emph{people}. These are the three
#'    entities of crash data. The flat files therefore repeat some data elements
#'    across multiple rows. Please conduct your analysis with your entity in mind.
#'
#'    Some data elements can include multiple values for any data level
#'    (e.g., multiple weather conditions corresponding to the crash, or multiple
#'    crash factors related to vehicle or person). These elements have been
#'    collected in the yyyy_multi_[acc/veh/per].rds files in long format.
#'    These files contain crash, vehicle, and person identifiers, and two
#'    variables labelled \code{name} and \code{value}. These correspond to
#'    variable names from the raw data files and the corresponding values,
#'    respectively.
#'
#'    The events tibble provides a sequence of events for all vehicles involved
#'    in the crash. See Crash Sequences vignette for an example.
#'
#'    The codebook tibble serves as a searchable codebook for all files of any given year.
#'
#'    Please review the \href{https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/813436}{CRSS Analytical User's Manual}
#'
#'    Regions are as follows:
#'       mw = Midwest   = OH, IN, IL, MI, WI, MN, ND, SD, NE, IA, MO, KS
#'       ne = Northeast = PA, NJ, NY, NH, VT, RI, MA, ME, CT
#'       s  = South     = MD, DE, DC, WV, VA, KY, TN, NC, SC, GA, FL, AL, MS, LA, AR, OK, TX
#'       w  = West      = MT, ID, WA, OR, CA, NV, NM, AZ, UT, CO, WY, AK, HI
#'
#'
#' @examples
#'
#'   \dontrun{
#'     myGESCRSS <- get_gescrss(years = 2021, regions = "s")
#'   }

get_gescrss <- function(years     = 2011:2022,
                        regions   = c("mw", "ne", "s", "w"),
                        dir       = NULL,
                        proceed   = FALSE,
                        cache     = NULL
                        ){


  # Check for cached RDS file in dir
    if(!is.null(cache)){
      if(!is.null(dir)){
        if(cache %in% list.files(dir)){

          message("Note: you specified years and a cache file. If the cache file exists, 'years' is ignored.")
          return(readRDS( gsub("//", "/", paste0(dir, "/", cache))))

        }
      }
    }


  # Check years ----
    ymax <- max(as.numeric(years), na.rm = TRUE)
    ymin <- min(as.numeric(years), na.rm = TRUE)
    if(ymin < 2011) stop("Data not available prior to 2011.")
    if(ymax > 2022) stop("Data not available beyond 2022.")


  # Check regions
    if(any(!regions %in% c("mw", "ne", "s", "w"))) stop("Specify regions as: mw (midwest), ne (northeast), s (south), w (west).")


  # Download data without saving or checking hard drive ----

    if(is.null(dir)){

      dest_files <-
        tempdir() %>%
        stringr::str_replace_all(stringr::fixed("\\"), "/") %>%
        stringr::str_c("/GESCRSS data")

      dest_raw   <- paste0(dest_files, "/raw")
      dest_prepd <- paste0(dest_files, "/prepd")

      dir.create(dest_files, showWarnings = FALSE)
      dir.create(dest_raw,   showWarnings = FALSE)
      dir.create(dest_prepd, showWarnings = FALSE)

      download_gescrss(
        years = years,
        dest_raw = dest_raw,
        dest_prepd = dest_prepd,
        regions = regions)

      return(
        use_gescrss(
          dir = dir,
          prepared_dir = dest_prepd,
          cache = cache
          )
        )

    }


  # Look for pre-existing data ----

    if(!is.null(dir)){ #...in this folder

      # dir = getwd()

      my_dir <-
        data.frame(path=list.dirs(dir)) %>%
        mutate(folder = stringr::word(.data$path, start = -1, end = -1, sep = "/")) %>%
        filter(.data$folder == "GESCRSS data")

      dest_files <- paste0(dir, "/GESCRSS data")
      dest_raw   <- paste0(dir, "/GESCRSS data/raw")
      dest_prepd <- paste0(dir, "/GESCRSS data/prepd")


      if(nrow(my_dir)==0){ ## No data found ----

        if(!proceed){
          x <- readline("We will now download several files from https://www.nhtsa.gov/file-downloads?p=nhtsa/downloads/  \nProceed? (Y/N) \n")
          if(!(x %in% c("y", "Y"))) return(message("Download cancelled.\n"))
        }

        dir.create(dest_files, showWarnings = FALSE)
        dir.create(dest_raw,   showWarnings = FALSE)
        dir.create(dest_prepd, showWarnings = FALSE)

        download_gescrss(years = years, dest_raw = dest_raw, dest_prepd = dest_prepd, regions = regions)

        return(use_gescrss(dir = dir, prepared_dir = dest_prepd, cache = cache))

      }



    if(nrow(my_dir)==1){ # Some data found ----

       check_dir <- paste0(my_dir$path, "/prepd")

       # Check years
         files_found <- list.files(check_dir, pattern = "_flat.csv")
         years_found <- stringr::word(files_found, sep = "_")
         years_needed  <- setdiff(years, years_found)

         if(length(years_needed) > 0){

           if(!proceed){
            x <-
              paste0(
                paste(years_needed, collapse = ", "),
                " not found in ", dir,
                "\nEnter '1' to download them or any other key to skip") %>%
              readline()
            if(x == "1"){
              download_gescrss(years = years, dest_raw = dest_raw, dest_prepd = dest_prepd, regions = regions)
            } else{
              stop("Download cancelled.\n")
            }
           } else{
             message(paste0("Downloading years ", paste(years_needed, collapse = ", ")))
             download_gescrss(years = years_needed, dest_raw = dest_raw, dest_prepd = dest_prepd, regions = regions)
           }
         }

         return(use_gescrss(dir = dir, prepared_dir = dest_prepd, cache = cache))


    }


    if(nrow(my_dir)>1){ #Ambiguous
      stop("Multiple 'GESCRSS data' folders found. Please specify using the 'dir' parameter.")
    }

    }


}
