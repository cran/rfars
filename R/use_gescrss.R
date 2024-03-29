#' (Internal) Use GESCRSS data files
#'
#' Compile multiple years of prepared GESCRSS data.
#'
#' @param dir Inherits from get_gescrss().
#' @param prepared_dir Inherits from get_gescrss().
#' @param cache Inherits from get_gescrss().
#'
#' @return Returns an object of class 'GESCRSS' which is a list of six tibbles:
#'     flat, multi_acc, multi_veh, multi_per, events, and codebook.

use_gescrss <- function(dir, prepared_dir, cache){

  flat <-

    suppressWarnings({ #this is just for the small number of coercion errors with mutate_at(lat, lon, as.numeric)

    suppressMessages({

      data.frame(path = list.files(path = prepared_dir, full.names = TRUE, pattern = "_flat.rds", recursive = TRUE)) %>%
      mutate(year = stringr::word(.data$path, -1, sep = "/") %>% substr(1,4)) %>%
      pull(.data$path) %>%
      lapply(function(x){
        readRDS(x) %>%
        mutate_all(as.character)
        }) %>%
      bind_rows() %>%
      readr::type_convert() %>%
      distinct()

    })

    })


  multi_acc <- import_multi("multi_acc.rds", where = prepared_dir) #%>% distinct()
  multi_veh <- import_multi("multi_veh.rds", where = prepared_dir) #%>% distinct()
  multi_per <- import_multi("multi_per.rds", where = prepared_dir) #%>% distinct()
  events    <- import_multi("_events.rds",   where = prepared_dir) #%>% distinct()

  codebook <- readRDS(file = paste0(prepared_dir, "/codebook.rds")) #%>% distinct()

  out <- list(
    "flat"      = flat,
    "multi_acc" = multi_acc,
    "multi_veh" = multi_veh,
    "multi_per" = multi_per,
    "events"    = events,
    "codebook"  = codebook
    )

  class(out) <- c(class(out), "GESCRSS")

  if(!is.null(cache)){
    saveRDS(out,
            gsub("//", "/", paste0(dir, "/", cache))
            )
    }

  return(out)

  }
