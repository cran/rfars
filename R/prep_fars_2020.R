#' Handle yyyy data preparation
#'
#' @param y year, to be passed from \code{prep_fars}
#' @param wd working directory, , to be passed from \code{prep_fars}
#' @param rawfiles dataframe translating filenames into standard terms,
#'     to be passed from \code{prep_fars}
#' @param prepared_dir the location where prepared files will be saved,
#'     to be passed from \code{prep_fars}
#'
#' @return Produces five files: yyyy_flat.csv, yyyy_multi_acc.csv,
#'     yyyy_multi_veh.csv, yyyy_multi_per.csv, and yyyy_events.csv
#'
#' @importFrom rlang .data


prep_fars_2020 <- function(y, wd, rawfiles, prepared_dir){

# Fix wd, prepared_dir
  if(substr(wd, nchar(wd), nchar(wd)) != "/") wd <- paste0(wd, "/")
  if(substr(prepared_dir, nchar(prepared_dir), nchar(prepared_dir)) != "/") prepared_dir <- paste0(prepared_dir, "/")


# core files ----

  ## accident ----

  fars.accident <-
    read_basic_csv(x = "accident", wd = wd, rawfiles = rawfiles) %>%
    select(-starts_with("weather"))


  ## vehicle ----

  # NOTE several variables appear here for the first time:
  # "vpicmake"      "vpicmodel"     "vpicbodyclass"
  # "icfinalbody"   "gvwr_from"     "gvwr_to"       "trlr1gvwr"
  # "trlr2gvwr"     "trlr3gvwr"


  fars.vehicle <-
    read_basic_csv(x = "vehicle", wd = wd, rawfiles = rawfiles) %>%
    select(-starts_with("vin_"))

  fars.vehicle <- fars.vehicle[,!names(fars.vehicle) %in% setdiff(intersect(names(fars.accident), names(fars.vehicle)), c("state", "st_case"))]


  ## person ----

  fars.person <- read_basic_csv(x = "person", wd = wd, rawfiles = rawfiles)

  fars.person <- fars.person[,!names(fars.person) %in% setdiff(intersect(names(fars.accident), names(fars.person)), c("state", "st_case"))]
  fars.person <- fars.person[,!names(fars.person) %in% setdiff(intersect(names(fars.vehicle), names(fars.person)), c("state", "st_case", "veh_no"))]


# accident-level files ----


  ## multiple-row files ----

    ### x cevent ----

    # fars.cevent <- read_basic_csv(x = "cevent", wd = wd, rawfiles = rawfiles)
    # Cevent, Vevent, and vsoe are very similar; vsoe is the one to use (p. 16)

    ### weather ----

    fars.weather <- read_basic_csv(x = "weather", wd = wd, rawfiles = rawfiles)

    ### crashrf ----

    fars.crashrf <- read_basic_csv(x = "crashrf", wd = wd, rawfiles = rawfiles)



# vehicle-level files ----


  ## x parkwork ----

  ## multi-row files ----

    ### vsoe ----

    fars.vsoe <- read_basic_csv(x = "vsoe", wd = wd, rawfiles = rawfiles)

    ### distract ----

    fars.distract <- read_basic_csv(x = "distract", wd = wd, rawfiles = rawfiles)

    ### drimpair ----

    fars.drimpair <- read_basic_csv(x = "drimpair", wd = wd, rawfiles = rawfiles)

    ### factor ----

    fars.factor <- read_basic_csv(x = "factor", wd = wd, rawfiles = rawfiles)

    ### maneuver ----

    fars.maneuver <- read_basic_csv(x = "maneuver", wd = wd, rawfiles = rawfiles)

    ### violatn ----

    fars.violatn <- read_basic_csv(x = "violatn", wd = wd, rawfiles = rawfiles)

    ### vision ----

    fars.vision <- read_basic_csv(x = "vision", wd = wd, rawfiles = rawfiles)

    ### damage ----

    fars.damage <- read_basic_csv(x = "damage", wd = wd, rawfiles = rawfiles)

    ### vehiclesf ----

    fars.vehiclesf <- read_basic_csv(x = "vehiclesf", wd = wd, rawfiles = rawfiles)

    ### x pvehiclesf ----

    #fars.pvehiclesf <- read_basic_csv(x = "pvehiclesf", wd = wd, rawfiles = rawfiles)

    ### driverrf ----

    fars.driverrf <- read_basic_csv(x = "driverrf", wd = wd, rawfiles = rawfiles)


# person-level files ----

  ## pbtype ----

  fars.pbtype <-
    read_basic_csv(x = "pbtype", wd = wd, rawfiles = rawfiles) %>%
    select(-.data$pbptype, -.data$pbage, -.data$pbsex)

  fars.pbtype <- fars.pbtype[,!names(fars.pbtype) %in% setdiff(intersect(names(fars.person), names(fars.pbtype)), c("state", "st_case", "veh_no", "per_no"))]



  ## safetyeq ----

  fars.safetyeq <- read_basic_csv(x = "safetyeq", wd = wd, rawfiles = rawfiles)

  fars.safetyeq <- fars.safetyeq[,!names(fars.safetyeq) %in% setdiff(intersect(names(fars.person), names(fars.safetyeq)), c("state", "st_case", "veh_no", "per_no"))]



  ## multi-row files ----

    ### nmcrash ----

    fars.nmcrash <- read_basic_csv(x = "nmcrash", wd = wd, rawfiles = rawfiles)

    ### nmimpair ----

    fars.nmimpair <- read_basic_csv(x = "nmimpair", wd = wd, rawfiles = rawfiles)

    ### nmprior ----

    fars.nmprior <- read_basic_csv(x = "nmprior", wd = wd, rawfiles = rawfiles)

    ### nmdistract ----

    fars.nmdistract <- read_basic_csv(x = "nmdistract", wd = wd, rawfiles = rawfiles)

    ### drugs ----

    fars.drugs <- read_basic_csv(x = "drugs", wd = wd, rawfiles = rawfiles)

    ### race ----

    fars.race <- read_basic_csv(x = "race", wd = wd, rawfiles = rawfiles)

    fars.race <- fars.race[,!names(fars.race) %in% setdiff(intersect(names(fars.person), names(fars.race)), c("state", "st_case", "veh_no", "per_no"))]


    ### personrf ----

    fars.personrf <- read_basic_csv(x = "personrf", wd = wd, rawfiles = rawfiles)

    fars.personrf <- fars.personrf[,!names(fars.personrf) %in% setdiff(intersect(names(fars.person), names(fars.personrf)), c("state", "st_case", "veh_no", "per_no"))]



# produce flat file ----

  fars <-
    fars.accident %>%
    left_join(fars.person, by = c("state", "st_case")) %>%
    #! This order (accident >> person >> vehicle) is very important for including non-motorists
    left_join(fars.vehicle, by = c("state", "st_case", "veh_no")) %>%
    left_join(fars.pbtype, by = c("state", "st_case", "veh_no", "per_no")) %>%
    left_join(fars.safetyeq, by = c("state", "st_case", "veh_no", "per_no")) %>%
    as.data.frame() %>%

  # State filter
    #filter(.data$state %in% geo_filtered$state_name_full) %>%

  # Dates
    # mutate(
    #   date_crash = lubridate::make_datetime(year, match(month, month.name), day, hour, minute),
    #   date_death = lubridate::make_datetime(death_yr, match(death_mo, month.name), death_da, death_hr, death_mn),
    #   ) %>%

  # Generate state-independent id for each crash
    mutate(id = paste0(.data$year, .data$st_case)) %>%

  # Final organization
    select(.data$year, .data$state, .data$st_case, .data$id,
           #date_crash,
           .data$veh_no, .data$per_no,
           .data$county, .data$city,
           lon = .data$longitud,
           lat = .data$latitude,
           everything())



# concatenate long files for multi-row files ----

  multi_acc <-
    bind_rows(
      fars.weather %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:2)),
      fars.crashrf %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:2))
      ) %>%
    as.data.frame() %>%
    mutate(year = y)

  multi_veh <-
    bind_rows(
      fars.vehiclesf %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:3)),
      fars.driverrf %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:3)),
      fars.drimpair %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:3)),
      fars.distract %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:3)),
      fars.factor %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:3)),
      fars.maneuver %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:3)),
      fars.violatn %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:3)),
      fars.vision %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:3)),
      fars.damage %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:3))
      ) %>%
    as.data.frame() %>%
    mutate(year = y)

  multi_per <-
    bind_rows(
      fars.race %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:4)),
      fars.personrf %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:4)),
      fars.drugs %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:4)),
      fars.nmcrash %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:4)),
      fars.nmimpair %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:4)),
      fars.nmprior %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:4)),
      fars.nmdistract %>% mutate_all(as.character) %>% pivot_longer(cols = -c(1:4))
      ) %>%
    as.data.frame() %>%
    mutate(year = y)

  soe <-
    fars.vsoe %>%
    as.data.frame() %>%
    mutate(year = y)


# return ----

  write_csv(fars, paste0(prepared_dir, "/", y, "_flat.csv"))
  write_csv(multi_acc, paste0(prepared_dir, "/", y, "_multi_acc.csv"))
  write_csv(multi_veh, paste0(prepared_dir, "/", y, "_multi_veh.csv"))
  write_csv(multi_per, paste0(prepared_dir, "/", y, "_multi_per.csv"))
  write_csv(soe, paste0(prepared_dir, "/", y, "_events.csv"))

}
