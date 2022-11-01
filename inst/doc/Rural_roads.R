## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE, warning=FALSE, message=FALSE--------------------------
library(rfars)
library(dplyr)
library(ggplot2)
library(magrittr)

## ---- warning=FALSE-----------------------------------------------------------
myFARS <- get_fars(years = 2018:2020, states = "VA")

## -----------------------------------------------------------------------------
myFARS$flat$per_typ <- 
  ifelse(grepl("motorcycle", 
               myFARS$flat$body_typ, 
               ignore.case = TRUE),
         "Motorcyclist",
         myFARS$flat$per_typ)

## -----------------------------------------------------------------------------
compare_counts <- function(myFARS, what, involved=NULL){
  
  bind_rows(
    counts(myFARS, what=what, involved=involved, where="rural") %>%
      mutate(where = "Rural"),
    counts(myFARS, what=what, involved=involved, where="urban") %>%
      mutate(where = "Urban")
    ) %>%
  return()
  
}

## -----------------------------------------------------------------------------
compare_counts(myFARS, "crashes") %>%

ggplot(aes(x=date, y=n, label=scales::comma(n))) + 
  geom_col() + 
  geom_label(vjust=1) +
  facet_wrap(.~where) +
  labs(x=NULL, y=NULL, title = "Crashes", fill=NULL)

## -----------------------------------------------------------------------------
compare_counts(myFARS, "fatalities") %>%
  
ggplot(aes(x=date, y=n, label=scales::comma(n))) + 
  geom_col() + 
  geom_label(vjust=1) +
  facet_wrap(.~where) +
  labs(x=NULL, y=NULL, title = "Fatalities", fill=NULL)

## ---- warning=FALSE-----------------------------------------------------------
crashfactors <- c("distracted driver", "drowsy driver", 
                  "police pursuit", "motorcycle", "pedalcyclist", 
                  "bicyclist", "pedestrian", "pedbike", 
                  "young driver", "older driver", "speeding", 
                  "alcohol", "drugs", "hit and run", 
                  "roadway departure", "rollover", "large trucks"
                  )

for(crashfactor in crashfactors){
  
  p <- 
    compare_counts(myFARS, "fatalities", involved = crashfactor) %>%
    ggplot(aes(x=year, y=n, label=scales::comma(n))) +
      geom_col(position="dodge") +
      facet_wrap(.~where) +
      geom_label(position = position_dodge(.9), vjust=1) +
      labs(title = paste0("Fatalities: ", crashfactor))

  print(p)
  
}

## -----------------------------------------------------------------------------
bind_rows(
  counts(myFARS,
       what = "crashes",
       where = "rural",
       filterOnly = TRUE
       ) %>%
    filter(veh_no==1) %>% #crash type is on the vehicle-level, this prevents over-counting
    select(id, year, acc_type) %>% unique() %>% group_by(acc_type, year) %>% summarize(n=n()) %>%
    mutate(where = "Rural"),
  counts(myFARS,
       what = "crashes",
       where = "urban",
       filterOnly = TRUE
       ) %>%
    filter(veh_no==1) %>%
    select(id, year, acc_type) %>% unique() %>% group_by(acc_type, year) %>% summarize(n=n()) %>%
    mutate(where = "Urban")
    ) %>%
  filter(!is.na(acc_type)) %>%
  group_by(where, acc_type) %>% summarize(n=sum(n, na.rm=TRUE)) %>%
  tidyr::pivot_wider(names_from = "where", values_from = "n") %>%
  mutate(Total = Urban + Rural,
         rural_pct = Rural/Total) %>%
  arrange(desc(Total)) %>%
  slice(1:20) %>%
  arrange(desc(rural_pct)) %>%
  mutate(acc_type = reorder(acc_type, rural_pct)) %>%
  
  ggplot(aes(y=acc_type, x=rural_pct, fill=Rural, label=scales::percent(rural_pct, accuracy = 1))) + 
    geom_col() + 
    geom_label(hjust=1, fill="white") +
  scale_fill_continuous(labels=scales::comma) +
    labs(x=NULL, y=NULL, 
         title = "20 Most Common Crash Types by Prevalence in Rural Areas") +
    theme(plot.title.position = "plot")

## -----------------------------------------------------------------------------
myFARS$flat %>%
  mutate(
    vprofile = ifelse(vprofile %in% c("Uphill", "Downhill"), "Up/downhill", vprofile),
    valign = ifelse(grepl("Curve", valign), "Curve", valign)
    ) %>%
  filter(veh_no == 1, #to avoid over-counting
         rur_urb %in% c("Rural", "Urban"),
         valign %in% c("Straight", "Curve"),
         !(vprofile %in% c("Unknown", "Reported as Unknown", "Not Reported"))
         ) %>%
  select(id, vprofile, valign, rur_urb) %>% unique() %>%
  group_by(vprofile, valign, rur_urb) %>%
  summarize(n = n()) %>%
  
ggplot(aes(x=valign, y=vprofile, fill=n, label=scales::comma(n))) +
  #geom_tile() +
  facet_wrap(.~rur_urb) +
  viridis::scale_fill_viridis() +
  geom_label() +
  labs(title = "Roadway Profile and Alignment")


## -----------------------------------------------------------------------------
myFARS$flat %>%
  filter(rur_urb %in% c("Rural", "Urban")) %>%
  filter(grepl("(K)", inj_sev)) %>%
  group_by(rur_urb, per_typ) %>%
  summarise(n=n()) %>%
  filter(n>2) %>%
  mutate(per_typ = stringr::str_wrap(per_typ, 15)) %>%
  
  ggplot(aes(x=per_typ, y=n, fill=rur_urb, label = scales::comma(n))) +
    geom_col(position = "dodge") +
    geom_label(vjust=1, position = position_dodge(.9)) +
    labs(title = "Fatalities by Person Type and Urbanicity")

## -----------------------------------------------------------------------------
myFARS$flat %>%
  filter(rur_urb %in% c("Rural", "Urban")) %>%
  filter(grepl("(K)", inj_sev)) %>%
  group_by(rur_urb, sex) %>%
  summarise(n=n()) %>%
  filter(n>90) %>%
  mutate(sex = stringr::str_wrap(sex, 15)) %>%
  
  ggplot(aes(x=sex, y=n, fill=rur_urb, label = scales::comma(n))) +
    geom_col(position = "dodge") +
    geom_label(vjust=1, position = position_dodge(.9)) +
    labs(title = "Fatalities by Sex and Urbanicity")

## -----------------------------------------------------------------------------
myFARS$flat %>%
  filter(rur_urb %in% c("Rural", "Urban")) %>%
  filter(grepl("(K)", inj_sev)) %>%
  group_by(rur_urb, hispanic) %>%
  summarise(n=n()) %>%
  filter(n>10) %>%
  mutate(hispanic = stringr::str_wrap(hispanic, 15)) %>%
  
  ggplot(aes(x=hispanic, y=n, fill=rur_urb, label = scales::comma(n))) +
    geom_col(position = "dodge") +
    geom_label(vjust=1, position = position_dodge(.9)) +
    labs(title = "Fatalities by Ethnicity and Urbanicity")

## -----------------------------------------------------------------------------
myFARS$flat %>%
  filter(rur_urb %in% c("Rural", "Urban")) %>%
  filter(grepl("(K)", inj_sev)) %>%
  filter(!(per_typ %in% c("Bicyclist", "Pedestrian"))) %>%
  group_by(rur_urb, body_typ) %>%
  summarise(n=n()) %>%
  filter(n>30) %>%
  mutate(body_typ = stringr::str_wrap(body_typ, 80)) %>%

  ggplot(aes(y=body_typ, x=n, fill=rur_urb, label=scales::comma(n, accuracy = 1))) + 
    geom_col(position = "dodge") + 
    geom_label(hjust=1, position = position_dodge(.9)) +
    labs(title = "Fatalities by Vehicle Type and Urbanicity")

## -----------------------------------------------------------------------------
myFARS$flat %>%
  filter(grepl("(K)", inj_sev), 
         rur_urb %in% c("Rural", "Urban")) %>%
  mutate(age_n = gsub("\\D+","", age) %>% as.numeric()) %>%
  group_by(rur_urb, age_n) %>% summarize(n=n()) %>%
  filter(age_n <=90) %>%
  
  ggplot(aes(x=age_n, y=n, color = rur_urb)) +
    geom_line(size=1.2, alpha=.8) +
    labs(title = "Fatalities by Age and Urbanicity")

## -----------------------------------------------------------------------------
myFARS$flat %>%
  mutate(age_n = gsub("\\D+","", age) %>% as.numeric()) %>%
  filter(grepl("(K)", inj_sev),
         rur_urb %in% c("Rural", "Urban"),
         hour < 25,
         age_n <= 90) %>%
  group_by(rur_urb, age_n, hour) %>% summarize(n=n()) %>%
  
  ggplot(aes(x=hour, y=age_n, fill=n)) +
    geom_tile() +
    facet_wrap(.~rur_urb) +
    viridis::scale_fill_viridis() +
    labs(title = "Fatalities by Age, Time of Day, and Urbanicity")

## -----------------------------------------------------------------------------
myFARS$multi_per %>% 
  filter(name == "race") %>%
  select(state, st_case, veh_no, per_no, year, race=value) %>%
  inner_join(myFARS$flat) %>%
  
  filter(rur_urb %in% c("Rural", "Urban")) %>%
  filter(grepl("(K)", inj_sev)) %>%
  group_by(rur_urb, race) %>%
  summarise(n=n()) %>%
  filter(n>9) %>%
  mutate(race = stringr::str_wrap(race, 15)) %>%
  
  ggplot(aes(x=race, y=n, fill=rur_urb, label = scales::comma(n))) +
    geom_col(position = "dodge") +
    geom_label(vjust=1, position = position_dodge(.9))

