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
library(tidyr)

## ---- warning=FALSE-----------------------------------------------------------
myFARS <- get_fars(years = 2020, states = "VA")

## -----------------------------------------------------------------------------
myFARS$events %>%
  group_by(soe) %>% summarize(n=n()) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  
  ggplot(aes(x=n, y=reorder(soe, n), label=scales::comma(n))) +
    geom_col() +
    geom_label()

## -----------------------------------------------------------------------------
myFARS$events %>%
  select(-aoi) %>%
  pivot_wider(names_from = "veventnum", values_from = "soe", values_fill = "x",
              names_prefix = "event") %>%
  select(starts_with("event")) %>%
  group_by_all() %>%
  summarize(n=n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  select(event1, event2, n)
  

## ---- fig.height=7, fig.width=10----------------------------------------------
myFARS$events %>%
  group_by(year, state, st_case, veh_no) %>%
  dplyr::rename(event_to = soe) %>%
  mutate(event_from = data.table::shift(event_to, fill = "Pre-Crash")) %>%
  select(event_from, event_to) %>%
  group_by(event_from, event_to) %>% summarize(n=n()) %>%
  group_by(event_from) %>% mutate(n_from = sum(n)) %>%
  mutate(n_pct = n/n_from) %>%
  filter(n_pct>.1, n>5) %>%
  mutate(event_from = stringr::str_wrap(event_from, 40),
         event_to = stringr::str_wrap(event_to, 40)) %>%

  ggplot(aes(y=event_from, x=event_to, fill=n_pct, label=scales::percent(n_pct, accuracy = 1))) +
    viridis::scale_fill_viridis() +
    geom_label() +
    theme(
      axis.text.x.bottom = element_text(angle=270, hjust = 0, vjust=.5),
      legend.position = "none"
      )

