library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
library(readxl)

# data from here
# https://www.wko.at/zahlen-daten-fakten/verbraucherpreisindex#heading_Verbraucherpreisindex__VPI____Indexentwicklung
# Table: VPI Veränderungsraten gegenüber Vorjahresmonaten bzw Vorjahren ab 1967

path_to_data = here("data/vpi.xlsx")

# index year --------------------------------------------------------------
index_year = 2022
minimal_year = 2004


# read data ---------------------------------------------------------------
data_vpi_raw = read_xlsx(path_to_data, skip = 8) %>% janitor::clean_names()

usethis::use_data(data_vpi_raw)

data %>%
  filter(!is.na(jahres_durch_schnitt)) %>%
  select(jahr, vpi = jahres_durch_schnitt) %>%
  mutate(jahr = as.numeric(jahr)) %>%
  filter(jahr >= minimal_year) %>%
  arrange(desc(jahr)) %>%
  mutate(
    vpi_cum = cumsum(vpi),
    vpi_rel_to_index = if_else(jahr == index_year, 100, 100 - lag(vpi_cum))
  )  -> vpi_index_2022


op_data_vpi_clean = here("data/data_vpi_clean.csv")
usethis::use_data()


