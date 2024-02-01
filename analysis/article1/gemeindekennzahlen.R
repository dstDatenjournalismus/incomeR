library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)


# gems 22 -----------------------------------------------------------------
gems_22 = rajudas::oe_gem_data(2022)

d = gems_22 %>%
  st_drop_geometry() %>%
  rename(gkz=id)

op = makePath(here("output/article1/gemeindekennzahlen.csv"))
write_csv(d, op)
