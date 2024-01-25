library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)


# consts for income data
sex = c("Männer", "Frauen")
type = c("Nettobezüge")
variable = c("Median")

# consts for gemeindeänderungen
current_year_gemeinden = 2022
path_to_gem_aenderungen = here("~/projects/dst/DATEN/statistik_austria/Gemeindeaenderungen_ab2002bis2022.xlsx")
dir_with_excels_to_gemeindedaten = here("~/projects/dst/DATEN/statistik_austria/lst_daten/")
path_excel_bls = here("~/projects/dst/DATEN/statistik_austria/lst_daten/AN_gjvz_BL_2004-2022.xlsx")

# data for bundesländer ---------------------------------------------------
data_each_bl = processBlData(path = "~/projects/dst/DATEN/statistik_austria/lst_daten/AN_gjvz_BL_2004-2022.xlsx", sex=sex,type=type, variable=variable)


data_each_bl %>%
  filter(bl == "Österreich") %>%
  select(bl, sex, year, val_real) %>%
  pivot_wider(
    names_from = sex,
    values_from = val_real
  ) %>% select(-bl) -> men_woman_austria_over_time

op = makePath(here("output/article1/4_men_woman/men_woman_austria_over_time.csv"))
write_csv(men_woman_austria_over_time, op)

