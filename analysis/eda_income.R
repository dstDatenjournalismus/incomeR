library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(DT)


# loading this package
devtools::load_all()

## Raw data
out_dir_lst_data = here("processed_lst_data")
if(!dir.exists(out_dir_lst_data)){
  dir.create(out_dir_lst_data)
}

raw_data = incomeR::read_lst_data(
  dir_with_excels = here("~/projects/dst/DATEN/statistik_austria/lst_daten/"),
  sex = "Zusammen",
  type = "Nettobezüge",
  variable = c("Median"),
  out_dir = out_dir_lst_data
)

# bind all the years together
raw_data = bind_rows(raw_data)



## Gemeindeänderungen laden
out_dir_gem_changes = here("raw_gem_changes")
if(!dir.exists(out_dir_gem_changes)){
  dir.create(out_dir_gem_changes)
}

## Double Check Gnas! Shoukd have Zusammenlegungen, but doesnt!!
historic_gemeindestaende = get_clean_gemeinden(2021,
                                   path_to_aenderungen = here("data/Gemeindeaenderungen_ab2002bis2022.xlsx"),
                                   out_dir=out_dir_gem_changes,
                                   TEST = FALSE)



## Rohe Einkommensdaten und Gemeindeänderungen zusammenbringen
historic_gem_income_data = get_historic_income(raw_data, historic_gemeindestaende)
names(historic_gem_income_data) = map_chr(historic_gemeindestaende, ~.x$gkz[[1]])
