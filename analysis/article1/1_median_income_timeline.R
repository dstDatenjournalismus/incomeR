library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)

# loading this package
devtools::load_all()


# params ------------------------------------------------------------------

# consts for income data
sex = c("Zusammen")
type = c("Nettobezüge")
variable = c("Median", "Anzahl")

# consts for gemeindeänderungen
current_year_gemeinden = 2022
path_to_gem_aenderungen = here("~/projects/dst/DATEN/statistik_austria/Gemeindeaenderungen_ab2002bis2022.xlsx")
dir_with_excels_to_gemeindedaten = here("~/projects/dst/DATEN/statistik_austria/lst_daten/")
path_excel_bls = here("~/projects/dst/DATEN/statistik_austria/lst_daten/AN_gjvz_BL_2004-2022.xlsx")


# get the data ------------------------------------------------------------

# where to save the processed lst data
out_dir_lst_data = here("processed_lst_data/")

raw_data = incomeR::read_lst_data(
  dir_with_excels = dir_with_excels_to_gemeindedaten,
  sex = sex,
  type = type,
  variable = variable,
  out_dir = out_dir_lst_data
)

# bind all the years together
raw_data = bind_rows(raw_data)


# -------------------------------------------------------------------------
## Gemeindeänderungen laden
# -------------------------------------------------------------------------

out_dir_gem_changes = here("raw_gem_changes")
if (!dir.exists(out_dir_gem_changes)) {
  dir.create(out_dir_gem_changes)
}

historic_gemeindestaende = get_clean_gemeinden(current_year_gemeinden,
  path_to_aenderungen = path_to_gem_aenderungen,
  out_dir = out_dir_gem_changes,
  TEST = FALSE
)

# -------------------------------------------------------------------------
## Rohe Einkommensdaten und Gemeindeänderungen zusammenbringen
# -------------------------------------------------------------------------

# raw data as on the income sheet
historic_gem_income_data = get_historic_income(raw_data, historic_gemeindestaende)

# -------------------------------------------------------------------------
## Get the yearly inflation relative to the max year in the data
# -------------------------------------------------------------------------

max_year = historic_gem_income_data[[1]]$year %>% max
data_vpi = get_yearly_inflation(index_year = max_year)

# -------------------------------------------------------------------------
## Adjust data for inflation
# -------------------------------------------------------------------------
data_real_all = map(historic_gem_income_data, function(data_nom) {
  data_real = data_nom %>%
    pivot_wider(names_from = variable,
                values_from = val) %>%
    left_join(data_vpi, join_by(year)) %>%
    mutate(Median_real = round(Median / (index_2022 / 100), 2)) %>% janitor::clean_names()
  return(data_real)
}) %>%
  bind_rows() %>%
  select(gkz = gkz_current, name, year, median, median_real, anzahl)


data_real_all %>%
  group_by(gkz, year) %>%
  summarise(median_real_weighted = round(weighted.mean(median_real, anzahl), 2),
            agg = if_else(n() > 1, "1", "")) -> data_clean

# -------------------------------------------------------------------------
## Data for each Bundesland
# -------------------------------------------------------------------------

# bundeslaender
data_each_bl = processBlData(path = "~/projects/dst/DATEN/statistik_austria/lst_daten/AN_gjvz_BL_2004-2022.xlsx", sex=sex, type=type, variable="Median")

data_each_bl %>%
  mutate(year=as.numeric(year), agg="") %>%
  select(
    gkz = id,
    year,
    median_real_weighted = val_real,
    agg
  ) %>% filter(!is.na(gkz)) -> per_bl


data_clean_all = bind_rows(data_clean, per_bl) %>% ungroup() %>%
  rename(jahr = year, median_real = median_real_weighted)



# -------------------------------------------------------------------------
## Rausschreiben
# -------------------------------------------------------------------------

# one file each gemeinde
per_gkz = data_clean_all %>% split(.$gkz)
iwalk(per_gkz, function(x, nm){
  print(nm)
  op = makePath(here(glue("output/article1/zeitreihen_real/{nm}.csv")))
  d = x %>%
    select(-gkz)
  data.table::fwrite(d, op)
})



# -------------------------------------------------------------------------
# realeinkommen2022 -------------------------------------------------------
# -------------------------------------------------------------------------
realeinkommen2022 = data_clean_all %>%
  filter(str_detect(gkz, "\\d{5}") & jahr==2022) %>%
  select(-jahr, -agg)

op_realeinkommen = makePath(here("output/article1/realeinkommen.csv"))
write_csv(realeinkommen2022, op_realeinkommen)















