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
min_year = historic_gem_income_data[[1]]$year %>% min
data_vpi = get_yearly_inflation(upper_index_year = max_year, lower_index_year = )

# # -------------------------------------------------------------------------
# ## Adjust data for inflation
# # -------------------------------------------------------------------------
# data_real_all = map(historic_gem_income_data, function(data_nom) {
#   data_real = data_nom %>%
#     pivot_wider(names_from = variable,
#                 values_from = val) %>%
#     left_join(data_vpi, join_by(year)) %>%
#     mutate(Median_real = round(Median / (index_2022 / 100)), 2) %>% janitor::clean_names()
#   return(data_real)
# }) %>%
#   bind_rows() %>%
#   select(gkz = gkz_current, name, year, median, median_real, anzahl)


# -------------------------------------------------------------------------
# one value for each year -------------------------------------------------
# -------------------------------------------------------------------------
library(furrr)
plan(multisession, nworker=4)
data_nominal_income_vpi_index_2004 =  future_map(historic_gem_income_data, function(gem) {
  gem %>%
    pivot_wider(names_from = variable, values_from = val) %>%
    group_by(gkz, year) %>%
    summarise(median_nominal = weighted.mean(Median, Anzahl),
              agg = if_else(n() > 1, "1", "")) %>%
    left_join(data_vpi, join_by(year)) %>%
    select(-vpi, -index_2022) %>%
    rename(cumulative_vpi_2004 = index_2004) %>%
    ungroup() %>%
    mutate(
      median_nominal_index_2004 = median_nominal / median_nominal[year == 2004],
      cumulative_vpi_index_2004 = cumulative_vpi_2004 / cumulative_vpi_2004[year ==
                                                                              2004]
    ) %>%
    select(gkz, year, matches("index")) -> nominal_income_vpi_index_2004

  nominal_income_vpi_index_2004
})


# bundeslaender
data_each_bl = processBlData(path = "~/projects/dst/DATEN/statistik_austria/lst_daten/AN_gjvz_BL_2004-2022.xlsx", sex=sex, type=type, variable="Median")

data_each_bl %>%
  mutate(year = as.numeric(year), agg="") %>%
  select(
    gkz = id,
    jahr = year,
    median_real = val_real,
    median_nominal = val,
    agg
  ) -> data_per_bl




# -------------------------------------------------------------------------
# put them together -------------------------------------------------------
# -------------------------------------------------------------------------
data_final = bind_rows(data_per_gemeinde, data_per_bl)

# -------------------------------------------------------------------------
# write them out ----------------------------------------------------------
# -------------------------------------------------------------------------
data_final_split = data_final %>% split(.$gkz)

imap(data_final_split, function(x, nm){
  print(nm)
  op = makePath(here(glue("output/article1/zeitreihen_real_nominal/{nm}.csv")))
  d = x %>%
    select(-gkz)
  data.table::fwrite(d, op)
})























