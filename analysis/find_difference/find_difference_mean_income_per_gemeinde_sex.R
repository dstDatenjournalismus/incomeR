library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)

devtools::load_all()


# data --------------------------------------------------------------------
dir_with_excels_to_gemeindedaten = here("~/projects/dst/DATEN/statistik_austria/lst_daten/")
path_to_gem_aenderungen = here("~/projects/dst/DATEN/statistik_austria/Gemeindeaenderungen_ab2002bis2022.xlsx")

out_dir_lst_data = here("processed_lst_data")
if (!dir.exists(out_dir_lst_data)) {
  dir.create(out_dir_lst_data)
}

# mean 2004 ---------------------------------------------------------------
raw_data = incomeR::read_lst_data(
  dir_with_excels = dir_with_excels_to_gemeindedaten,
  sex = c("Männer", "Frauen"),
  type = "Nettobezüge",
  variable = c("Mittelwert", "Anzahl"),
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

historic_gemeindestaende = get_clean_gemeinden_raw(
  year_gemeinde_data = 2022,
  path_to_aenderungen = path_to_gem_aenderungen,
  out_dir = out_dir_gem_changes,
  TEST = FALSE
)


# -------------------------------------------------------------------------
## Rohe Einkommensdaten und Gemeindeänderungen zusammenbringen
# -------------------------------------------------------------------------
historic_gem_income_data = get_historic_income(raw_data, historic_gemeindestaende)



# pro gemeinde und jahr ein wert ------------------------------------------
dfs = map(seq_along(historic_gem_income_data), function(i){
  print(i)

  # all income data for that gemeinde
  gem_data = historic_gem_income_data[[i]]

  gem_data_wide = gem_data %>%
    pivot_wider(
      names_from = variable,
      values_from = val
    )

  # weighted mean Anzahl
  gem_data_wide %>%
    group_by(year, gkz_current) %>%
    summarise(mean_val = weighted.mean(Mittelwert, Anzahl), name=first(name)) %>%
    ungroup() %>%
    mutate(
      diff_to_year_before_abs = mean_val - dplyr::lag(mean_val),
      diff_to_year_before_rel = mean_val / dplyr::lag(mean_val, 1),
    ) %>%
    summarise(
      name = first(name),
      diff_abs = mean_val[year == max(year)] - mean_val[year == min(year)],
      diff_rel = mean_val[year == max(year)] / mean_val[year == min(year)],
      max_yearly_change_abs = max(diff_to_year_before_abs, na.rm = T),
      max_yearly_change_rel = max(diff_to_year_before_rel, na.rm = T),
      min_yearly_change_abs = min(diff_to_year_before_abs, na.rm = T),
      min_yearly_change_rel = min(diff_to_year_before_rel, na.rm = T),
      yearly_mean_change_abs = mean(diff_to_year_before_abs, na.rm=T),
      yearly_mean_change_rel = mean(diff_to_year_before_rel, na.rm=T),
      gkz_current = first(gkz_current)
    ) -> df

  df

})

all = dfs %>% list_rbind()



# eda ---------------------------------------------------------------------
all %>%
  filter(yearly_mean_change_rel == min(yearly_mean_change_rel))

all %>%
  mutate(
    yearly_mean_change_rel = (yearly_mean_change_rel - 1) * 100
  ) -> all


# write out ---------------------------------------------------------------
out_dir_diff = makePath(here("diff_income/mean_diff_income.csv"))
write_csv(all, out_dir_diff)














