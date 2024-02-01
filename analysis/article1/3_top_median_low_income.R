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
variable = c("P10", "P20", "Median", "P80", "P90", "Anzahl")

# consts for gemeindeänderungen
current_year_gemeinden = 2022
path_to_gem_aenderungen = here("~/projects/dst/DATEN/statistik_austria/Gemeindeaenderungen_ab2002bis2022.xlsx")
dir_with_excels_to_gemeindedaten = here("~/projects/dst/DATEN/statistik_austria/lst_daten/")
path_excel_bls = here("~/projects/dst/DATEN/statistik_austria/lst_daten/AN_gjvz_BL_2004-2022.xlsx")



# -------------------------------------------------------------------------
# get the data ------------------------------------------------------------
# -------------------------------------------------------------------------

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
historic_top_low_data = get_historic_income(raw_data, historic_gemeindestaende)


# clean and adjust for inflation -------------------------------------------
max_year = historic_top_low_data[[1]]$year[[1]]
data_vpi = get_yearly_inflation(max_year)

# -------------------------------------------------------------------------
# adjust for inflation
# -------------------------------------------------------------------------
data_top_low_adjusted = map(seq_along(historic_top_low_data), function(i){

  d = historic_top_low_data[[i]]

  d %>%
    left_join(data_vpi, join_by(year)) %>%
    mutate(val_real = if_else(variable == "Anzahl", val, round(val / (index_2022 / 100),2))) %>%
    select(year, gkz_current, name,  val_real, variable) %>%
    pivot_wider(names_from = variable,
                values_from = val_real) -> data_clean

  return(data_clean)
})


# -------------------------------------------------------------------------
# get the weighted quantiles ----------------------------------------------
# -------------------------------------------------------------------------
data_agg = map(seq_along(data_top_low_adjusted), function(i) {
  d_adj = data_top_low_adjusted[[i]]
  name = d_adj %>% filter(year == 2022) %>% pull(name)
  d_adj %>%
    group_by(year, gkz_current) %>%
    summarise(across(Median:P90, function(x) {
      round(weighted.mean(x, Anzahl), 2)
    }),
    agg = if_else(n() > 1, "1", "")) %>% ungroup() -> d_adj_agg

  return(d_adj_agg)
})





# -------------------------------------------------------------------------
# data for bundesländer ---------------------------------------------------
# -------------------------------------------------------------------------

data_each_bl = processBlData(path = "~/projects/dst/DATEN/statistik_austria/lst_daten/AN_gjvz_BL_2004-2022.xlsx", sex=sex,type=type, variable=variable)

data_bls = data_each_bl %>%
  select(gkz_current=id,val_real, variable,year) %>%
  pivot_wider(names_from = variable,values_from = val_real) %>%
  select(-Anzahl) %>%
  filter(!is.na(gkz_current)) %>%
  mutate(
    year=as.numeric(year)
  )



# -------------------------------------------------------------------------
# join all ----------------------------------------------------------------
# -------------------------------------------------------------------------

cols_def = c("gkz" = as.character, "year" = as.numeric, "P10" = as.numeric, "P20" = as.numeric, "Median" = as.numeric, "P80" = as.numeric, "P90" = as.numeric, "agg" = as.character)

data.table::rbindlist(data_agg) %>%
  rename(gkz=gkz_current) %>%
  ungroup() %>%
  select(all_of(names(cols_def)), agg) %>%
  imap(function(d, nm){
    d = cols_def[[nm]](d)
    return(d)
  }) %>%  bind_cols() -> data_gemeinden_clean

data_bls %>%
  rename(gkz = gkz_current) %>%
  mutate(agg = "") %>%
  select(all_of(names(cols_def))) %>%
  imap(function(d, nm) {
    d = cols_def[[nm]](d)
    return(d)
  }) %>%  bind_cols() -> data_bls_clean

data_all = bind_rows(data_gemeinden_clean, data_bls_clean) %>%
  rename(jahr = year) %>%
  rename_with(function(x) {
    return(paste0(x, "_real"))
  }, P10:P90) %>%
  janitor::clean_names()


# -------------------------------------------------------------------------
# write out ---------------------------------------------------------------
# -------------------------------------------------------------------------
data_p90_p10 = data_all %>% select(gkz, jahr, p10_real, median_real, p90_real, agg)
data_p80_p20 = data_all %>% select(gkz, jahr, p20_real, median_real, p80_real, agg)

data_all_list = list(median_p90_p10 = data_p90_p10, median_p80_p20  = data_p80_p20)


iwalk(data_all_list, function(d, name_mode) {
  data_split = d %>% split(.$gkz)

  iwalk(data_split, function(d_gem, name_gem) {
    d = d_gem %>% select(-gkz)
    op = makePath(here(glue(
      "output/article1/{name_mode}/{name_gem}.csv"
    )))
    data.table::fwrite(d, op)
  })
})


# -------------------------------------------------------------------------
# some analytics ----------------------------------------------------------
# -------------------------------------------------------------------------

# analize change over time for each ---------------------------------------
changes_rel = map(seq_along(data_agg), function(i){

  data_one_gem = data_agg[[i]]

  data_one_gem %>%
    mutate(ratio_90_10 = P90 / P10) %>%
    select(year, gkz_current, name, ratio_90_10) %>%
    ungroup() %>%
    pivot_wider(
      names_from = year,
      values_from = ratio_90_10
    ) -> d
  return(d)
}) %>% bind_rows()

changes_rel %>%
  mutate(
    last_to_first = `2022` / `2004`,
    .before=`2004`
  ) -> changes


op_change_top_low = makePath(here("output/article1/top_low/ratio_90_10_per_year.csv"))
write_csv(changes, op_change_top_low)

### absolute
changes_abs = map(seq_along(data_agg), function(i){

  data_one_gem = data_agg[[i]]

  data_one_gem %>%
    mutate(diff_90_10 = P90 - P10) %>%
    select(year, gkz_current, name, diff_90_10) %>%
    ungroup() %>%
    summarise(gkz=first(gkz_current), name=first(name), diff_last_first = diff_90_10[year=="2022"] - diff_90_10[year=="2004"]) -> d

  return(d)
}) %>% bind_rows()

changes_abs %>%
  mutate(
    text = glue("In <strong>{name}</strong> is die Differenz zwischen dem 90- und 10-%-Dezil in 2022 um<br><span style='font-size: 1.1rem'>{abs(round(diff_last_first,2))}€</span> {ifelse(diff_last_first > 0, 'höher', 'geringer')}<br>als in 2022")
  ) -> changes_abs

op_change_top_low_abs = makePath(here("output/article1/top_low/abs_90_10_one_val.csv"))
write_csv(changes_abs, op_change_top_low_abs)

















