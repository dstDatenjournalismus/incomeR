library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)

# params ------------------------------------------------------------------

# consts for income data
sex = c("Männer", "Frauen")
type = c("Nettobezüge")
variable = c("Median")

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
## Only use data for 2022
# -------------------------------------------------------------------------
data_men_woman_22 = raw_data %>%
  mutate(
    year = as.numeric(glue("20{year}"))
  ) %>%
  filter(year==2022)


data_men_woman_wide = data_men_woman_22 %>%
  filter(str_detect(gkz, "\\d{5}")) %>%
  select(gkz, val,sex) %>%
  pivot_wider(names_from = sex, values_from = val)



# -------------------------------------------------------------------------
# for datawrapper choropleth map ------------------------------------------
# -------------------------------------------------------------------------
data_diff_22 = data_men_woman_wide %>%
  mutate(
    across(2:3, as.numeric),
    diff = Männer - Frauen,
    class = if_else(diff > 0, "Männer mehr verdienen", "Frauen mehr verdienen"),
    text = glue("<span style='text-align: center; font-size: 1rem; display: grid;'><span>Frauen verdienen</span><span style='text-align: center; font-size: 1.1rem;'>{format(abs(round(diff, 2)), big.mark='.', decimal.mark=',')} €</span><span style='font-size: 1rem; color:{ifelse(diff > 0, '#e59590', '#000')}'>{ifelse(diff > 0, 'weniger', 'mehr')}</span></span>")) %>%
  filter(!is.na(diff))

op = makePath(here("output/article1/4_men_woman/diff_men_woman_2022_median_netto.csv"))
write_csv(data_diff_22, op)









