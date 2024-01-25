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
variable = c("Median")

# consts for gemeindeänderungen
current_year_gemeinden = 2022
path_to_gem_aenderungen = here("~/projects/dst/DATEN/statistik_austria/Gemeindeaenderungen_ab2002bis2022.xlsx")
dir_with_excels_to_gemeindedaten = here("~/projects/dst/DATEN/statistik_austria/lst_daten/")
path_excel_bls = here("~/projects/dst/DATEN/statistik_austria/lst_daten/AN_gjvz_BL_2004-2022.xlsx")


# get the data ------------------------------------------------------------
raw_data = incomeR::read_lst_data(
  dir_with_excels = dir_with_excels_to_gemeindedaten,
  sex = sex,
  type = type,
  variable = variable,
  out_dir = out_dir_lst_data
)

# bind all the years together
raw_data = bind_rows(raw_data)


# data for 2022 -----------------------------------------------------------
raw_data %>%
  filter(year == "22") %>%
  filter(str_detect(gkz, "\\d{5}")) -> median_netto_2022_zusammen


# get the names -----------------------------------------------------------
gems2022 = rajudas::oe_gem_data(2022) %>% st_drop_geometry()
df_final = left_join(gems2022, median_netto_2022_zusammen, join_by(id == gkz))



# write out ---------------------------------------------------------------
op_median_2022_zusammen = makePath(here("output/article1/median_netto_zusammen_2022.csv"))
write_csv(df_final, op_median_2022_zusammen)


# some facts --------------------------------------------------------------
df_final %>%
  arrange(val)



