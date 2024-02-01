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
# clean -------------------------------------------------------------------
# -------------------------------------------------------------------------
data_22 = raw_data %>%
  filter(year == "22") %>%
  filter(str_detect(gkz, "\\d{5}")) %>%
  pivot_wider(
    names_from=variable,
    values_from=val
  ) %>%
  select(gkz,Anzahl,Median) %>%
  mutate(Median = round(as.numeric(Median), 2)) %>%
  select(gkz, median_real=Median, anzahl=Anzahl)



# -------------------------------------------------------------------------
# write out ---------------------------------------------------------------
# -------------------------------------------------------------------------
op_realeinkommen_anzahl = makePath(here("output/article1/realeinkommen_anzahl.csv"))
write_csv(data_22, op_realeinkommen_anzahl)





