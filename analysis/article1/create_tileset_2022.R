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
out_dir_lst_data = here("processed_lst_data/")


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
# gems2022 = rajudas::oe_gem_data(2022)
gems2022 = read_sf(here("data/gemeinden_2022_clipped_bodensee.fgb"))

df_final = left_join(gems2022, median_netto_2022_zusammen, join_by(id == gkz))


df_tileset = df_final %>%
  select(gkz=id, name, median_real=val) %>%
  mutate(
    median_real = as.numeric(median_real),
    median_real = round(median_real, 2)
  )

op_tileset_geo = makePath(here("output/article1/tileset_geo/gemeinden_einkommen_median_real_2022.geojson"))
unlink(op_tileset_geo)
df_tileset %>% st_transform("EPSG:4326") %>% write_sf(op_tileset_geo)



# -------------------------------------------------------------------------
# list all existing sources
# -------------------------------------------------------------------------
user="derstandardat"
key = read_lines("~/.ssh/mapbox")
Sys.setenv(MAPBOX_ACCESS_TOKEN=key)

list_cmd = glue("tilesets list-sources {user}")
system(list_cmd)

# -------------------------------------------------------------------------
# create tileset source
# -------------------------------------------------------------------------
name_tileset = "gemeinden_einkommen_2022_clipped"
dir_geo = dirname(op_tileset_geo)
setwd(dir_geo)
tileset_source_cmd = glue("tilesets upload-source --replace {user} {name_tileset} {basename(op_tileset_geo)}")
system(tileset_source_cmd)



# -------------------------------------------------------------------------
# create (read) recipe
# -------------------------------------------------------------------------
# read recilpe
path_json_recipe = here("analysis/article1/tileset_einkommen_2022.json")
jsonlite::fromJSON(path_json_recipe) %>% jsonlite::toJSON()


# -------------------------------------------------------------------------
# create empty tileset
# -------------------------------------------------------------------------
create_cmd = glue("tilesets create {user}.{name_tileset} --recipe {path_json_recipe} --name 'gemeinden_einkommen_median_real_2022'")
system(create_cmd)

# -------------------------------------------------------------------------
# publish empty tileset
# -------------------------------------------------------------------------
publish_cmd = glue("tilesets publish {user}.{name_tileset}")
system(publish_cmd)


















