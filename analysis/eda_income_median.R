library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(DT)
library(plotly)

# loading this package
devtools::load_all()

# set ggplot theme
ggplot2::theme_set(theme_gray())


# -------------------------------------------------------------------------
# PARAMS
# -------------------------------------------------------------------------

# consts for income data
sex = c("Zusammen", "Männer", "Frauen")
type = c("Nettobezüge")
variable = c("Median")

# consts for gemeindeänderungen
current_year_gemeinden = 2022
path_to_gem_aenderungen = here("~/projects/dst/DATEN/statistik_austria/Gemeindeaenderungen_ab2002bis2022.xlsx")
dir_with_excels_to_gemeindedaten = here("~/projects/dst/DATEN/statistik_austria/lst_daten/")
path_excel_bls = here("~/projects/dst/DATEN/statistik_austria/lst_daten/AN_gjvz_BL_2004-2022.xlsx")

# -------------------------------------------------------------------------
## Raw income data
# -------------------------------------------------------------------------

out_dir_lst_data = here("processed_lst_data")
if (!dir.exists(out_dir_lst_data)) {
  dir.create(out_dir_lst_data)
}

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
historic_gem_income_data = get_historic_income(raw_data, historic_gemeindestaende)


# -------------------------------------------------------------------------
# do some plots -----------------------------------------------------------
# -------------------------------------------------------------------------

hide = function() {
  # most changes
  max_nrow_index = which.max(map_dbl(historic_gem_income_data, nrow))
  max_nrow_data = historic_gem_income_data[[max_nrow_index]]

  p = ggplot(max_nrow_data, aes(year,
    val,
    color = gkz
  )) +
    geom_line() +
    geom_point()


  ggplotly(p)

  # ratios
  ratios = map_dbl(historic_gem_income_data, function(g) {
    g %>%
      filter(
        year == max(year) |
          year == min(year)
      ) %>%
      group_by(year) %>%
      summarise(med = median(val, na.rm = T)) %>%
      mutate(ratio = med[year == max(year)] / med[year == min(year)]) %>%
      filter(year == max(year)) %>%
      pull(ratio)
  })

  # max and min
  historic_gem_income_data[c(which.max(ratios), which.min(ratios))] %>%
    bind_rows() %>%
    ggplot(aes(year,
      val,
      color = name
    )) +
    geom_line() +
    geom_point() +
    ylim(0, NA)

  # all
  historic_gem_income_data %>%
    bind_rows() %>%
    group_by(gkz_current) %>%
    ungroup() %>%
    ggplot(aes(year,
      val,
      group = gkz_current
    )) +
    geom_line(show.legend = F, alpha = .1) +
    theme_minimal()
}




# -------------------------------------------------------------------------
# Save the centroids of the gemeinden
# -------------------------------------------------------------------------
op_dir = rajudas::makePath("~/projects/dst/2023/setembro/2023-09-einkommens-karte/public/data/centroids/")
if(!dir.exists(op_dir)){
  dir.create(op_dir, recursive = T)
}

oe_gem_data = rajudas::oe_gem_data(current_year_gemeinden)
oe_gem_data = oe_gem_data %>% st_transform(crs=4326) %>% st_make_valid()
oe_gem_data_centroids = st_centroid(oe_gem_data)
walk(1:nrow(oe_gem_data_centroids), function(r){
  row = oe_gem_data_centroids[r, ] %>%
    mutate(
      x = st_coordinates(.)[,1],
      y = st_coordinates(.)[,2]
    ) %>% st_drop_geometry()
  op = glue("{op_dir}/{row$id}.csv")
  write_csv(row, op)
})




# -------------------------------------------------------------------------
# Save the output
# -------------------------------------------------------------------------

# save the data for each year
makeYearData("~/projects/dst/2023/setembro/2023-09-einkommens-karte/public/data/", data = historic_gem_income_data)

# save the data for each gemeinde
makeGemeindeData("~/projects/dst/2023/setembro/2023-09-einkommens-karte/public/data/", data = historic_gem_income_data)



# -------------------------------------------------------------------------
# Look at the gemeinden
# -------------------------------------------------------------------------
processBlData(path_excel_bls, out_file=here("processed_lst_data/bundesländer_daten.csv"))


# -------------------------------------------------------------------------
# data for tooltip --------------------------------------------------------
# -------------------------------------------------------------------------

makeDataForTooltip(year = 2021,
                   dir_yearly_data = "~/projects/dst/2023/setembro/2023-09-einkommens-karte/public/data/all/",
                   dir_bl_data = "~/projects/dst/2023/setembro/2023-09-einkommens-karte/public/data/bundeslaender/")








