library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
library(ggbeeswarm)
library(packcircles)


# loading this package
devtools::load_all()


# params ------------------------------------------------------------------

# consts for income data
sex = c("Zusammen", "Männer", "Frauen")
type = c("Nettobezüge")
variable = c("Mittelwert")

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


# data for 2023 for men and women -----------------------------------------
d = raw_data %>%
  filter(str_detect(gkz, "\\d{5}")) %>%
  filter(sex != "Zusammen") %>%
  filter(year == "22")


# get the population ------------------------------------------------------
pop = oe_pop_data()
pop22 = pop %>% select(gkz, "2022")
dpop = left_join(d, pop22, join_by(gkz==gkz)) %>% mutate(id=row_number())



# normal beeswarm ---------------------------------------------------------
bee = ggplot(dpop, aes(x="x", y=val, color=sex)) +
  geom_beeswarm()

bee_data = ggplot_build(bee)


new_bee = data.frame(
  x = bee_data$data[[1]]$x,
  y = bee_data$data[[1]]$y,
  r = dpop$`2022`
)

newbee_repel = circleRepelLayout(new_bee, wrap=FALSE)
newbee_repel_out = circleLayoutVertices(newbee_repel$layout, xysizecols = 1:3)
newbee_repel_out = left_join(newbee_repel_out, dpop %>% select(sex, id), join_by(id==id))


newbee_repel_out %>% ggplot(aes(x, y, group=id)) +
  geom_polygon(aes(fill=sex), color="black", show.legend = T) +
  coord_equal() +
  theme_minimal() +
  labs(title="", fill=NULL)

