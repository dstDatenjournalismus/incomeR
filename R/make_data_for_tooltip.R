make_data_for_tooltip = function(year,
                                 dir_yearly_data = "~/projects/dst/2023/setembro/2023-09-einkommens-karte/public/data/all/",
                                 bl_all_data =here("processed_lst_data/bundesländer_daten.csv")){

  bl_lookup = list(
    c("bl" = "Burgenland", "id" = 1),
    c("bl" = "Kärnten", "id" = 2),
    c("bl" = "Niederösterreich", "id" = 3),
    c("bl" = "Oberösterreich", "id" = 4),
    c("bl" = "Salzburg", "id" = 5),
    c("bl" = "Steiermark", "id" = 6),
    c("bl" = "Tirol", "id" = 7),
    c("bl" = "Vorarlberg", "id" = 8),
    c("bl" = "Wien", "id" = 9),
    c("bl" = "Österreich", "id" = 0)
  ) %>% bind_rows() %>%


  # yearly data -------------------------------------------------------------
  yearly_files  = dir(dir_yearly_data, ".*\\.csv", full.names = T)
  which_file = which(map_lgl(yearly_files, function(f){
    str_detect(basename(f), as.character(year))
  }))
  data_year = read_csv(yearly_files[which_file]) %>%
    mutate(
      bl_num = str_sub(as.character(gkz_current), 1,1)
    ) %>% left_join(bl_lookup, join_by(bl_num == id))


  # bl data that year -------------------------------------------------------
  bl_data = read_csv(bl_all_data)

  bl_year = bl_data %>%
    filter(
      year == {{year}}
    )

  oe_median = bl_year %>%
    filter(bl=="ÖSTERREICH") %>%
    filter(type=="Nettobezüge") %>%
    filter(variable=="Median") %>%
    pivot_wider(
      names_from = sex,
      values_from = val
    )

  data_tooltip = left_join(
    data_year,
    bl_year %>% select(-year),
    join_by(bl == bl,
            sex == sex,
            type == type,
            variable == variable),
    suffix = c("_gem", "_bl")
  ) %>%
    mutate(
      val_oe_zusammen = oe_median$Zusammen,
      val_oe_frauen = oe_median$Frauen,
      val_oe_maenner = oe_median$Männer,
    )

  op = makePath("~/projects/dst/2023/setembro/2023-09-einkommens-karte/public/data/tooltip/data_tooltip.csv")
  write_csv(data_tooltip, op)


}
