processBlData = function(path, sex, type, variable, output_dir){

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
  ) %>% bind_rows()



  # read sheets
  sheets = excel_sheets(path)
  years = str_match(sheets, "\\d{2}") %>% paste0("20", .)

  data_list = map(sheets, function(s){
    data = read_xlsx(path, s, skip = 1)
    year = str_match(s, "\\d{2}") %>% paste0("20", .)
    formatted = statistik_format_raw_lsv(data) %>%
      rename(bl = gkz) %>%
      mutate(bl = str_to_title(bl)) %>%
      mutate(year = year) %>%
      left_join(bl_lookup)
  })

  # make one per bl
  per_bl = bind_rows(data_list) %>%
    split(.$bl)

  per_bl_clean = per_bl[!names(per_bl) %in% c("Zusammen", "unbekannt", "Ausland")]


  # write out
  data_each_bl = map(per_bl_clean, function(bl){

    # only median and netto
    bl_clean = bl %>%
      filter(
        .data$sex %in% .env$sex &
          .data$type %in% .env$type &
          .data$variable %in% .env$variable
      )

    # get inflationd data
    max_year = bl_clean$year %>% max
    data_vpi = get_yearly_inflation(max_year) %>% mutate(year=as.character(year))

    bl_adjusted = bl_clean %>%
      left_join(data_vpi, join_by(year)) %>%
      mutate(val=as.numeric(val)) %>%
      mutate(val_real = if_else(variable=="Anzahl", val, as.numeric(val) / (index_2022 / 100)))

    bl_id = bl$id[[1]]
    bl_adjusted
  }) %>% bind_rows()

  return(data_each_bl)
}
