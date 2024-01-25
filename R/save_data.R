makeYearData = function(output_dir = "~/projects/dst/2023/setembro/2023-09-einkommens-karte/public/data/",
                        data = NULL,
                        sum_fun = mean,
                        variable) {


  data_summarised = data %>% map(function(g) {
    g %>%
      group_by(year) %>%
      summarise(
        name = first(name),
        {{variable}} := sum_fun(.data[[variable]]),
        # val = sum_fun(!!dplyr::sym(variable)),
        gkz_current = first(gkz_current)
      )
  })

  # get the data per year
  per_year = bind_rows(data_summarised) %>%
    split(.$year)

  # save each year
  walk(seq_along(per_year), function(i) {
    cat(paste0(i, "/", length(per_year), "\r"))
    y = d$year[[1]]
    d = per_year[[i]]
    path = here(base_dir, "all", glue("{y}.csv"))
    suppressMessages(write_csv(d, path))
  })
}

makeGemeindeData = function(base_dir = "~/projects/dst/2023/setembro/2023-09-einkommens-karte/public/data/",
                            data = NULL,
                            one_val_per_year = T) {


  walk(seq_along(data), function(i) {
    cat(paste0(i, "/", length(data), "\r"))
    current_gkz = names(data)[[i]]
    d = data[[i]]

    if(one_val_per_year) {
      d = d %>%
        group_by(year, gkz=gkz_current, sex, type, variable) %>%
        summarise(val=mean(val, na.rm=T), name=first(name))
    }


    path = makePath(
      glue(
        "~/projects/dst/2023/setembro/2023-09-einkommens-karte/public/data/gemeinden/{current_gkz}.csv"
      )
    )
    write_csv(d, path)
  })
}
