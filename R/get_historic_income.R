#' Get the income for Gemeinden with Änderungen
#'
#' @param raw_data Raw income data for **one variable** (e.g. the Median)
#' @param gems_changes a list of changes for each gemeinde
#'
#' @return
#' @export
#'
#' @examples
get_historic_income = function(raw_data, historic_gemeindestaende){

  # how many years are in the income data? We only need the historic gemeindestatus
  # for these years
  years_with_lst_data = raw_data %>% pull(year) %>% unique
  if (!str_detect(years_with_lst_data[[1]], "\\d{4}")) {
    years_with_lst_data = as.character(glue("20{years_with_lst_data}"))
  }

  # pre format raw data
  year_ = raw_data$year[[1]]
  if(!str_detect(year_, "\\d{4}")){
    raw_data = raw_data %>%
      mutate(
        year = as.character(glue("20{year}"))
      )
  }

  # for each historic gemeindestand at the income
  income_all_gemeinden = map(seq_along(historic_gemeindestaende), function(i){

    # the current gemeinde
    current_gem = names(historic_gemeindestaende)[[i]]

    # the gemeinden it was historically made from
    historic_gemeinden = historic_gemeindestaende[[i]] %>%
      filter(year >= min(years_with_lst_data),
             year <= max(years_with_lst_data),)

    # for every year in the income data, get the income for the gemeinden
    income = left_join(historic_gemeinden,
                       raw_data,
                       join_by(year == year,
                               gkz == gkz)) %>%
      mutate(.before = gkz, gkz_current = current_gem) %>%
      mutate(
        year = as.numeric(year),
        val = as.numeric(val)
      )

    return(income)

  })

  # set Names
  gkzs_current = map_chr(income_all_gemeinden, function(inc){
    return(inc$gkz_current[[1]])
  })
  names(income_all_gemeinden) = gkzs_current

  return(income_all_gemeinden)
}
