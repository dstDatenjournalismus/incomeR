#' Create VPI timeseries
#'
#' @param upper_index_year which year to set to compare to
#'
#' @return
#' @export
#'
#' @examples
get_yearly_inflation = function(upper_index_year = 2022, lower_index_year = 2004){

  # yearly inflation
  data_vpi = incomeR::data_vpi_raw

  data_vpi %>%
    filter(!is.na(jahres_durch_schnitt)) %>%
    select(jahr, vpi = jahres_durch_schnitt) %>%
    mutate(jahr = as.numeric(jahr)) %>%
    arrange(desc(jahr)) %>%
    filter(jahr <= upper_index_year & jahr >= lower_index_year) %>% rename(year = jahr) %>% split(.$year) -> l

  yearly_vpi = l[sort(names(l), decreasing = T)]

  cumulative_values = numeric(length = length(yearly_vpi))
  for(i in seq_along(yearly_vpi)){
    if(i == 1){
      cumulative_values[[i]] = 100
    }else{
      change = 1 + yearly_vpi[[i-1]]$vpi/100
      cumulative_values[[i]] = cumulative_values[[i-1]] / change
    }
  }

  data_vpi = bind_rows(yearly_vpi) %>% mutate("index_{upper_index_year}" := cumulative_values)

  # now do it backwards
  data_vpi_ascending = data_vpi %>% arrange(year) %>% split(.$year)
  cumulative_values_index_lower = numeric(length = length(data_vpi_ascending))
  for(i in seq_along(data_vpi_ascending)){
    if(i == 1){
      cumulative_values_index_lower[[i]] = 100
    }else{
      change = data_vpi_ascending[[i]]$vpi/100
      cumulative_values_index_lower[[i]] = cumulative_values_index_lower[[i-1]] + (change * cumulative_values_index_lower[[i-1]])
    }
  }

  data_vpi = data_vpi %>%
    mutate(
      "index_{lower_index_year}" := cumulative_values_index_lower %>% rev
    )

  return(data_vpi)
}
