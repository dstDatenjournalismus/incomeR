#' Create VPI timeseries
#'
#' @param index_year which year to set to compare to
#'
#' @return
#' @export
#'
#' @examples
get_yearly_inflation = function(index_year){

  # yearly inflation
  data_vpi = incomeR::data_vpi_raw

  data_vpi %>%
    filter(!is.na(jahres_durch_schnitt)) %>%
    select(jahr, vpi = jahres_durch_schnitt) %>%
    mutate(jahr = as.numeric(jahr)) %>%
    arrange(desc(jahr)) %>%
    filter(jahr <= index_year) %>% rename(year = jahr) %>% split(.$year) -> l

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

  data_vpi = bind_rows(yearly_vpi) %>% mutate("index_{index_year}" := cumulative_values)
  return(data_vpi)
}
