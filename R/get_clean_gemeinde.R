#' Title
#'
#' @importFrom lubridate year
#' @importFrom dplyr tibble
#'
#' @param gkz
#' @param data_all
#'
#' @return
#' @export
#'
#' @examples
get_clean_gemeinde = function(gkz, data_all, years_range = 2004:2022){

  # theoretical data for all years the same
  theoretical = dplyr::tibble(
    year = years_range,
    gkz
  )

  # check which old gemeinden build this gemeinde in 2023
  zusammenlegungen = get_zusammenlegungen(gkz$gkz, data_all$zusammenlegungen)

  # check if the gkz was changed at some point
  gkz_aenderungen = get_gkz_aenderungen(gkz$gkz, data_all$gkz_aenderungen)

  # check if that gemeinde had any namensänderung
  old_names = get_names(gkz$gkz, data_all$namensaenderungen)



}

#' @title Find the Zusammenlegungen for that gemeinden
#'
#' @description
#' recursively try to find the gemeinden that now make up that single gemeinde
#'
get_zusammenlegungen = function(gkz, zusammenlegungen_all){

  newest_zusammenlegung = zusammenlegungen_all %>%
    filter(gemeindekennziffer_neu == gkz) %>% split(.$gemeindekennziffer_alt)

  if(length(newest_zusammenlegung) == 0) {
    return(NA)
  }


}

get_gkz_aenderungen = function(gkz, gkz_aenderungen_all){

}

get_names = function(gkz, namesaenderungen){
}
