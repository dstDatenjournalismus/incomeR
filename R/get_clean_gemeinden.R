#' Create one File that tracks the changes of the gemeinden
#'
#' @importFrom janitor clean_names
#' @importFrom jsonlite fromJSON
#'
#' @param current_gemeinden
#' @param path_to_aenderungen
#'
#' @return
#' @export
#'
#' @examples
get_clean_gemeinden = function(current_gemeinden, year_gemeinde_data=2021, path_to_aenderungen = NULL) {

  # just for testing
  current_gemeinden = rajudas::oe_gem_data(year_gemeinde_data) %>%
    st_drop_geometry() %>%
    rename(
      gkz = id
    )

  if(is.null(path_to_aenderungen)){
    stop("Path to änderungs-file is missing!")
  }

  # each sheet
  namensaenderungen = read_xlsx(path_to_aenderungen, sheet = "Namensaenderung", skip = 1)
  teilungen = read_xlsx(path_to_aenderungen, sheet="Teilungen", skip = 1)
  zusammenlegungen = read_xlsx(path_to_aenderungen, sheet="Zusammenlegungen", skip = 1)
  gkz_aenderungen = read_xlsx(path_to_aenderungen, sheet="GKZ Änderungen", skip = 1)

  data_all = list(
    namensaenderungen = namensaenderungen,
    teilungen = teilungen,
    zusammenlegungen = zusammenlegungen,
    gkz_aenderungen = gkz_aenderungen
  ) %>% map(janitor::clean_names)



  # for each gemeinde in 2023 find the gkz(s) this gemeinde was in the past
  current_gemeinden_list = current_gemeinden %>% split(.$gkz)


  # indedx 1569 is gemeinde with gkz: 61442 - which had two zusammenlegungen
  # indedx 1633 has gkz änderung and zusammenlegung
  all = imap(current_gemeinden_list[1:10], function(gem, i){
    g = get_clean_gemeinde(gem, data_all = data_all, years_range = (year_gemeinde_data):2002)
    return(g)
  }) %>% map(bind_rows)
}
