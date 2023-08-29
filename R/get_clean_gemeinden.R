#' Title
#' @importFrom janitor clean_names
#' @importFrom jsonlite fromJSON
#'
#' @param path_to_aenderungen
#'
#' @return
#' @export
#'
#' @examples
get_clean_gemeinden = function(path_to_aenderungen = NULL) {

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


  # get gemeinde data for 2023
  gemeinden2023 = jsonlite::fromJSON("https://www.statistik.at/gs-atlas/ATLAS_IMAP_WFS/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=ATLAS_IMAP_WFS:DATA_GEM_GDB_DDT_DS_PG&outputFormat=application%2Fjson&viewparams=DDT:mv_t0278_them_bev_alter_2023;YEAR:2023-01-01;V1:bev_gesamt;V2:bev_gesamt;V1_ABS:bev_gesamt;GEN:100;V2FILTER:-1;WIENFLAG:23;") %>% .[["features"]] %>%
    unnest(properties) %>%
    select(gkz=ID, name=NAME)

  # for each gemeinde in 2023 find the gkz(s) this gemeinde was in the past
  gemeinden2023_list = gemeinden2023 %>% split(.$gkz)

  map(gemeinden2023_list, ~get_clean_gemeide(.x, data_all))

}
