#' Create one File that tracks the changes of the gemeinden
#'
#' @importFrom janitor clean_names
#' @importFrom jsonlite fromJSON
#'
#' @param path_to_aenderungen
#'
#' @return
#' @export
#'
#' @examples
get_clean_gemeinden_raw = function(year_gemeinde_data = 2021,
                               path_to_aenderungen = NULL,
                               out_dir = NULL,
                               TEST = FALSE) {


  # if data is to be saved!
  if(!is.null(out_dir)){

    output_basename = glue("historic_gemeinden_upto_{year_gemeinde_data}.Rdata")
    output_filename = here(out_dir, output_basename)


    if(file.exists(output_filename)){
      cli::cli_h2(glue("File for {year_gemeinde_data} already exists on disk -> Loading it!"))
      preprocessed_data = readRDS(output_filename)
      return(preprocessed_data)
    }
  }

  # get the gemeindedata for that recent specified year
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
  # Gnas (with gkz: 62380) is kind of strange
  # i = which(names(current_gemeinden_list) == 62380)
  # Murfeld has index 1514
  if(TEST){
    current_gemeinden_list = current_gemeinden_list[1300:1750]
  }

  all = map(seq_along(current_gemeinden_list), function(i) {

    cat(i, "/", length(current_gemeinden_list), "\r")

    gem = current_gemeinden_list[[i]]

    g = get_clean_gemeinde(gem,
                           years_range = (year_gemeinde_data):2002,
                           data_all = data_all)
    return(g)
  })

  all_joined = map(all, ~ bind_rows(.x, .id = "year")) %>% setNames(current_gemeinden$gkz)

  if(!is.null(out_dir)){
    saveRDS(all_joined, output_filename)
  }


  return(all_joined)

}

replace_2012_2013 = function(clean_gemeinden_raw){

  map(seq_along(clean_gemeinden_raw), function(i){
    gem = clean_gemeinden_raw[[i]]

    #iterate over the rows
    final_gkz = map(seq_along(1:nrow(gem)), function(r){

      row = gem[r, ]

      # return if not 2012
      if(as.numeric(row$year) != 2012){
        return(row)
      }

      # if its 2012 and the gemeinde exists with the same name in 2013, give it the 2013 gkz
      gems_2013 = gem %>% filter(year =="2013") %>% pull(name)
      in_2013 = row$name %in% gems_2013

      ## Kirchbach ARTIKEL!!
      ## in dem sheet gkz änderungen ist der Name falsch...
      if(row$name == "Kirchbach in Steiermark"){
        row$name = "Kirchbach in der Steiermark"
      }

      # was passiert hier??
      if(any(str_detect(gems_2013, row$name))){
        row_2013 = gem %>% filter(year=="2013") %>% filter(str_detect(name, row$name))
        if(nrow(row_2013) > 1){
          row_2013 = row_2013 %>%
            mutate(str_dist = stringdist::stringdist(name, row$name)) %>%
            filter(str_dist == min(str_dist)) %>% select(-str_dist)
        }
        row$gkz = row_2013$gkz
      }

      # manual cases
      # Warum mache ich das?!
      # Trofaiach
      if (row$gkz %in% c("61102",
                         "61103",
                         "61117")) {
        row$gkz = "61120"
      }

      # Buch-St. Magdalena
      if (row$gkz %in% c("60702",
                         "60734")) {
        row$gkz = "62205"
      }



      return(row)
    }) %>% bind_rows()
  }) %>% setNames(names(clean_gemeinden_raw))
}

get_clean_gemeinden = function(year_gemeinde_data = 2021,
                               path_to_aenderungen = NULL,
                               out_dir = NULL,
                               TEST = FALSE) {

  # raw data as it is in the files
  clean_gemeinden_raw = get_clean_gemeinden_raw(year_gemeinde_data, path_to_aenderungen, out_dir, TEST)

  # replace 2012 gkz with 2013 gkz
  final_replaced = replace_2012_2013(clean_gemeinden_raw)
}
