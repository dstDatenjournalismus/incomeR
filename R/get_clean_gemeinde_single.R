#' Get the history of a current gemeinde
#'
#' @param gem
#' @param years_range
#' @param data_all
#'
#' @return
#' @export
#'
#' @examples

get_clean_gemeinde = function(gem, years_range, data_all){

  # the current state
  current_gemeinden_this_gemeinde = gem
  years_list = list(
    current_gemeinden_this_gemeinde
  ) %>% setNames(years_range[[1]])

  current_gkzs = gem$gkz
  for(y in years_range){


    # check if any of the gkzs of the gemeinde changed in that year
    new_gkzs = map(current_gkzs, function(gkz) {


      old_gkzs = data_all$gkz_aenderungen %>%
        mutate(year = (lubridate::year(in_kraft_seit)) - 1) %>% # minus 1 -> Wie war der stand ein Jahr vorher?
        filter(year == y) %>%
        filter(gemeindekennziffer_neu == gkz)

      if(nrow(old_gkzs) == 0){
        return(NA)
      }else{
        return(old_gkzs)
      }
    })

    # no current gkz was changed
    no_new_gkzs = all(is.na(new_gkzs))
    gkzs_to_check_for_zusammenlegung = current_gkzs

    # there are new gkzs for any of the current ones!
    if(!no_new_gkzs){

      # which of the current one has new ones
      which_is_new = !is.na(new_gkzs)

      # update the current gemeinden
      new_gemeinden = bind_rows(new_gkzs[which_is_new]) %>%
        select(gkz = gemeindekennziffer_alt, gkz_neu=gemeindekennziffer_neu, name=gemeindename)

      # what does this do?
      updated_current_gemeinden = map(1:nrow(current_gemeinden_this_gemeinde), function(r){
        row_old = current_gemeinden_this_gemeinde[r, ] %>% mutate(
          name = as.character(name),
          gkz = as.character(gkz)
        )

        if (row_old$gkz %in% new_gemeinden$gkz_neu) {
          row = new_gemeinden[new_gemeinden$gkz_neu == row_old$gkz,] %>%
            mutate(name = as.character(name),
                   gkz = as.character(gkz)) %>%
            select(-gkz_neu)
          return(row)
        }

        return(row_old)
      }) %>% bind_rows()

      current_gemeinden_this_gemeinde = updated_current_gemeinden


      # update the gkzs to check for a potential union
      gkzs_to_check_for_zusammenlegung = current_gemeinden_this_gemeinde$gkz
    }


    # now check if any of these gkzs was part of a union
    unioned_gemeinden = map(gkzs_to_check_for_zusammenlegung, function(gkz){

      union_gemeinden = data_all$zusammenlegungen %>%
        mutate(
          year = (lubridate::year(in_kraft_seit) - 1)
        ) %>%
        filter(year == y) %>%
        filter(
         gemeindekennziffer_neu == gkz
        )

      if(nrow(union_gemeinden) == 0){
        return(NA)
      }else{
        return(union_gemeinden)
      }
    })

    # if there was a union
    if(!all(is.na(unioned_gemeinden))) {
      unioned_gemeinden = unioned_gemeinden[!is.na(unioned_gemeinden)]
      unioned_gemeinden = bind_rows(unioned_gemeinden) %>%
        select(gkz = gemeindekennziffer_alt, name=gemeindename_alt)
    }else{
      unioned_gemeinden = current_gemeinden_this_gemeinde
    }

    ## check for namesänderungen
    unioned_gemeinden = map(1:nrow(unioned_gemeinden), function(r) {
      row = unioned_gemeinden[r,] %>% mutate(across(everything(), as.character))
      gkz = row$gkz

      namensaenderung = data_all$namensaenderungen %>%
        mutate(year = (lubridate::year(in_kraft_seit)) - 1) %>% # minus 1
        filter(year == y) %>%
        filter(gemeindekennziffer == gkz) %>%
        select(gkz  = gemeindekennziffer,
               name = gemeindename_alt) %>%
        mutate(across(everything(), as.character))

      if (nrow(namensaenderung) > 0) {
        return(namensaenderung)
      } else{
        return(row)
      }
    }) %>% bind_rows()

    # the current gemeinden that are now building the original one
    current_gemeinden_this_gemeinde = unioned_gemeinden
    current_gkzs = current_gemeinden_this_gemeinde$gkz

    # update the gemeinde list
    years_list[[glue("{y}")]] = current_gemeinden_this_gemeinde
  }


  return(years_list)
}


