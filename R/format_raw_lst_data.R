#' Format raw lst data sent by the statistik austria
statistik_format_raw_lsv = function(data_year){

  names_l1 = names(data_year) %>%
    as.data.frame() %>%
    mutate(
      across(everything(), function(x){
        case_when(
          str_detect(x, "\\.\\.\\.") ~ NA,
          .default = x
        )
      })
    ) %>%
    rename(sex = 1) %>%
    tidyr::fill(sex) %>%
    pull(1)



  names_l2 = data_year[1,] %>%
    as.list() %>%
    unlist() %>%
    as.data.frame() %>%
    rename(n = 1) %>%
    tidyr::fill(n) %>%
    pull(1)

  names_l3 = data_year[2,] %>%
    as.list() %>%
    unlist() %>%
    as.data.frame() %>%
    rename(n = 1) %>%
    tidyr::fill(n) %>%
    pull(1)

  # actual data
  data = data_year %>%
    slice(3:nrow(.)) %>%
    rename(gkz = 1) %>%
    filter(if_all(everything(), ~ !is.na(.x)))



  dfs = vector("list", length=length(names_l1)-1)
  for (i in seq_along(names_l1)) {
    if(i == 1) next

    # sex
    sex = names_l1[[i]]
    type = names_l2[[i]]
    variable = names_l3[[i]]



    df = data %>%
      select(1, {
        {
          i
        }
      }) %>%
      rename(val = 2) %>%
      mutate(sex = sex,
             type = type,
             variable = variable)

    dfs[[i - 1]] = df
  }

  # bind them togehter
  df = bind_rows(dfs) %>%
    rename(gkz = 1)

  return(df)


}
