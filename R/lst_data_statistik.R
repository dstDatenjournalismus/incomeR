#' Process the statistik lst data
#'
#' @description
#' Loads, for each year, the Lohnsteuerstatistikdaten from the Statistik Austria
#' for a given sex, type of income (netto, brutto) and statistic value (e.g. median)
#'
#'
#' @param dir_with_excels directory with all the xlsx files
#' @param sex choose between (Frauen, Männer, Zusammen (default))
#' @param type choose between (Bruttobezüge, Nettobezüge (default), SV-Beiträge, einbehaltene Lohnsteuer)
#' @param variable Anzahl, Median (default), Mittelwert, P10, P20, P30, P40, P60, P70, P80, P90, Summe
#'
#' @import readxl
#' @export

read_lst_data = function(dir_with_excels = NULL,
                         sex = "Zusammen",
                         type = "Nettobezüge",
                         variable = "Median") {

  if(is.null(dir_with_excels)){
    stop("You must provide the path to the directory where all the .xlsx files are in")
  }

  # list all the excels -----------------------------------------------------
  excel_files = dir(dir_with_excels, ".*Gem.*\\.xls[x]?$", full.names = T)
  if (!length(excel_files) >= 18) {
    stop("There must be at least 18 files... Maybe the wrong folder?!")
  }


  # read the years from the files -------------------------------------------
  years = str_match(basename(excel_files), "\\d{2,4}") %>% .[, 1]

  data_all = purrr::map(seq_along(excel_files), function(i) {

    # the year of the data
    year = years[[i]]
    cli::cli_inform(glue("Reading data for year: {year}"))
    cat("\n")

    # read the data
    data_year = suppressMessages(readxl::read_excel(excel_files[[i]], skip = 1))

    # bring the data into long format
    clean_data = statistik_format_raw_lsv(data_year)

    # filter
    clean_data %>%
      dplyr::filter(
        .data$sex %in% .env$sex &
          .data$type %in% .env$type &
          .data$variable %in% .env$variable
      ) -> data_filtered

    data_filtered[["year"]] = year

    return(data_filtered)

  })

  return(data_all)

}
