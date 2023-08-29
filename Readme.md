# Installation

-   Man kann das Paket über `remotes::install_github("https://github.com/dstDatenjournalistmus/incomeR")` installieren

# Verwendung

## Daten abfragen

-   Mit der Funktion können die Lohnsteuerdaten abgefragt werden. Dabei kann man
das Geschlecht, die Art des Einkommens und die Variable wählen

``` r

# Hilfe für die Funktion abfragen
?incomeR::read_lst_data

# Media-Nettobezüge für beide Geschlechter für alle Jahre
raw_data = incomeR::read_lst_data(
  dir_with_excels = here("~/projects/dst/DATEN/statistik_austria/lst_daten/"),
  sex = "Zusammen",
  type = "Nettobezüge",
  variable = c("Median", "Mittelwert")
)
```
