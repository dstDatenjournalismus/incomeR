# Installation

-   Man kann das Paket über `remotes::install_github("https://github.com/dstDatenjournalistmus/incomeR")` installieren

# Verwendung

## Daten abfragen

-   Mit der Funktion können die Lohnsteuerdaten abgefragt werden. Dabei kann man das Geschlecht, die Art des Einkommens und die Variable wählen

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

> Gibt eine Liste zurück in der jedes Element die Daten für ein Jahr sind mit denen dann weitergearbeitet werden kann! E.g. 2004

``` r
   gkz       val      sex      type        variable   year 
   <chr>     <chr>    <chr>    <chr>       <chr>      <chr>
 1 unbekannt 19430.47 Zusammen Nettobezüge Mittelwert 04   
 2 Ausland   20239.52 Zusammen Nettobezüge Mittelwert 04   
 3 10101     26101.62 Zusammen Nettobezüge Mittelwert 04   
 4 10201     21108.21 Zusammen Nettobezüge Mittelwert 04   
 5 10301     26468    Zusammen Nettobezüge Mittelwert 04   
 6 10302     24185.42 Zusammen Nettobezüge Mittelwert 04   
 7 10303     24456.9  Zusammen Nettobezüge Mittelwert 04   
```
