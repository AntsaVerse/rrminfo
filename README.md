
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rrminfo

<!-- badges: start -->
<!-- badges: end -->

This package includes the tools used by REACH Mali to produce analyses
within the framework of RRM and post-RRM information management.The
analytical models were designed by the Post-RRM team at IMPACT
Initiatives Mali. Sharing this package aims to ensure transparency in
calculation processes and facilitate their understanding by all users.

## Installation

You can install the development version of rrminfo from
[GitHub](https://github.com/AntsaVerse/rrminfo) with:

``` r
# install.packages("pak")
pak::pak("AntsaVerse/rrminfo")
```

## Load packages

``` r
library(rrminfo)
```

## Clean ActivityInfo databases

``` r
alert_data_clean <- clean_rrm_datasets(alert_data,
                                 hh_number = "menage_estimes",
                                 ind_number = "personne_estimees",
                                 hhsize = 6,
                                 start_date = "date_incident",
                                 end_date = "date_validation")
```
