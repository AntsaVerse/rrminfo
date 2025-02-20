
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
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.4     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

## Data Cleaning and Processing for RRM Analysis

### Clean and Format the Alert Data

- Convert date columns to the proper format.
- Handle missing values in household and individual numbers.
- Adjust outliers in date variables.

``` r
alert_clean <- alert_data %>%
  clean_HH_number(hh_number = "menage_estimes", ind_number = "personne_estimees", hhsize = 6) %>%
  format_date(dates_vector = c("date_incident", "date_validation"), date_format = "%d/%m/%Y") %>%
  clean_dates(start_date = "date_incident", end_date = "date_validation")
```

### Process the Evaluation Data

- Convert date columns to the correct format.
- Discretize multiple-choice categorical variables (priority needs) into
  individual sector columns.

``` r
secteur_besoin <- c(
  food_need = "Vivre",
  wash_need = "WASH",
  nfi_need = "NFI",
  shelter_need = "Abris",
  flour_need = "Farine enrichie"
)

eval_clean <- eval_data %>%
  format_date(dates_vector = c("date_debut_evaluation", "date_fin_evaluation"), date_format = "%Y-%m-%d") %>%
  discretize_multiple_choice_variables(multi_choice_column = "Besoins.prioritaires", category_labels = secteur_besoin)
```

### Process RRM (Rapid Response Mechanism) Data

- Handle missing values in household and individual numbers.
- Format date columns.
- Discretize categorical variables for RRM sectors.

``` r
secteur_besoin <- c(
  food_rrm = "Vivre",
  wash_rrm = "WASH",
  nfi_rrm = "NFI",
  shelter_rrm = "Abris",
  flour_rrm = "Farine enrichie"
)

rrm_clean <- rrm_data %>%
  clean_HH_number(hh_number = "menage_rrm", ind_number = "personne_rrm", hhsize = 6) %>%
  format_date(dates_vector = c("debut_rrm", "fin_rrm"), date_format = "%Y-%m-%d") %>%
  clean_dates(start_date = "debut_rrm", end_date = "fin_rrm") %>%
  discretize_multiple_choice_variables(multi_choice_column = "secteur_rrm", category_labels = secteur_besoin) %>%
  rename(uuid = uuid)
```

### Process Post-RRM Data

``` r
secteur_besoin <- c(
  food_postrrm = "Vivre",
  wash_postrrm = "WASH",
  nfi_postrrm = "NFI",
  shelter_postrrm = "Abris",
  flour_postrrm = "Farine enrichie"
)

postrrm_clean <- postrrm_data %>%
  clean_HH_number(hh_number = "menage_postrrm", ind_number = "personne_postrrm", hhsize = 6) %>%
  format_date(dates_vector = c("debut_postrrm", "fin_postrrm"), date_format = "%Y-%m-%d") %>%
  clean_dates(start_date = "debut_postrrm", end_date = "fin_postrrm") %>%
  discretize_multiple_choice_variables(multi_choice_column = "secteur_postrrm", category_labels = secteur_besoin)
```

## Define the Time Period for Analysis

``` r
date_periode_prec <- as.Date("2024-12-01")
date_periode_act <- as.Date("2024-12-31")
date_12_mois_passee <- date_periode_act %m-% months(12)
```

## Reshape RRM and Post-RRM Data

- Transform response data into a structured format for analysis.
- Ensure unique identifiers and rename columns for consistency.

``` r
rrm_reshaped <- process_response_data(
  rrm_clean,
  uuid = "uuid",
  response_actor = "partner",
  food_response = "food_rrm",
  wash_response = "wash_rrm",
  nfi_response = "nfi_rrm",
  shelter_response = "shelter_rrm",
  health_response = "health_rrm",
  protection_response = "protection_rrm",
  mhm_response = "menstrual_rrm",
  enriched_flour_response = "flour_rrm",
  education_response = "education_support",
  livelihood_response = "livelihood_rrm",
  hh_number = "menage_rrm",
  ind_number = "personne_rrm",
  response_start_date = "debut_rrm",
  response_end_date = "fin_rrm",
  response_donor = "bailleur",
  admin1 = "region_rrm",
  admin2 = "cercle_rrm"
) %>%
  rename(rrm_response_number = response_number, bailleur_rrm = bailleur)

postrrm_reshaped <- process_response_data(
  postrrm_clean,
  uuid = "uuid",
  response_actor = "partner",
  food_response = "food_postrrm",
  wash_response = "wash_postrrm",
  nfi_response = "nfi_postrrm",
  shelter_response = "shelter_postrrm",
  health_response = "health_postrrm",
  protection_response = "protection_postrrm",
  mhm_response = "menstrual_postrrm",
  enriched_flour_response = "flour_postrrm",
  education_response = "education_postrrm",
  livelihood_response = "livelihood_postrrm",
  hh_number = "menage_postrrm",
  ind_number = "personne_postrrm",
  response_start_date = "debut_postrrm",
  response_end_date = "fin_postrrm",
  response_donor = "bailleur_postrrm",
  admin1 = "region_postrrm",
  admin2 = "cercle_postrrm"
) %>%
  rename(postrrm_response_number = response_number)
```

## Merge All Data for Analysis

Combine alerts, evaluations, RRM, and Post-RRM data into a single
analysis dataset.

``` r
df_analysis <- join_alerts_responses_data(
  uuid = "uuid",
  alert_data = alert_clean,
  eval_data = eval_clean,
  rrm_data = rrm_reshaped,
  rrm_response_number = "rrm_response_number",
  postrrm_data = postrrm_reshaped,
  postrrm_response_number = "postrrm_response_number",
  incident_date = "date_incident",
  validation_date = "date_validation",
  rrm_start_date = "debut_rrm",
  rrm_end_date = "fin_rrm",
  postrrm_start_date = "debut_postrrm",
  postrrm_end_date = "fin_postrrm"
)
```

## Merge All Data for Analysis

- Calculate the previous Post-RRM date.
- Add Post-RRM indicators for model analysis.

``` r
df_analysis <- add_prev_postrrm_date(
  df_analysis,
  rrm_started = "has_postrrm_response",
  rrm_start_date = "debut_rrm",
  alert_status = "statut_alerte",
  valid_status = "Validee",
  incident_date = "date_incident",
  time_alert_to_rrm = "time_alert_to_rrm"
)

model_one_indicators <- add_postrrm_model_one_indicators(
  df_analysis,
  prev_period_date = date_periode_prec,
  current_period_date = date_periode_act,
  prev_postrrm_date = "prev_postrrm_date",
  alert_status = "statut_alerte",
  valid_status = "Validee",
  validation_date_col = "date_validation",
  postrrm_start_date_col = "debut_postrrm",
  postrrm_start_threshold = as.Date("2023-01-01")
)
```

## Extract Specific Indicators

e.g: List of completed RRM responses that have not received a post-RRM
response as of the current date.

``` r
extract_uuid_by_indicator(model_one_indicators, "uuid", date_prev_response = "prev_postrrm_date", indicators = c("D"))
#> $D
#>    uuid prev_postrrm_date
#> 1    A2        2025-04-19
#> 2    A3        2025-04-13
#> 3    A4        2025-03-23
#> 4    A5        2025-03-28
#> 5    A6        2025-03-15
#> 6    A9        2025-03-07
#> 7   A11        2025-03-22
#> 8   A12        2025-03-27
#> 9   A14        2025-03-30
#> 10  A15        2025-03-25
#> 11  A16        2025-03-24
#> 12  A17        2025-03-26
#> 13  A21        2025-03-17
#> 14  A23        2025-03-16
```
