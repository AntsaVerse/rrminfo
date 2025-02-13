#' Clean RRM Datasets
#'
#' This function applies multiple cleaning steps to an RRM dataset:
#' - Converts `start_date` and `end_date` to Date format.
#' - Ensures household (`hh_number`) and individual (`ind_number`) counts are logically consistent.
#' - Handles missing dates and removes outliers based on the IQR method.
#'
#' @param database A dataframe containing the dataset to be cleaned.
#' @param hh_number A numeric column representing the household count.
#' @param ind_number A numeric column representing the number of individuals.
#' @param hhsize An integer value representing the estimated average household size from MSNA.
#' @param start_date A column representing the start date (eg:incident date; response start date).
#' @param end_date A column representing the end date (eg: validation date; response end date).
#'
#' @return A cleaned dataframe with corrected date formats, household and individual counts, and imputed dates.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' data <- data.frame(
#'   hh_number = c(10, NA, 15, 50, NA, 30),
#'   ind_number = c(50, 30, NA, 100, 60, 20),
#'   start_date = c("2024-01-01", NA, "2024-03-01", "2024-04-01", NA,NA),
#'   end_date = c("2024-01-10", "2024-02-15", NA, "2024-05-01", "2024-06-15",NA)
#' )
#'
#' cleaned_data <- clean_rrm_datasets(data,
#'                                    hh_number = "hh_number",
#'                                    ind_number = "ind_number",
#'                                    hhsize = 5,
#'                                    start_date = "start_date",
#'                                    end_date = "end_date")
#'
#' print(cleaned_data)
#'
clean_rrm_datasets <- function(database, hh_number, ind_number, hhsize, start_date, end_date) {

  # Step 1: Automatically create the dates vector
  dates_vector <- c(as.character(start_date), as.character(end_date))

  # Step 2: Format start_date and end_date columns
  database <- format_date(database, dates_vector)

  # Step 3: Clean Household and Individual Numbers
  database <- clean_HH_number(database, dplyr::pull(database, {{hh_number}}), dplyr::pull(database, {{ind_number}}), hhsize)

  # Step 4: Clean Dates and Handle Outliers
  database <- clean_dates(database, dplyr::pull(database, {{start_date}}), dplyr::pull(database,{{end_date}}))

  return(database)
}

