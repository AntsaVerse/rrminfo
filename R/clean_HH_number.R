#' Clean Household and Individual Number Data
#'
#' This function ensures logical consistency between household (`hh_number`) and individual (`ind_number`) counts
#' based on an estimated **average household size (`hhsize`)**, which is a single integer value derived from
#' the most recent MSNA (Multi-Sector Needs Assessment).
#'
#' - If `hh_number` is missing but `ind_number` is available, it estimates `hh_number = round(ind_number / hhsize)`.
#' - If `ind_number` is missing but `hh_number` is available, it estimates `ind_number = hh_number * hhsize`.
#' - If `hh_number > ind_number`, it replaces `hh_number` with `hh_number = round(ind_number / hhsize)`.
#'
#' @param database A dataframe containing the household and individual number data.
#' @param hh_number A numeric column representing the household count.
#' @param ind_number A numeric column representing the number of individuals.
#' @param hhsize An **integer value** representing the estimated **average household size** (from MSNA).
#'
#' @return A dataframe with **imputed and corrected** `hh_number` and `ind_number` values.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' data <- data.frame(
#'   hh_number = c(10, NA, 15, 50, NA, 30),
#'   ind_number = c(50, 30, NA, 100, 60, 20)
#' )
#'
#' hhsize_value <- 5  # estimated average household size from MSNA
#' cleaned_data <- clean_HH_number(data, hh_number, ind_number, hhsize_value)
#' print(cleaned_data)
#'
clean_HH_number <- function(database, hh_number, ind_number, hhsize) {
  # Ensure hhsize is treated as a single integer value
  hh_number <- as.integer(round(hh_number))
  ind_number <- as.integer(round(ind_number))
  hhsize <- as.integer(round(hhsize))

  database <- database %>%
    dplyr::mutate(
      # If hh_number is missing but ind_number is available, estimate hh_number using hhsize
      hh_number = dplyr::if_else(
        is.na(hh_number) & !is.na(ind_number),
        round(ind_number / hhsize),
        hh_number
      ),

      # If ind_number is missing but hh_number is available, estimate ind_number using hhsize
      ind_number = dplyr::if_else(
        is.na(ind_number) & !is.na(hh_number),
        round(hh_number * hhsize),
        ind_number
      ),

      # If hh_number > ind_number, adjust hh_number to maintain logical consistency
      hh_number = dplyr::if_else(
        !is.na(hh_number) & !is.na(ind_number) & hh_number > ind_number,
        round(ind_number / hhsize),
        hh_number
      )
    )

  return(database)
}
