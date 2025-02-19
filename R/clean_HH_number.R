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
  # Ensure hhsize is a valid integer
  hhsize <- as.integer(round(hhsize))
  if (hhsize <= 0) stop("Household size (hhsize) must be greater than zero.")

  database <- database %>%
    mutate(
      !!hh_number := if_else(
        is.na(.data[[hh_number]]) & !is.na(.data[[ind_number]]),
        round(.data[[ind_number]] / hhsize),
        .data[[hh_number]]
      ),

      !!ind_number := if_else(
        is.na(.data[[ind_number]]) & !is.na(.data[[hh_number]]),
        round(.data[[hh_number]] * hhsize),
        .data[[ind_number]]
      ),

      !!hh_number := if_else(
        !is.na(.data[[hh_number]]) & !is.na(.data[[ind_number]]) & .data[[hh_number]] > .data[[ind_number]],
        round(.data[[ind_number]] / hhsize),
        .data[[hh_number]]
      )
    )

  return(database)
}

