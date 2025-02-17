#' Compute Monthly Response by Sector
#'
#' This function aggregates response data by sector for a specified time period, computing
#' the total number of responses and the total number of individuals assisted for each sector.
#'
#' @name compute_monthly_response_sector
#' @param response_data A dataframe containing response records, including UUIDs and response start dates.
#' @param processed_response_data A dataframe containing processed response data, including sectoral responses.
#' @param prev_period_date The start date of the previous period (used for filtering responses).
#' @param current_period_date The start date of the current period (used for filtering responses).
#' @param start_response_date Column name (as a string) indicating the start date of the response.
#' @param ind_number Column name (as a string) indicating the number of individuals assisted.
#' @param response_mapping A named list mapping response sector names to corresponding column names in the dataset.
#'
#' @return A dataframe summarizing the monthly RRM responses by sector.
#' @importFrom dplyr summarise bind_rows filter pull
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Example response data
#' response_data <- data.frame(
#'   uuid = c("A1", "B2", "C3", "D4"),
#'   start_response_date = as.Date(c("2024-02-01", "2024-02-10", "2024-03-01", "2024-03-05"))
#' )
#'
#' # Example processed response data
#' processed_response_data <- data.frame(
#'   uuid = c("A1", "B2", "C3", "D4"),
#'   food_response = c(1, 0, 1, 1),
#'   wash_response = c(0, 1, 0, 1),
#'   nfi_response = c(1, 1, 1, 0),
#'   ind_number = c(100, 200, 150, 250)
#' )
#'
#' # Response mapping
#' response_mapping <- list(
#'   "Food" = "food_response",
#'   "WASH" = "wash_response",
#'   "NFI" = "nfi_response"
#' )
#'
#' # Compute monthly sector response summary
#' summary_df <- compute_monthly_response_sector(
#'   response_data = response_data,
#'   processed_response_data = processed_response_data,
#'   prev_period_date = as.Date("2024-02-01"),
#'   current_period_date = as.Date("2024-03-01"),
#'   start_response_date = "start_response_date",
#'   ind_number = "ind_number",
#'   response_mapping = response_mapping
#' )
#'
#' print(summary_df)

compute_monthly_response_sector <- function(response_data,
                                            processed_response_data,
                                            prev_period_date,
                                            current_period_date,
                                            start_response_date,
                                            ind_number,
                                            response_mapping) {

  # Get the list of unique UUIDs for responses starting in the period
  uuid_for_analysis <- response_data %>%
    filter(.data[[start_response_date]] >= prev_period_date &
             .data[[start_response_date]] < current_period_date) %>%
    pull(uuid) %>%
    unique()

  # Initialize an empty dataframe
  summary_monthly_response_sector <- data.frame()

  # Iterate through the response mapping
  for (sector in names(response_mapping)) {
    response_col <- response_mapping[[sector]]

    # Summarize the data for the current sector
    sector_summary <- processed_response_data %>%
      filter(uuid %in% uuid_for_analysis) %>%
      summarise(
        sector = sector,
        responses_count = sum(.data[[response_col]], na.rm = TRUE),
        individuals_assisted = sum(.data[[response_col]] * .data[[ind_number]], na.rm = TRUE)
      )

    # Append results
    summary_monthly_response_sector <- bind_rows(summary_monthly_response_sector, sector_summary)
  }

  return(summary_monthly_response_sector)
}
