#' Compute Monthly Summary of Responses
#'
#' This function calculates a monthly summary of responses based on given response
#' data and processed response data. It computes the total number of responses
#' initiated, unique alerts with at least one response, and the number of individuals reached.
#' @name compute_monthly_summary
#' @param response_data A dataframe containing response data.
#' @param processed_response_data A dataframe containing processed response data with
#' deduplicated alerts.
#' @param uuid The column name (as a string) representing unique identifiers for each response.
#' @param prev_period_date The start date of the previous period.
#' @param current_period_date The start date of the current period.
#' @param start_response_date The column name (as a string) representing the start date of a response.
#' @param ind_number The column name (as a string) representing the number of individuals
#' reached by a response.
#' @param response_number The column name (as a string) representing the number of responses.
#' @param desag_by (Optional) The column name (as a string) by which to disaggregate the
#' summary (e.g., region, sector). Default is `NA`.
#'
#' @return A dataframe summarizing the number of responses initiated (`response_started`),
#' the number of alerts that received at least one response (`alerts_with_at_least_oneresponse`),
#' and the total number of individuals reached (`people_reached`). If `desag_by` is
#' provided, results are grouped accordingly.
#'
#' @importFrom dplyr filter summarise group_by pull
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Sample response data
#' response_data <- data.frame(
#'   uuid = c("A1", "A1", "B2", "B2", "C1"),
#'   start_response_date = as.Date(c("2024-01-05", "2024-01-10", "2024-02-15",
#'   "2024-01-20", "2024-03-01")),
#'   ind_number = c(10, 20, 15, 30, 25),
#'   region = c("North", "North", "South", "South", "East"),
#'   donor=c("a1","a2","a1","a1","a3")
#'   )
#'
#' # Sample processed response data (deduplicated alerts)
#' processed_response_data <- data.frame(
#' uuid = c("A1", "B2", "C1"),
#' ind_number = c(100, 50, 75),
#' response_number = c(2, 2, 1),
#' region = c("North", "South", "East")
#' )
#'
#' # Compute summary with disaggregation by region
#' compute_monthly_summary(
#'   response_data = response_data,
#'   processed_response_data = processed_response_data,
#'   uuid = "uuid",
#'   prev_period_date = as.Date("2024-01-01"),
#'   current_period_date = as.Date("2024-02-01"),
#'   start_response_date = "start_response_date",
#'   ind_number = "ind_number",
#'   response_number = "response_number",
#'   desag_by = "region"
#' )
compute_monthly_summary <- function(response_data,
                                    processed_response_data,
                                    uuid,
                                    prev_period_date,
                                    current_period_date,
                                    start_response_date,
                                    ind_number,
                                    response_number,
                                    desag_by = NA) {

  # Get the list of unique UUIDs for responses starting in the period
  uuid_for_analysis <- response_data %>%
    filter(.data[[start_response_date]] >= prev_period_date &
             .data[[start_response_date]] < current_period_date) %>%
    pull(.data[[uuid]]) %>%
    unique()

  # If desag_by is provided, group by it
  if (!is.na(desag_by)) {
    monthly_summary <- processed_response_data %>%
      filter(.data[[uuid]] %in% uuid_for_analysis) %>%
      group_by(.data[[desag_by]]) %>%   # Group when desag_by is provided
      summarise(response_started = sum(.data[[response_number]], na.rm = TRUE),
                alerts_with_at_least_oneresponse = n(),
                people_reached = sum(.data[[ind_number]], na.rm = TRUE),
                .groups = "drop")
  } else {
    monthly_summary <- processed_response_data %>%
      filter(.data[[uuid]] %in% uuid_for_analysis) %>%
      summarise(response_started = sum(.data[[response_number]], na.rm = TRUE),
                alerts_with_at_least_oneresponse = n(),
                people_reached = sum(.data[[ind_number]], na.rm = TRUE),
                .groups = "drop")
  }

  return(monthly_summary)
}
