#' Compute Monthly Spatial Summary of Response Gaps
#'
#' This function computes the spatial summary of response gaps at the national or administrative level.
#' It analyzes validated alerts and calculates the number of individuals assisted and not assisted
#' in a given time period.
#' @name compute_monthly_spatial_summary_gaps
#' @param alerts_responses_data A dataframe containing alert and response data.
#' @param prev_period_date The start date of the previous period (Date object).
#' @param current_period_date The start date of the current period (Date object).
#' @param incident_date The column name (string) representing the incident or alert occurrence date.
#' @param alert_status_col The column name (string) indicating the alert validation status.
#' @param valid_status The value (string) in `alert_status_col` that indicates a valid alert.
#' @param start_response_date The column name (string) representing the response start date.
#' @param alert_ind_number The column name (string) representing the number of individuals in the alert.
#' @param response_ind_number The column name (string) representing the number of individuals assisted.
#' @param admin1 The column name (string) representing the first level of administrative division (e.g., region).
#' @param admin2 The column name (string) representing the second level of administrative division (e.g., district).
#' @param admin_desag A string indicating the level of disaggregation:
#'        * `"national"` - Summary at the national level (aggregated by `admin1`).
#'        * `"admin1"` - Summary at the first administrative level (aggregated by `admin1` and `admin2`).
#'
#' @return A dataframe containing:
#'   - `admin1`: The first administrative division.
#'   - `admin2`: The second administrative division (if `admin_desag = "admin1"`).
#'   - `response_ind_assisted`: Total number of individuals assisted.
#'   - `response_ind_notassisted`: Total number of individuals not assisted.
#'   - `response_ind_notassisted_percent`: Percentage of individuals not assisted.
#'
#' @examples
#' library(dplyr)
#'
#' # Example dataset
#' alerts_responses_data <- data.frame(
#'   incident_date = as.Date(c("2024-03-01", "2024-03-10", "2024-02-25",
#'                             "2024-03-15", "2024-03-20")),
#'   alert_status_col = c("Valid", "Valid", "Valid", "Valid", "Valid"),
#'   start_response_date = as.Date(c("2024-03-05", "2024-03-15", NA,
#'                                   "2024-03-25", NA)), # Some missing responses
#'   alert_ind_number = c(50, 100, 80, 60, 40),  # People in the alert
#'   response_ind_number = c(40, 90, 0, 50, 0), # Some assisted, others not
#'   admin1 = c("Region A", "Region A", "Region B", "Region A", "Region B"),
#'   admin2 = c("District 1", "District 1", "District 2", "District 3", "District 2")
#' )
#'
#' prev_period_date <- as.Date("2024-03-01")
#' current_period_date <- as.Date("2024-04-01")
#' valid_status <- "Valid"
#'
#' # Run function at the national level
#' compute_monthly_spatial_summary_gaps(
#'   alerts_responses_data,
#'   prev_period_date,
#'   current_period_date,
#'   "incident_date",
#'   "alert_status_col",
#'   valid_status,
#'   "start_response_date",
#'   "alert_ind_number",
#'   "response_ind_number",
#'   "admin1",
#'   "admin2",
#'   admin_desag = "national"
#' )
#'
#' # Run function at the admin1 level
#' compute_monthly_spatial_summary_gaps(
#'   alerts_responses_data,
#'   prev_period_date,
#'   current_period_date,
#'   "incident_date",
#'   "alert_status_col",
#'   valid_status,
#'   "start_response_date",
#'   "alert_ind_number",
#'   "response_ind_number",
#'   "admin1",
#'   "admin2",
#'   admin_desag = "admin1"
#' )
#' @importFrom dplyr filter mutate summarise group_by ungroup
#' @export
compute_monthly_spatial_summary_gaps <- function(alerts_responses_data,
                                                 prev_period_date,
                                                 current_period_date,
                                                 incident_date,
                                                 alert_status_col,
                                                 valid_status,
                                                 start_response_date,
                                                 alert_ind_number,
                                                 response_ind_number,
                                                 admin1,
                                                 admin2,
                                                 admin_desag = "national") {
  # Compute alert timelines and response timelines
  alerts_responses_data <- alerts_responses_data %>%
    mutate(
      alert_timeline = ifelse(
        !is.na(.data[[incident_date]]) &
          .data[[incident_date]] >= prev_period_date &
          .data[[incident_date]] < current_period_date &
          .data[[alert_status_col]] == valid_status,
        1, 0
      ),
      response_timeline = ifelse(
        !is.na(.data[[start_response_date]]) &
          .data[[start_response_date]] >= prev_period_date &
          .data[[start_response_date]] < current_period_date,
        1, 0
      )
    )

  # Compute spatial summary based on the admin level
  if (admin_desag == "national") {
    summary_response_spatial_gaps <- alerts_responses_data %>%
      group_by(.data[[admin1]]) %>%
      summarise(
        response_ind_assisted = sum(alert_timeline * response_timeline * .data[[response_ind_number]], na.rm = TRUE),
        response_ind_notassisted = sum(alert_timeline * ifelse(response_timeline == 0, 1, 0) * .data[[alert_ind_number]], na.rm = TRUE),
        response_ind_notassisted_percent = round((response_ind_notassisted /
                                                    (response_ind_assisted + response_ind_notassisted)) * 100),
        .groups = "drop"
      )
  } else if (admin_desag == "admin1") {
    summary_response_spatial_gaps <- alerts_responses_data %>%
      group_by(.data[[admin1]], .data[[admin2]]) %>%
      summarise(
        response_ind_assisted = sum(alert_timeline * response_timeline * .data[[response_ind_number]], na.rm = TRUE),
        response_ind_notassisted = sum(alert_timeline * ifelse(response_timeline == 0, 1, 0) * .data[[alert_ind_number]], na.rm = TRUE),
        response_ind_notassisted_percent = round((response_ind_notassisted /
                                                    (response_ind_assisted + response_ind_notassisted)) * 100),
        .groups = "drop"
      )
  } else {
    stop("Error: Please choose either 'national' or 'admin1' for the admin_desag argument.")
  }

  return(summary_response_spatial_gaps)
}
