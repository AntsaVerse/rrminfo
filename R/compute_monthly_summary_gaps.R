#' Compute Monthly Key Summary Gaps for Alerts and Responses
#'
#' This function summarizes key indicators for alerts and responses over a specified period.
#' It calculates the number of validated alerts, treated alerts, non-treated alerts,
#' and individuals assisted or not assisted during the response period.
#'
#' @name compute_monthly_key_summary_gaps
#' @param alerts_responses_data A dataframe containing alerts and response data.
#' @param prev_period_date The start date of the previous period (Date object).
#' @param current_period_date The start date of the current period (Date object).
#' @param incident_date The column name (string) that contains the date of the incident/movement.
#' @param alert_status_col The column name (string) indicating the status of the alert.
#' @param valid_status The value (string) that represents a valid alert status.
#' @param start_response_date The column name (string) that contains the start date of the response.
#' @param alert_ind_number The column name (string) containing the number of individuals affected by the alert.
#' @param response_ind_number The column name (string) containing the number of individuals assisted during the response.
#' @param desag_by (Optional) The column name (string) by which the data should be grouped (e.g., "region", "sector"). Default is `NA`, meaning no grouping.
#'
#' @return A dataframe summarizing the key response indicators for the specified period.
#'
#' @examples
#' # Example data
#' df_alerts <- data.frame(
#'   uuid = c("A1", "B2", "C3", "D4", "E5"),
#'   incident_date = as.Date(c("2024-01-05", "2024-01-10", "2023-12-25", "2023-12-30", "2024-01-15")),
#'   alert_status = c("Validated", "Validated", "Validated", "Validated", "Validated"),
#'   start_response_date = as.Date(c("2024-01-12", "2024-01-20", "2024-01-05", NA, "2024-01-22")),
#'   alerted_individuals = c(100, 200, 150, 80, 90),
#'   responded_individuals = c(80, 180, 120, 0, 85),
#'   region = c("Region A", "Region A", "Region B", "Region A", "Region C")
#' )
#'
#' # Without disaggregation
#' compute_monthly_key_summary_gaps(
#'   alerts_responses_data = df_alerts,
#'   prev_period_date = as.Date("2023-12-01"),
#'   current_period_date = as.Date("2024-01-01"),
#'   incident_date = "incident_date",
#'   alert_status_col = "alert_status",
#'   valid_status = "Validated",
#'   start_response_date = "start_response_date",
#'   alert_ind_number = "alerted_individuals",
#'   response_ind_number = "responded_individuals"
#' )
#'
#' # With disaggregation by region
#' compute_monthly_key_summary_gaps(
#'   alerts_responses_data = df_alerts,
#'   prev_period_date = as.Date("2023-12-01"),
#'   current_period_date = as.Date("2024-01-01"),
#'   incident_date = "incident_date",
#'   alert_status_col = "alert_status",
#'   valid_status = "Validated",
#'   start_response_date = "start_response_date",
#'   alert_ind_number = "alerted_individuals",
#'   response_ind_number = "responded_individuals",
#'   desag_by = "region"
#' )
#'
#' @importFrom dplyr mutate summarise group_by filter if_else
#' @export

utils::globalVariables(c("alert_timeline", "response_timeline", "alert_before",
                         "response_ind_assisted", "response_ind_notassisted"))

compute_monthly_key_summary_gaps <- function(alerts_responses_data,
                                             prev_period_date,
                                             current_period_date,
                                             incident_date,
                                             alert_status_col,
                                             valid_status,
                                             start_response_date,
                                             alert_ind_number,
                                             response_ind_number,
                                             desag_by = NA) {

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
      alert_before = ifelse(
        !is.na(.data[[incident_date]]) &
          .data[[incident_date]] < prev_period_date &
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

  # If desag_by is provided, group by it
  if (!is.na(desag_by)) {
    summary_response_gaps <- alerts_responses_data %>%
      group_by(.data[[desag_by]]) %>%
      summarise(
        validated_alerts = sum(alert_timeline, na.rm = TRUE),
        treated_alerts = sum(alert_timeline * response_timeline, na.rm = TRUE),
        response_ind_assisted = sum(alert_timeline * response_timeline * .data[[response_ind_number]], na.rm = TRUE),
        non_treated_alerts = sum(alert_timeline * ifelse(response_timeline == 0, 1, 0), na.rm = TRUE),
        response_ind_notassisted = sum(alert_timeline * ifelse(response_timeline == 0, 1, 0) * .data[[alert_ind_number]], na.rm = TRUE),
        treated_alerts_before_prev_date = sum(alert_before * response_timeline, na.rm = TRUE),
        response_ind_assisted_before_prev_date = sum(alert_before * response_timeline * .data[[response_ind_number]], na.rm = TRUE),
        displaced_ind_number = response_ind_assisted + response_ind_notassisted,
        .groups = "drop"
      )
  } else {
    summary_response_gaps <- alerts_responses_data %>%
      summarise(
        validated_alerts = sum(alert_timeline, na.rm = TRUE),
        treated_alerts = sum(alert_timeline * response_timeline, na.rm = TRUE),
        response_ind_assisted = sum(alert_timeline * response_timeline * .data[[response_ind_number]], na.rm = TRUE),
        non_treated_alerts = sum(alert_timeline * ifelse(response_timeline == 0, 1, 0), na.rm = TRUE),
        response_ind_notassisted = sum(alert_timeline * ifelse(response_timeline == 0, 1, 0) * .data[[alert_ind_number]], na.rm = TRUE),
        treated_alerts_before_prev_date = sum(alert_before * response_timeline, na.rm = TRUE),
        response_ind_assisted_before_prev_date = sum(alert_before * response_timeline * .data[[response_ind_number]], na.rm = TRUE),
        displaced_ind_number = response_ind_assisted + response_ind_notassisted
      )
  }

  return(summary_response_gaps)
}
