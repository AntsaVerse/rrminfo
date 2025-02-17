#' Add Previsional Post-RRM Response Date
#'
#' This function calculates the expected start date of the Post-RRM response.
#' If an RRM response has started, the post-RRM date is set to 90 days after the RRM start.
#' Otherwise, if the alert is validated, the estimated incident date plus the median time
#' between alert and RRM response (from the entire dataset) plus 90 days is used.
#' @name add_prev_postrrm_date
#' @param df A dataframe containing alerts and response data.
#' @param rrm_started Column indicating whether the RRM response started (binary: 1 = Yes, 0 = No).
#' @param rrm_start_date Column representing the start date of the RRM response.
#' @param alert_status Column indicating the validation status of the alert.
#' @param valid_status The status value indicating a validated alert (e.g., "Validée").
#' @param incident_date Column representing the date of the incident.
#' @param time_alert_to_rrm Column representing the time between an alert and an RRM response.
#'
#' @return A dataframe with an additional column `prev_postrrm_date` containing the estimated Post-RRM response start date.
#' @importFrom dplyr mutate case_when
#' @importFrom stats median
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' df <- data.frame(
#'   uuid = c("A1", "B2", "C3"),
#'   rrm_started = c(1, 0, 0),
#'   rrm_start_date = as.Date(c("2024-01-10", NA, NA)),
#'   alert_status = c("Validée", "Validée", "Pending"),
#'   incident_date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
#'   time_alert_to_rrm = c(10, NA, NA)
#' )
#'
#' df <- add_prev_postrrm_date(df, "rrm_started", "rrm_start_date",
#'                             "alert_status", "Validée", "incident_date",
#'                             "time_alert_to_rrm")
#' print(df)

utils::globalVariables(c(
  "prev_postrrm_date", "time_alert_to_rrm", "time_alert_to_validation",
  "time_rrm_to_postrrm", "time_validation_to_rrm"
))

add_prev_postrrm_date <- function(df, rrm_started, rrm_start_date,
                                  alert_status, valid_status,
                                  incident_date, time_alert_to_rrm) {

  # Compute the median time between alert and RRM response
  median_time_alert_to_rrm <- as.integer(median(df[[time_alert_to_rrm]], na.rm = TRUE))

  df <- df %>%
    mutate(
      prev_postrrm_date = case_when(
        .data[[rrm_started]] == 1 ~ .data[[rrm_start_date]] + 90,
        .data[[alert_status]] == valid_status ~ .data[[incident_date]] + median_time_alert_to_rrm + 90,
        TRUE ~ as.Date(NA)
      ),
      prev_postrrm_date = as.Date(prev_postrrm_date)
    )

  return(df)
}
