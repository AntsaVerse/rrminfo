#' Compute Monthly Summary of Sector Response Gaps
#'
#' This function calculates the monthly summary of response gaps by sector,
#' identifying the number of individuals in need who received or did not receive assistance.
#'
#' @name compute_monthly_summary_sector_gaps
#' @param alerts_responses_data A `data.frame` containing alert and response data.
#' @param prev_period_date A `Date` object indicating the start of the period to analyze.
#' @param current_period_date A `Date` object indicating the end of the period to analyze.
#' @param incident_date A `string` specifying the column name in `alerts_responses_data` that contains the incident (alert) dates.
#' @param alert_status_col A `string` specifying the column name indicating the alert status.
#' @param valid_status A `string` specifying the status that qualifies an alert as valid.
#' @param start_response_date A `string` specifying the column name containing the start date of the response.
#' @param ind_in_need A `string` specifying the column name indicating the number of individuals in need.
#' @param response_ind_number A `string` specifying the column name indicating the number of individuals assisted in the response.
#' @param response_mapping A `list` where each element is a named vector of length 2. The first value represents the column for individuals in need (`need_col`), and the second value represents the column for individuals who received assistance (`response_col`).
#'
#' @return A `data.frame` summarizing:
#' \describe{
#'   \item{sector_in_need}{Sector name}
#'   \item{response_need_covered}{Total individuals in need who were assisted}
#'   \item{response_need_notcovered}{Total individuals in need who were NOT assisted}
#'   \item{response_need_notcovered_percent}{Percentage of individuals in need who were NOT assisted}
#' }
#'
#' @examples
#' # Example dataset
#' alerts_responses_data <- data.frame(
#'   incident_date = as.Date(c("2024-03-01", "2024-03-10", "2024-02-25", "2024-03-15", "2024-03-20")),
#'   alert_status_col = c("Valid", "Valid", "Valid", "Valid", "Valid"),
#'   start_response_date = as.Date(c("2024-03-05", "2024-03-15", NA, "2024-03-25", NA)),
#'   ind_in_need = c(50, 100, 80, 60, 40),
#'   response_ind_number = c(40, 90, 0, 50, 0),
#'   food_need = c(1, 0, 1, 1, 0),
#'   food_response = c(1, 0, 0, 0, 0),
#'   nfi_need = c(1, 1, 0, 1, 1),
#'   nfi_response = c(1, 0, 0, 1, 0)
#' )
#'
#' # Define response mapping for sectors
#' response_mapping <- list(
#'   "Food" = c("food_need", "food_response"),
#'   "NFI" = c("nfi_need", "nfi_response")
#' )
#'
#' # Define time period
#' prev_period_date <- as.Date("2024-03-01")
#' current_period_date <- as.Date("2024-04-01")
#' valid_status <- "Valid"
#'
#' # Compute sector gaps
#' compute_monthly_summary_sector_gaps(
#'   alerts_responses_data,
#'   prev_period_date,
#'   current_period_date,
#'   "incident_date",
#'   "alert_status_col",
#'   valid_status,
#'   "start_response_date",
#'   "ind_in_need",
#'   "response_ind_number",
#'   response_mapping
#' )
#'
#' @export

utils::globalVariables(c(
"response_covered","response_notcovered","response_need_notcovered",
"response_need_covered"
))

compute_monthly_summary_sector_gaps<- function(alerts_responses_data,
                                               prev_period_date,
                                               current_period_date,
                                               incident_date,
                                               alert_status_col,
                                               valid_status,
                                               start_response_date,
                                               ind_in_need,
                                               response_ind_number,
                                               response_mapping){

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

  summary_monthly_sector_gaps <- data.frame()

  for (sector in names(response_mapping)) {
    need_col <- response_mapping[[sector]][1]
    response_col <- response_mapping[[sector]][2]

    sector_summary <- alerts_responses_data %>%
      mutate(response_covered=ifelse(.data[[need_col]]==1&.data[[response_col]]==1,1,0),
             response_notcovered=ifelse(.data[[need_col]]==1 & (is.na(.data[[response_col]]) | .data[[response_col]]!=1),1,0)
             ) %>%
      summarise(
        sector_in_need = sector,
        response_need_covered= sum(alert_timeline*response_timeline*response_covered*.data[[response_ind_number]], na.rm = TRUE),
        response_need_notcovered= sum(alert_timeline*response_timeline*response_notcovered*.data[[ind_in_need]], na.rm = TRUE),
        response_need_notcovered_percent=ifelse(
          response_need_notcovered + response_need_covered == 0,
          NA,
          round((response_need_notcovered / (response_need_notcovered + response_need_covered)) * 100)
        )
      )
    summary_monthly_sector_gaps <- bind_rows(summary_monthly_sector_gaps, sector_summary)
  }
  return(summary_monthly_sector_gaps)
}
