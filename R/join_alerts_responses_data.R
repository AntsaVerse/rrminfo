#' Join Alerts and Responses Data
#'
#' This function merges alert, evaluation, and response datasets (RRM & Post-RRM) into a single dataset.
#' It also calculates key time differences between incident, validation, RRM response, and Post-RRM response.
#'
#' @name join_alerts_responses_data
#' @param uuid The unique identifier column name.
#' @param alert_data The alert dataset.
#' @param eval_data The evaluation dataset.
#' @param rrm_data The RRM dataset.
#' @param rrm_response_number The column representing the number of RRM responses.
#' @param postrrm_data The Post-RRM dataset.
#' @param postrrm_response_number The column representing the number of Post-RRM responses.
#' @param incident_date The column representing the incident date.
#' @param validation_date The column representing the validation date.
#' @param rrm_start_date The column representing the start date of the RRM response.
#' @param rrm_end_date The column representing the end date of the RRM response.
#' @param postrrm_start_date The column representing the start date of the Post-RRM response.
#' @param postrrm_end_date The column representing the end date of the Post-RRM response.
#'
#' @return A merged dataframe containing alert-response data and computed time differences.
#' @importFrom dplyr mutate left_join if_else
#' @export
#'
#' @examples
#' # Sample alert data
#' df_alert <- data.frame(
#'   uuid = c("A1", "B2", "C3"),
#'   incident_date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
#'   validation_date = as.Date(c("2024-01-05", "2024-02-05", "2024-03-05"))
#' )
#'
#' # Sample evaluation data
#' df_eval <- data.frame(
#'  uuid = c("A1", "B2", "C3"),
#'    food_need = c(1, 0, 1)
#'    )
#'
#' # Sample RRM response data
#' df_rrm <- data.frame(
#'   uuid = c("A1", "B2", "C3"),
#'   rrm_response_number = c(2, 0, 1),  # B2 has no RRM response
#'   rrm_start_date = as.Date(c("2024-01-10", NA, "2024-03-10")),
#'   rrm_end_date = as.Date(c("2024-01-20", NA, "2024-03-15"))
#' )
#'
#' # Sample Post-RRM response data
#' df_postrrm <- data.frame(
#'   uuid = c("A1", "B2", "C3"),
#'   postrrm_response_number = c(1, 1, 0),  # C3 has no Post-RRM response
#'   postrrm_start_date = as.Date(c("2024-01-25", "2024-02-15", NA)),
#'   postrrm_end_date = as.Date(c("2024-02-01", "2024-02-20", NA))
#' )
#'
#' # Run the function
#' df_combined <- join_alerts_responses_data(
#'   uuid = "uuid",
#'   alert_data = df_alert,
#'   eval_data = df_eval,
#'   rrm_data = df_rrm,
#'   rrm_response_number = "rrm_response_number",
#'   postrrm_data = df_postrrm,
#'   postrrm_response_number = "postrrm_response_number",
#'   incident_date = "incident_date",
#'   validation_date = "validation_date",
#'   rrm_start_date = "rrm_start_date",
#'   rrm_end_date = "rrm_end_date",
#'   postrrm_start_date = "postrrm_start_date",
#'   postrrm_end_date = "postrrm_end_date"
#' )
#' print(df_combined)

join_alerts_responses_data <- function(uuid, alert_data, eval_data, rrm_data,
                                       rrm_response_number, postrrm_data,
                                       postrrm_response_number,
                                       incident_date, validation_date,
                                       rrm_start_date, rrm_end_date,
                                       postrrm_start_date, postrrm_end_date) {

  # Merge datasets into one dataframe
  df <- alert_data %>%
    left_join(eval_data, by = uuid) %>%
    left_join(rrm_data, by = uuid) %>%
    left_join(postrrm_data, by = uuid)

  # Ensure response numbers are handled properly
  df <- df %>%
    mutate(
      has_postrrm_response = if_else(.data[[postrrm_response_number]] > 0 & !is.na(.data[[postrrm_response_number]]), 1, 0),
      has_rrm_response = if_else(.data[[rrm_response_number]] > 0 & !is.na(.data[[rrm_response_number]]), 1, 0)
    )

  # Compute time differences (Ensure NA handling)
  df <- df %>%
    mutate(
      time_alert_to_validation = as.numeric(.data[[validation_date]] - .data[[incident_date]], units = "days"),
      time_validation_to_rrm = as.numeric(.data[[rrm_start_date]] - .data[[validation_date]], units = "days"),
      time_rrm_duration = as.numeric(.data[[rrm_end_date]] - .data[[rrm_start_date]], units = "days"),
      time_rrm_to_postrrm = as.numeric(.data[[postrrm_start_date]] - .data[[rrm_end_date]], units = "days"),
      time_postrrm_duration = as.numeric(.data[[postrrm_end_date]] - .data[[postrrm_start_date]], units = "days"),
      time_alert_to_rrm = time_alert_to_validation + time_validation_to_rrm,
      time_alert_to_postrrm = time_alert_to_rrm + time_rrm_to_postrrm
    )

  return(df)
}

