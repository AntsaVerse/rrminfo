#' Add Post-RRM Model One Indicators
#'
#' This function adds indicators for tracking alerts that require post-RRM (Rapid Response Mechanism) positioning.
#' It categorizes alerts into different groups based on their validation status, response forecast dates,
#' and whether they have been registered for a post-RRM response.
#' @name add_postrrm_model_one_indicators
#' @param alert_response_database A dataframe containing alerts and response data.
#' @param prev_period_date The date marking the previous period boundary (time = 1).
#' @param current_period_date The date marking the current period boundary (time = 2).
#' @param prev_postrrm_date The forecast date for post-RRM response positioning.
#' @param alert_status_col The column name indicating alert status (e.g., "Validée").
#' @param valid_status The value in `alert_status_col` representing a validated alert.
#' @param validation_date_col The column name containing the alert validation date.
#' @param postrrm_start_date_col The column name containing the start date of post-RRM response.
#' @param postrrm_start_threshold The minimum date threshold for post-RRM response forecasting (default: "2023-01-01").
#'
#' @return A dataframe containing the original dataset with additional binary indicators.
#'
#' ## **Indicator Definitions**
#' **A**: Alerts validated before the previous period that still need post-RRM positioning.
#' - **A1**: Should have started before the previous period.
#' - **A2**: Should have started between the previous and current period.
#' - **A3**: Should start after the current period.
#' - **A4**: Total alerts needing post-RRM before the current period.
#' - **A5**: Total alerts needing post-RRM after the previous period.
#' - **A6**: Should start before the previous period or after the current period.
#'
#' **B**: Alerts validated between the previous and current period.
#' - **B1**: Should have started before the previous period.
#' - **B2**: Should have started between the previous and current period.
#' - **B3**: Should start after the current period.
#' - **B4**: Total alerts needing post-RRM before the current period.
#' - **B5**: Total alerts needing post-RRM after the previous period.
#' - **B6**: Should start before the previous period or after the current period.
#'
#' **C**: Alerts registered by post-RRM actors between the previous and current period.
#' - **C1-C6**: Same breakdown as A and B categories.
#'
#' **D**: Alerts requiring post-RRM positioning at the current period.
#' - **D1-D6**: Same breakdown as A and B categories.
#'
#' **Final Indicators**
#' - **forecast**: Future alerts requiring post-RRM but not yet registered.
#' - **arrears**: Alerts overdue for post-RRM but still not registered.
#'
#' @importFrom dplyr mutate case_when select filter
#' @export

utils::globalVariables(c(
  "A", "A1", "A2", "A3", "A4", "A5", "A6", "Aextra",
  "B", "B1", "B2", "B3", "B4", "B5", "B6","C","C1","C2",
  "C3", "C4","C5","C6",
  "D", "D1", "D2", "D3", "D4", "D5", "D6", "Dextra",
  "uuid", "validation"
))


add_postrrm_model_one_indicators <- function(
    alert_response_database,
    prev_period_date, current_period_date, prev_postrrm_date,
    alert_status_col, valid_status,
    validation_date_col, postrrm_start_date_col,
    postrrm_start_threshold = as.Date("2023-01-01")
) {

  alert_response_database <- alert_response_database %>%
    mutate(
      validation = ifelse(.data[[alert_status_col]] == valid_status, 1, 0),
      postrrm_registered = ifelse(!is.na(.data[[postrrm_start_date_col]]), 1, 0)
    ) %>%

    # Aextra: Alerts validated before previous period but not registered
    mutate(Aextra = case_when(
      validation == 1 &
        (postrrm_registered == 0 | .data[[postrrm_start_date_col]] > prev_period_date) &
        .data[[validation_date_col]] <= prev_period_date ~ 1,
      TRUE ~ 0
    )) %>%

    # A: Subset of Aextra with post-RRM forecast date after threshold
    mutate(A = case_when(
      Aextra == 1 & prev_postrrm_date > postrrm_start_threshold ~ 1,
      TRUE ~ 0
    )) %>%

    # A time categories
    mutate(
      A1 = ifelse(A == 1 & prev_postrrm_date <= prev_period_date, 1, 0),
      A2 = ifelse(A == 1 & prev_postrrm_date > prev_period_date & prev_postrrm_date <= current_period_date, 1, 0),
      A3 = ifelse(A == 1 & prev_postrrm_date > current_period_date, 1, 0),
      A4 = ifelse(A1 == 1 | A2 == 1, 1, 0),
      A5 = ifelse(A2 == 1 | A3 == 1, 1, 0),
      A6 = ifelse(A1 == 1 | A3 == 1, 1, 0)
    ) %>%

    # B: Alerts validated between previous and current period
    mutate(B = case_when(
      validation == 1 &
        .data[[validation_date_col]] <= current_period_date &
        .data[[validation_date_col]] > prev_period_date ~ 1,
      TRUE ~ 0
    )) %>%

    # B time categories
    mutate(
      B1 = ifelse(B == 1 & prev_postrrm_date <= prev_period_date, 1, 0),
      B2 = ifelse(B == 1 & prev_postrrm_date > prev_period_date & prev_postrrm_date <= current_period_date, 1, 0),
      B3 = ifelse(B == 1 & prev_postrrm_date > current_period_date, 1, 0),
      B4 = ifelse(B1 == 1 | B2 == 1, 1, 0),
      B5 = ifelse(B2 == 1 | B3 == 1, 1, 0),
      B6 = ifelse(B1 == 1 | B3 == 1, 1, 0)
    ) %>%
    # C: Alerts registered by post-RRM actors between the previous and current period
    mutate(C = case_when(
      validation == 1 &
        prev_postrrm_date > postrrm_start_threshold&
        .data[[validation_date_col]] <= current_period_date &
        .data[[postrrm_start_date_col]] > prev_period_date &
        .data[[postrrm_start_date_col]] <= current_period_date ~ 1,
      TRUE ~ 0
    ))%>%

    ## C time categories
    mutate(
      C1 = ifelse(C == 1 & prev_postrrm_date <= prev_period_date, 1, 0),
      C2 = ifelse(C == 1 & prev_postrrm_date > prev_period_date & prev_postrrm_date <= current_period_date, 1, 0),
      C3 = ifelse(C == 1 & prev_postrrm_date > current_period_date, 1, 0),
      C4 = ifelse(C1 == 1 | C2 == 1, 1, 0),
      C5 = ifelse(C2 == 1 | C3 == 1, 1, 0),
      C6 = ifelse(C1 == 1 | C3 == 1, 1, 0)
    )%>%

    # Dextra: Alerts needing post-RRM positioning
    mutate(Dextra = case_when(
      validation == 1 &
        .data[[validation_date_col]] < current_period_date &
        (postrrm_registered == 0 | .data[[postrrm_start_date_col]] > current_period_date) ~ 1,
      TRUE ~ 0
    )) %>%

    # D: Subset of Dextra with forecast date after threshold
    mutate(D = case_when(
      validation == 1 &
        .data[[validation_date_col]] < current_period_date &
        (postrrm_registered == 0 | .data[[postrrm_start_date_col]] > current_period_date) &
        prev_postrrm_date > postrrm_start_threshold ~ 1,
      TRUE ~ 0
    )) %>%

    # D time categories
    mutate(
      D1 = ifelse(D == 1 & prev_postrrm_date <= prev_period_date, 1, 0),
      D2 = ifelse(D == 1 & prev_postrrm_date > prev_period_date & prev_postrrm_date <= current_period_date, 1, 0),
      D3 = ifelse(D == 1 & prev_postrrm_date > current_period_date, 1, 0),
      D4 = ifelse(D1 == 1 | D2 == 1, 1, 0),
      D5 = ifelse(D2 == 1 | D3 == 1, 1, 0),
      D6 = ifelse(D1 == 1 | D3 == 1, 1, 0)
    ) %>%

    # Final indicators
    mutate(
      forecast = ifelse(D3 == 1 & C3 == 0, 1, 0),
      arrears = ifelse(A4 == 1 & C4 == 0, 1, 0)
    )
  return(alert_response_database)
}
