#' Extract UUID and date_prev_response for each indicator
#'
#' This function extracts UUIDs and their corresponding `date_prev_response`
#' for each indicator where the value is `1`.
#'
#' @name extract_uuid_by_indicator
#' @param df A dataframe containing the indicators and date_prev_response.
#' @param uuid_col The name of the UUID column.
#' @param date_prev_response The name of the date column (date_prev_response).
#' @param indicators A vector of column names representing the indicators.
#'
#' @return A named list where each element is a dataframe with UUIDs and their date_prev_response
#'         for a specific indicator.
#' @importFrom dplyr filter select
#' @export
#'
#' @examples
#' df_example <- data.frame(
#'   uuid = c("U1", "U2", "U3", "U4"),
#'   date_prev_response = as.Date(c("2024-01-15", "2024-02-20", "2024-03-10", "2024-04-05")),
#'   A = c(1, 0, 1, 0),
#'   B = c(0, 1, 0, 1),
#'   C1 = c(1, 1, 0, 0)
#' )
#'
#' extract_uuid_by_indicator(df_example, "uuid", "date_prev_response", c("A", "B", "C1"))

extract_uuid_by_indicator <- function(df, uuid_col, date_prev_response, indicators) {
  result_list <- list()

  for (ind in indicators) {
    if (ind %in% names(df)) {
      subset_df <- df %>%
        filter(.data[[ind]] == 1) %>%
        select(all_of(uuid_col), all_of(date_prev_response))

      result_list[[ind]] <- subset_df
    }
  }

  return(result_list)
}
