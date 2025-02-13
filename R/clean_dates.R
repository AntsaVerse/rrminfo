# Register global variables to avoid R CMD check notes
utils::globalVariables(c("days_gap", "Q1", "Q3", "IQR", "lower_bound", "upper_bound"))

#' Clean Date Variables by Handling Missing Values and Outliers
#'
#' This function calculates the gap in days between two date columns, detects outliers using the interquartile range (IQR) method,
#' replaces them with the median, and imputes missing dates accordingly.
#'
#' @param database A dataframe containing the date variables to clean.
#' @param start_date The column name representing the start date (quoted or unquoted).
#' @param end_date The column name representing the end date (quoted or unquoted).
#'
#' @return A dataframe with cleaned and imputed date values.
#' @importFrom stats quantile median
#' @importFrom dplyr mutate across all_of if_else select
#' @importFrom lubridate interval days
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' data <- data.frame(
#'   id = 1:5,
#'   start_date = as.Date(c("2024-01-01", "2024-02-01", NA, "2024-04-01", "2024-05-01")),
#'   end_date = as.Date(c("2024-01-10", "2024-02-15", "2024-03-20", NA, "2024-05-20"))
#' )
#'
#' clean_data <- clean_dates(data, start_date, end_date)
#' print(clean_data)

clean_dates <- function(database, start_date, end_date) {
  database <- database %>%
    dplyr::mutate(
      # Compute days gap between the two dates
      days_gap = lubridate::interval({{start_date}}, {{end_date}}) %/% lubridate::days(1),

      # Detect and Replace Outliers with Median using the interquartile range (IQR) method
      Q1 = stats::quantile(days_gap, 0.25, na.rm = TRUE),
      Q3 = stats::quantile(days_gap, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR,
      upper_bound = Q3 + 1.5 * IQR,
      days_gap = dplyr::if_else(days_gap < 0 |days_gap < lower_bound | days_gap > upper_bound,
                                as.integer(round(stats::median(days_gap, na.rm = TRUE))),
                                days_gap),

      # Impute missing dates
      end_date= dplyr::if_else(!is.na({{start_date}})&!is.na({{end_date}}),
                                     {{start_date}} + lubridate::days(days_gap),
                                     {{end_date}}),

      start_date= dplyr::if_else(!is.na({{end_date}}) & is.na({{start_date}}),
                                       {{end_date}} - lubridate::days(as.integer(round(stats::median(days_gap, na.rm = TRUE)))),
                                       {{start_date}})
    )%>% dplyr::select(-c(Q1,Q3,IQR,lower_bound,upper_bound,days_gap))

  return(database)
}


