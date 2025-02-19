#' Format Date Columns
#'
#' This function converts specified columns in a dataframe to Date format.
#'
#' @param database A dataframe containing date columns.
#' @param dates_vector A character vector of column names to convert to Date format.
#' @param date_format A character string specifying the format of the input dates (default: "%Y-%m-%d")
#' @return A dataframe with formatted date columns.
#' @importFrom dplyr mutate across all_of
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df<-data.frame(
#'date1=c("2025-01-28", "2025-01-27", "2025-01-26"),
#'organisation_name = c("org_1", "org_2", "org_3"),
#'date2=c("2025-02-05", "2025-01-30", "2025-02-05")
#')
#'
#'df<-format_date(database=df,dates_vector=c("date1","date2"),date_format = "%Y-%m-%d")
#'
#'class(df$date1)
#'class(df$date2)

format_date <- function(database, dates_vector, date_format = "%Y-%m-%d") {
  database <- database %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(dates_vector),
      ~ as.Date(.x, format = date_format)
    ))
  return(database)
}

