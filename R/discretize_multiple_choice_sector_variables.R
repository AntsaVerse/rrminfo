#' Discretize Multiple Choice Variables
#'
#' This function scans a specified column in a dataset to generate binary indicators
#' for multiple-choice responses. It checks for the presence of predefined categories
#' and creates new columns representing whether each category is present (`1`) or absent (`0`).
#'
#' @param df A dataframe containing multiple-choice responses.
#' @param multi_choice_column The name of the column containing multiple-choice responses (comma-separated string).
#' @param category_labels A named character vector where names represent the new column names
#'   and values represent the corresponding label to search for in the `multi_choice_column`.
#'
#' @return A dataframe with additional binary indicator columns for each specified category.
#' @importFrom dplyr mutate
#' @importFrom stringr str_count fixed
#' @export
#'
#' @examples
#' library(dplyr)
#' library(stringr)
#'
#' df <- data.frame(
#'   uuid = c("A1", "B2", "C3"),
#'   selected_options = c("Food, WASH, NFI", "Health, Education, Protection", "Shelter, Livelihoods")
#' )
#'
#' category_labels <- c(
#'   food = "Food",
#'   wash = "WASH",
#'   nfi = "NFI",
#'   shelter = "Shelter",
#'   health = "Health",
#'   education = "Education",
#'   protection = "Protection",
#'   livelihoods = "Livelihoods",
#'   fortified_flour = "Fortified Flour",
#'   mhm = "MHM"
#' )
#'
#' df <- discretize_multiple_choice_variables(df, "selected_options", category_labels)
#' print(df)

discretize_multiple_choice_variables <- function(df, multi_choice_column, category_labels) {

  if (!is.character(category_labels) || is.null(names(category_labels))) {
    stop("category_labels must be a named character vector, with names as new column names and values as search terms.")
  }

  for (category in names(category_labels)) {
    df[[category]] <- str_count(df[[multi_choice_column]], fixed(category_labels[[category]]))
  }

  return(df)
}



