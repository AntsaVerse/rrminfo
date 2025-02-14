#' Process and Aggregate Response Data
#'
#' This function processes and reshapes response data from humanitarian interventions.
#' It aggregates response information at the unique ID (`uuid`) level and calculates summary statistics.
#'
#' @param database A dataframe containing response data.
#' @param uuid The column name representing unique IDs (e.g., alert uuid).
#' @param response_actor Column representing the organizations providing assistance.
#' @param food_response Column indicating food assistance.
#' @param wash_response Column indicating WASH (Water, Sanitation, and Hygiene) response.
#' @param nfi_response Column indicating NFI (Non-Food Items) response.
#' @param shelter_response Column indicating shelter assistance.
#' @param health_response Column indicating health assistance.
#' @param protection_response Column indicating protection assistance.
#' @param mhm_response Column indicating menstrual hygiene management (MHM) assistance.
#' @param enriched_flour_response Column indicating fortified/enriched flour distribution.
#' @param education_response Column indicating education-related assistance.
#' @param livelihood_response Column indicating livelihood support.
#' @param hh_number Column representing the number of supported households.
#' @param ind_number Column representing the number of supported individuals.
#' @param response_start_date Column representing the start date of the response.
#' @param response_end_date Column representing the end date of the response.
#' @param response_donor Column representing the funding donor of the intervention.
#' @param admin1 Column representing the first administrative level (e.g., region).
#' @param admin2 Column representing the second administrative level (e.g., district or cercle).
#'
#' @return A cleaned and reshaped dataframe with aggregated response values.
#' The resulting dataset includes:
#' - `uuid`: Unique ID for each response.
#' - `response_number`: Number of interventions per UUID.
#' - `response_actor`: Aggregated list of response actors.
#' - Binary indicators (`food_response`, `wash_response`, etc.) showing whether assistance was provided.
#' - `hh_number`: The maximum number of supported households.
#' - `ind_number`: The maximum number of supported individuals.
#' - `response_start_date`: The earliest recorded response start date.
#' - `response_end_date`: The computed response end date based on `response_start_date` + `date_gap`.
#' - `response_donor`: The donor(s) funding the response.
#' - `admin1`: The administrative region.
#' - `admin2`: The sub-region or district (cercle).
#'
#' @importFrom dplyr mutate select filter group_by summarise left_join rowwise ungroup any_of row_number if_else n c_across
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym .data :=
#' @importFrom stats na.omit
#' @importFrom tidyselect contains
#' @export
#'
#' @examples
#' # Sample dataset
#' df_rrm <- data.frame(
#'   id = c("A", "A", "B", "B", "C"),
#'   organization = c("Org1", "Org2", "Org3", NA, "Org4"),
#'   food_aid = c(1, 0, 1, 1, 0),
#'   water_sanitation = c(0, 1, 1, 0, 1),
#'   non_food_items = c(1, 0, 0, 1, 1),
#'   shelter_support = c(0, 1, 1, 0, 1),
#'   health_support = c(1, 0, 1, 1, 0),
#'   protection_services = c(0, 1, 1, 0, 1),
#'   menstrual_hygiene = c(1, 0, 0, 1, 1),
#'   fortified_flour = c(0, 1, 1, 0, 1),
#'   education_support = c(1, 0, 1, 1, 0),
#'   livelihood_support = c(0, 1, 1, 0, 1),
#'   households_supported = c(3, 4, 5, NA, 2),
#'   people_supported = c(10, 15, 20, NA, 5),
#'   response_start_date = as.Date(c("2024-01-10", "2024-01-15", "2024-02-01", "2024-02-10", "2024-03-05")),
#'   response_end_date = as.Date(c("2024-01-20", "2024-01-25", "2024-02-10", "2024-02-20", "2024-03-10")),
#'   donor = c("Donor1", "Donor1", "Donor2", "Donor2", "Donor3"),
#'   region = c("Region1", "Region1", "Region2", "Region2", "Region3"),
#'   cercle = c("Cercle1", "Cercle1", "Cercle2", "Cercle2", "Cercle3")
#' )
#'
#' # Run the function
#' cleaned_data <- process_response_data(
#'   df_rrm,
#'   uuid = "id",
#'   response_actor = "organization",
#'   food_response = "food_aid",
#'   wash_response = "water_sanitation",
#'   nfi_response = "non_food_items",
#'   shelter_response = "shelter_support",
#'   health_response = "health_support",
#'   protection_response = "protection_services",
#'   mhm_response = "menstrual_hygiene",
#'   enriched_flour_response = "fortified_flour",
#'   education_response = "education_support",
#'   livelihood_response = "livelihood_support",
#'   hh_number = "households_supported",
#'   ind_number = "people_supported",
#'   response_start_date = "response_start_date",
#'   response_end_date = "response_end_date",
#'   response_donor = "donor",
#'   admin1 = "region",
#'   admin2 = "cercle"
#' )
#'
#' print(cleaned_data)

utils::globalVariables(c("row_num", "response_number"))

process_response_data <- function(database, uuid, response_actor, food_response, wash_response, nfi_response,
                                  shelter_response, health_response, protection_response, mhm_response,
                                  enriched_flour_response, education_response,
                                  livelihood_response, hh_number, ind_number,
                                  response_start_date, response_end_date, response_donor, admin1, admin2) {

  ### Define `date_gap` before using it
  date_gap <- "date_gap"

  database <- database %>%
    mutate(
      !!sym(date_gap) := .data[[response_end_date]] - .data[[response_start_date]]  # Fix: Define `date_gap`
    )

  ### Dynamically map user-defined column names
  value_cols <- c(response_actor, food_response, wash_response, nfi_response,
                  shelter_response, health_response, protection_response, mhm_response,
                  enriched_flour_response, education_response,
                  livelihood_response, hh_number, ind_number,
                  response_start_date, response_end_date, date_gap)

  ### Internal function: Aggregate response data
  aggregate_response_data <- function(data, uuid_col, value_cols) {
    data <- data %>%
      group_by(.data[[uuid_col]]) %>%
      mutate(row_num = row_number()) %>%
      ungroup()

    result <- data %>%
      pivot_wider(
        names_from = row_num,
        names_prefix = paste0(value_cols, "_"),
        values_from = any_of(value_cols),
        names_sep = "_"
      )

    return(result)
  }

  ### Initialize result
  result <- NULL

  ### Loop over each variable to reshape
  for (col in value_cols) {
    if (col %in% names(database)) {  # Check if the column exists
      data <- database %>% select(any_of(c(uuid, col)))  # Prevent missing column errors
      result_col <- aggregate_response_data(data = data, uuid_col = uuid, value_cols = col)

      if (is.null(result)) {
        result <- result_col
      } else {
        result <- result %>% left_join(result_col, by = uuid)
      }
    }
  }

  ### Count number of RRM responses per alert
  response_alert_number <- database %>% group_by(.data[[uuid]]) %>% summarise(response_number = n(), .groups = "drop")

  ### Apply Summarization Rules
  result <- result %>%
    rowwise() %>%  # Ensures calculations are done row-wise
    mutate(
      !!sym(response_actor) := paste(na.omit(c_across(contains(response_actor))), collapse = ","),
      !!sym(food_response) := ifelse(sum(c_across(contains(food_response)), na.rm = TRUE) > 0, 1, 0),
      !!sym(wash_response) := ifelse(sum(c_across(contains(wash_response)), na.rm = TRUE) > 0, 1, 0),
      !!sym(nfi_response) := ifelse(sum(c_across(contains(nfi_response)), na.rm = TRUE) > 0, 1, 0),
      !!sym(shelter_response) := ifelse(sum(c_across(contains(shelter_response)), na.rm = TRUE) > 0, 1, 0),
      !!sym(health_response) := ifelse(sum(c_across(contains(health_response)), na.rm = TRUE) > 0, 1, 0),
      !!sym(protection_response) := ifelse(sum(c_across(contains(protection_response)), na.rm = TRUE) > 0, 1, 0),
      !!sym(mhm_response) := ifelse(sum(c_across(contains(mhm_response)), na.rm = TRUE) > 0, 1, 0),
      !!sym(enriched_flour_response) := ifelse(sum(c_across(contains(enriched_flour_response)), na.rm = TRUE) > 0, 1, 0),
      !!sym(education_response) := ifelse(sum(c_across(contains(education_response)), na.rm = TRUE) > 0, 1, 0),
      !!sym(livelihood_response) := ifelse(sum(c_across(contains(livelihood_response)), na.rm = TRUE) > 0, 1, 0),
      !!sym(date_gap) := max(c_across(contains(date_gap)), na.rm = TRUE),
      !!sym(response_start_date) := min(c_across(contains(response_start_date)), na.rm = TRUE),
      !!sym(response_end_date) := .data[[response_start_date]] + .data[[date_gap]],
      !!sym(hh_number) := max(c_across(contains(hh_number)), na.rm = TRUE),
      !!sym(ind_number) := max(c_across(contains(ind_number)), na.rm = TRUE)

    ) %>%
    ungroup()

  ### Merge with response count
  result <- left_join(result, response_alert_number, by = uuid)

  ### Remove duplicates & keep only the first occurrence per `uuid`
  aggregated_database <- database %>% filter(!duplicated(.data[[uuid]])) %>% select(-any_of(value_cols))

  ### Keep important variables
  variable_rrm <- c(uuid, response_donor, admin1, admin2)

  aggregated_database <- left_join(aggregated_database %>% select(any_of(variable_rrm)), result, by = uuid)

  aggregated_database <-aggregated_database %>% select(uuid,response_number, response_actor, food_response, wash_response, nfi_response,
                                                              shelter_response, health_response, protection_response, mhm_response,
                                                              enriched_flour_response, education_response,
                                                              livelihood_response, hh_number, ind_number,
                                                              response_start_date, response_end_date, response_donor, admin1, admin2)

  return(aggregated_database)
}


