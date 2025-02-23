% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_monthly_gap_list.R
\name{compute_monthly_gap_list}
\alias{compute_monthly_gap_list}
\title{Compute Monthly Gap List for Unassisted Alerts}
\arguments{
\item{alerts_responses_data}{A dataframe containing alert and response data.}

\item{uuid}{Character string, the column name representing the unique ID of each alert.}

\item{incident_date}{Character string, the column name representing the incident date.}

\item{hh_number}{Character string, the column name representing the estimated number of households affected.}

\item{ind_number}{Character string, the column name representing the estimated number of individuals affected.}

\item{priority_needs}{Character string, the column name representing priority needs.}

\item{admin1}{Character string, the column name representing the first administrative level (region).}

\item{admin2}{Character string, the column name representing the second administrative level (district).}

\item{regions_mapping}{A character vector containing region names, including "national".}

\item{prev_period_date}{Date, the start date of the analysis period.}

\item{current_period_date}{Date, the end date of the analysis period.}

\item{alert_status_col}{Character string, the column name representing the alert status.}

\item{valid_status}{Character string, the value in \code{alert_status_col} that indicates a valid alert.}

\item{start_response_date}{Character string, the column name representing the response start date.}
}
\value{
A named list where each element corresponds to a region and contains a dataframe
with unassisted alerts in that region.
}
\description{
This function generates a list of unassisted alerts for different regions
by identifying alerts that were validated but did not receive a response
during the given time period.
}
\examples{
# Example dataset
alerts_responses_data <- data.frame(
  uuid = c("A1", "A2", "A3", "A4", "A5"),
  incident_date = as.Date(c("2024-03-01", "2024-03-10", "2024-02-25", "2024-03-15", "2024-03-20")),
  alert_status_col = c("Valid", "Valid", "Valid", "Valid", "Valid"),
  start_response_date = as.Date(c("2024-03-05", "2024-03-15", NA, "2024-03-25", NA)),
  hh_number = c(10, 20, 15, 12, 8),
  ind_number = c(50, 100, 80, 60, 40),
  priority_needs = c("Food", "Shelter", "Health", "Water", "Protection"),
  admin1 = c("Region A", "Region B", "Region A", "Region A", "Region B"),
  admin2 = c("District 1", "District 1", "District 2", "District 3", "District 2")
)

# Define parameters
prev_period_date <- as.Date("2024-03-01")
current_period_date <- as.Date("2024-04-01")
valid_status <- "Valid"
regions_mapping <- c("national", "Region A", "Region B")

# Run the function
resultats <- compute_monthly_gap_list(
  alerts_responses_data = alerts_responses_data,
  uuid = "uuid",
  incident_date = "incident_date",
  hh_number = "hh_number",
  ind_number = "ind_number",
  priority_needs = "priority_needs",
  admin1 = "admin1",
  admin2 = "admin2",
  regions_mapping = regions_mapping,
  prev_period_date = prev_period_date,
  current_period_date = current_period_date,
  alert_status_col = "alert_status_col",
  valid_status = valid_status,
  start_response_date = "start_response_date"
)

# View results for national level
resultats$summary_gap_rrm_list_national

}
