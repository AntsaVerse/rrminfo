% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_monthly_summary_gaps.R
\name{compute_monthly_key_summary_gaps}
\alias{compute_monthly_key_summary_gaps}
\title{Compute Monthly Key Summary Gaps for Alerts and Responses}
\arguments{
\item{alerts_responses_data}{A dataframe containing alerts and response data.}

\item{prev_period_date}{The start date of the previous period (Date object).}

\item{current_period_date}{The start date of the current period (Date object).}

\item{incident_date}{The column name (string) that contains the date of the incident/movement.}

\item{alert_status_col}{The column name (string) indicating the status of the alert.}

\item{valid_status}{The value (string) that represents a valid alert status.}

\item{start_response_date}{The column name (string) that contains the start date of the response.}

\item{alert_ind_number}{The column name (string) containing the number of individuals affected by the alert.}

\item{response_ind_number}{The column name (string) containing the number of individuals assisted during the response.}

\item{desag_by}{(Optional) The column name (string) by which the data should be grouped (e.g., "region", "sector"). Default is \code{NA}, meaning no grouping.}
}
\value{
A dataframe summarizing the key response indicators for the specified period.
}
\description{
This function summarizes key indicators for alerts and responses over a specified period.
It calculates the number of validated alerts, treated alerts, non-treated alerts,
and individuals assisted or not assisted during the response period.
}
\examples{
# Example data
df_alerts <- data.frame(
  uuid = c("A1", "B2", "C3", "D4", "E5"),
  incident_date = as.Date(c("2024-01-05", "2024-01-10", "2023-12-25", "2023-12-30", "2024-01-15")),
  alert_status = c("Validated", "Validated", "Validated", "Validated", "Validated"),
  start_response_date = as.Date(c("2024-01-12", "2024-01-20", "2024-01-05", NA, "2024-01-22")),
  alerted_individuals = c(100, 200, 150, 80, 90),
  responded_individuals = c(80, 180, 120, 0, 85),
  region = c("Region A", "Region A", "Region B", "Region A", "Region C")
)

# Without disaggregation
compute_monthly_key_summary_gaps(
  alerts_responses_data = df_alerts,
  prev_period_date = as.Date("2023-12-01"),
  current_period_date = as.Date("2024-01-01"),
  incident_date = "incident_date",
  alert_status_col = "alert_status",
  valid_status = "Validated",
  start_response_date = "start_response_date",
  alert_ind_number = "alerted_individuals",
  response_ind_number = "responded_individuals"
)

# With disaggregation by region
compute_monthly_key_summary_gaps(
  alerts_responses_data = df_alerts,
  prev_period_date = as.Date("2023-12-01"),
  current_period_date = as.Date("2024-01-01"),
  incident_date = "incident_date",
  alert_status_col = "alert_status",
  valid_status = "Validated",
  start_response_date = "start_response_date",
  alert_ind_number = "alerted_individuals",
  response_ind_number = "responded_individuals",
  desag_by = "region"
)

}
