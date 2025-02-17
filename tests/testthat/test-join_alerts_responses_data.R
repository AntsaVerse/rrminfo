library(testthat)
library(dplyr)

test_that("join_alerts_responses_data correctly merges datasets and calculates time differences", {

  # Sample alert data
  df_alert <- data.frame(
    uuid = c("A1", "B2", "C3"),
    incident_date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    validation_date = as.Date(c("2024-01-05", "2024-02-05", "2024-03-05"))
  )

  # Sample evaluation data
  df_eval <- data.frame(
    uuid = c("A1", "B2", "C3"),
    eval_score = c(80, 85, 90)
  )

  # Sample RRM response data
  df_rrm <- data.frame(
    uuid = c("A1", "B2", "C3"),
    rrm_response_number = c(2, 0, 1),  # B2 has no RRM response
    rrm_start_date = as.Date(c("2024-01-10", NA, "2024-03-10")),
    rrm_end_date = as.Date(c("2024-01-20", NA, "2024-03-15"))
  )

  # Sample Post-RRM response data
  df_postrrm <- data.frame(
    uuid = c("A1", "B2", "C3"),
    postrrm_response_number = c(1, 1, 0),  # C3 has no Post-RRM response
    postrrm_start_date = as.Date(c("2024-01-25", "2024-02-15", NA)),
    postrrm_end_date = as.Date(c("2024-02-01", "2024-02-20", NA))
  )

  # Run the function
  df_combined <- join_alerts_responses_data(
    uuid = "uuid",
    alert_data = df_alert,
    eval_data = df_eval,
    rrm_data = df_rrm,
    rrm_response_number = "rrm_response_number",
    postrrm_data = df_postrrm,
    postrrm_response_number = "postrrm_response_number",
    incident_date = "incident_date",
    validation_date = "validation_date",
    rrm_start_date = "rrm_start_date",
    rrm_end_date = "rrm_end_date",
    postrrm_start_date = "postrrm_start_date",
    postrrm_end_date = "postrrm_end_date"
  )

  # ✅ Test 1: Check if all expected columns exist
  expected_columns <- c("uuid", "incident_date", "validation_date", "eval_score",
                        "rrm_response_number", "rrm_start_date", "rrm_end_date",
                        "postrrm_response_number", "postrrm_start_date", "postrrm_end_date",
                        "has_rrm_response", "has_postrrm_response",
                        "time_alert_to_validation", "time_validation_to_rrm",
                        "time_rrm_duration", "time_rrm_to_postrrm",
                        "time_postrrm_duration", "time_alert_to_rrm", "time_alert_to_postrrm")

  expect_true(all(expected_columns %in% names(df_combined)))

  # ✅ Test 2: Ensure correct calculation of response indicators
  expect_equal(df_combined$has_rrm_response, c(1, 0, 1))  # B2 has no RRM response
  expect_equal(df_combined$has_postrrm_response, c(1, 1, 0))  # C3 has no Post-RRM response

  # ✅ Test 3: Check time difference calculations
  expect_equal(df_combined$time_alert_to_validation, c(4, 4, 4))  # 5 - 1 = 4 days
  expect_equal(df_combined$time_validation_to_rrm, c(5, NA, 5))  # 10 - 5 = 5 days (NA for B2)
  expect_equal(df_combined$time_rrm_duration, c(10, NA, 5))  # 20 - 10 = 10 days (NA for B2)
  expect_equal(df_combined$time_rrm_to_postrrm, c(5, NA, NA))  # 25 - 20 = 5 days (NA if no post-RRM)
  expect_equal(df_combined$time_postrrm_duration, c(7, 5, NA))  # 1 Feb - 25 Jan = 7 days (B2: 5 days)

  # ✅ Test 4: Check cumulative time calculations
  expect_equal(df_combined$time_alert_to_rrm, c(9, NA, 9))  # Alert to RRM = (4+5)
  expect_equal(df_combined$time_alert_to_postrrm, c(14, NA, NA))  # Total duration

})
