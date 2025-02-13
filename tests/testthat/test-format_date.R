library(testthat)
library(dplyr)

# Test for format_date function
test_that("format_date correctly converts character columns to Date format", {

  # Create a test dataframe
  df <- data.frame(
    date1 = c("2025-01-28", "2025-01-27", "2025-01-26"),
    organisation_name = c("org_1", "org_2", "org_3"),
    date2 = c("2025-02-05", "2025-01-30", "2025-02-05"),
    stringsAsFactors = FALSE
  )

  # Apply format_date function
  formatted_df <- format_date(df, c("date1", "date2"))

  # Check if columns are now Date objects
  expect_s3_class(formatted_df$date1, "Date")
  expect_s3_class(formatted_df$date2, "Date")
})

test_that("format_date handles missing values correctly", {

  df <- data.frame(
    date1 = c("2025-01-28", NA, "2025-01-26"),
    organisation_name = c("org_1", "org_2", "org_3"),
    date2 = c(NA, "2025-01-30", "2025-02-05"),
    stringsAsFactors = FALSE
  )

  formatted_df <- format_date(df, c("date1", "date2"))

  # Check if missing values remain NA
  expect_true(is.na(formatted_df$date1[2]))
  expect_true(is.na(formatted_df$date2[1]))

  # Check if non-NA values are converted to Date
  expect_s3_class(formatted_df$date1[1], "Date")
  expect_s3_class(formatted_df$date2[3], "Date")
})

test_that("format_date does not modify non-date columns", {

  df <- data.frame(
    date1 = c("2025-01-28", "2025-01-27", "2025-01-26"),
    organisation_name = c("org_1", "org_2", "org_3"),
    date2 = c("2025-02-05", "2025-01-30", "2025-02-05"),
    stringsAsFactors = FALSE
  )

  formatted_df <- format_date(df, c("date1", "date2"))

  # Check if non-date columns are unchanged
  expect_equal(formatted_df$organisation_name, df$organisation_name)
})
