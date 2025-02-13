library(testthat)
library(dplyr)
library(lubridate)

# Test for clean_dates function
test_that("clean_dates correctly calculates days_gap and replaces outliers", {

  # Create a test dataframe with normal and extreme date gaps
  df <- data.frame(
    id = 1:6,
    start_date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01", "2024-06-01")),
    end_date = as.Date(c("2024-01-10", "2024-02-15", "2024-03-20", "2030-01-01", "2024-05-20", NA))
  )

  # Apply clean_dates function
  cleaned_df <- clean_dates(df, start_date, end_date)

  # Check if days_gap was correctly replaced for the outlier (extreme date)
  days_gap <- as.numeric(difftime(cleaned_df$end_date, cleaned_df$start_date, units = "days"))

  expect_true(all(days_gap <= 365 * 5 | is.na(days_gap))) # Ensure extreme gaps are replaced
})

test_that("clean_dates correctly imputes missing start_date or end_date", {

  df <- data.frame(
    id = 1:5,
    start_date = as.Date(c("2024-01-01", NA, "2024-03-01", "2024-04-01", NA)),
    end_date = as.Date(c("2024-01-10", "2024-02-15", NA, "2024-05-01", "2024-06-15"))
  )

  cleaned_df <- clean_dates(df, start_date, end_date)

  # Check if missing start_date is correctly imputed
  expect_true(!is.na(cleaned_df$start_date[2]))
  expect_true(is.na(cleaned_df$end_date[3]))
})

test_that("clean_dates does not modify non-date columns", {

  df <- data.frame(
    id = 1:3,
    start_date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    end_date = as.Date(c("2024-01-10", "2024-02-15", "2024-03-20")),
    category = c("A", "B", "C")
  )

  cleaned_df <- clean_dates(df, start_date, end_date)

  # Ensure the "category" column remains unchanged
  expect_equal(cleaned_df$category, df$category)
})
