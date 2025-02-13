library(testthat)
library(dplyr)

test_that("clean_HH_number correctly imputes missing values", {

  df <- data.frame(
    hh_number = c(10, NA, 15, NA),
    ind_number = c(50, 30, NA, 100)
  )

  hhsize_value <- 5  # Single integer value for household size

  cleaned_df <- clean_HH_number(df, hh_number, ind_number, hhsize_value)

  # Expected calculations
  expected_hh_number <- c(10, 6, 15, 20)  # 6 = round(30 / 5), 20 = round(100 / 5)
  expected_ind_number <- c(50, 30, 75, 100)  # 75 = round(15 * 5)

  # Check if hh_number and ind_number are correctly imputed
  expect_equal(cleaned_df$hh_number, expected_hh_number)
  expect_equal(cleaned_df$ind_number, expected_ind_number)
})

test_that("clean_HH_number ensures hh_number does not exceed ind_number", {

  df <- data.frame(
    hh_number = c(50, 15, 20),
    ind_number = c(40, 30, 100)  # hh_number > ind_number in row 1
  )

  hhsize_value <- 5

  cleaned_df <- clean_HH_number(df, hh_number, ind_number, hhsize_value)

  expected_hh_number <- c(8, 15, 20)  # 8 = round(40 / 5) since 50 was too high
  expected_ind_number <- c(40, 30, 100)  # Unchanged

  expect_equal(cleaned_df$hh_number, expected_hh_number)
  expect_equal(cleaned_df$ind_number, expected_ind_number)
})

test_that("clean_HH_number does not modify rows where data is already correct", {

  df <- data.frame(
    hh_number = c(10, 20, 15),
    ind_number = c(50, 100, 75)  # Already consistent
  )

  hhsize_value <- 5

  cleaned_df <- clean_HH_number(df, hh_number, ind_number, hhsize_value)

  # No change should occur
  expect_equal(cleaned_df$hh_number, df$hh_number)
  expect_equal(cleaned_df$ind_number, df$ind_number)
})

test_that("clean_HH_number handles extreme edge cases", {

  df <- data.frame(
    hh_number = c(NA, NA, 0, 100),
    ind_number = c(NA, 50, 10, 500)
  )

  hhsize_value <- 5

  cleaned_df <- clean_HH_number(df, hh_number, ind_number, hhsize_value)

  expected_hh_number <- c(NA, 10, 0, 100)  # 10 = round(50 / 5), 0 stays 0
  expected_ind_number <- c(NA, 50, 10, 500)  # No changes needed

  expect_equal(cleaned_df$hh_number, expected_hh_number)
  expect_equal(cleaned_df$ind_number, expected_ind_number)
})
