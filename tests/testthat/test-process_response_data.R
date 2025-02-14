library(testthat)
library(dplyr)

test_that("process_response_data correctly processes response data", {

  # Sample dataset
  df_rrm <- data.frame(
    id = c("A", "A", "B", "B", "C"),
    organization = c("Org1", "Org2", "Org3", NA, "Org4"),
    food_aid = c(1, 0, 1, 1, 0),
    water_sanitation = c(0, 1, 1, 0, 1),
    non_food_items = c(1, 0, 0, 1, 1),
    shelter_support = c(0, 1, 1, 0, 1),
    health_support = c(1, 0, 1, 1, 0),
    protection_services = c(0, 1, 1, 0, 1),
    menstrual_hygiene = c(1, 0, 0, 1, 1),
    fortified_flour = c(0, 1, 1, 0, 1),
    education_support = c(1, 0, 1, 1, 0),
    livelihood_support = c(0, 1, 1, 0, 1),
    households_supported = c(3, 4, 5, NA, 2),
    people_supported = c(10, 15, 20, NA, 5),
    response_start_date = as.Date(c("2024-01-10", "2024-01-15", "2024-02-01", "2024-02-10", "2024-03-05")),
    response_end_date = as.Date(c("2024-01-20", "2024-01-25", "2024-02-10", "2024-02-20", "2024-03-10")),
    donor = c("Donor1", "Donor1", "Donor2", "Donor2", "Donor3"),
    region = c("Region1", "Region1", "Region2", "Region2", "Region3"),
    cercle = c("Cercle1", "Cercle1", "Cercle2", "Cercle2", "Cercle3")
  )

  # Run the function
  cleaned_data <- process_response_data(
    df_rrm,
    uuid = "id",
    response_actor = "organization",
    food_response = "food_aid",
    wash_response = "water_sanitation",
    nfi_response = "non_food_items",
    shelter_response = "shelter_support",
    health_response = "health_support",
    protection_response = "protection_services",
    mhm_response = "menstrual_hygiene",
    enriched_flour_response = "fortified_flour",
    education_response = "education_support",
    livelihood_response = "livelihood_support",
    hh_number = "households_supported",
    ind_number = "people_supported",
    response_start_date = "response_start_date",
    response_end_date = "response_end_date",
    response_donor = "donor",
    admin1 = "region",
    admin2 = "cercle"
  )

  # ✅ Debug: Print column names
  print(names(cleaned_data))

  # ✅ Test 1: Output should be a dataframe
  expect_s3_class(cleaned_data, "data.frame")

  # ✅ Test 2: Expected column names should exist dynamically from function parameters
  expected_columns <- c("id", "response_number", "organization", "food_aid", "water_sanitation", "non_food_items",
                        "shelter_support", "health_support", "protection_services", "menstrual_hygiene",
                        "fortified_flour", "education_support", "livelihood_support", "households_supported",
                        "people_supported", "response_start_date", "response_end_date", "donor", "region", "cercle")

  expect_true(all(expected_columns %in% names(cleaned_data)))

  # ✅ Debug: Check `hh_number` and `ind_number`
  print(cleaned_data$households_supported)
  print(cleaned_data$people_supported)

  # ✅ Test 3: Each `uuid` should be unique in the result
  expect_equal(nrow(cleaned_data), length(unique(df_rrm$id)))

  # ✅ Test 4: Aggregation of household and individual numbers should be correct
  expect_equal(cleaned_data$households_supported[cleaned_data$id == "B"], max(df_rrm$households_supported[df_rrm$id == "B"], na.rm = TRUE))
  expect_equal(cleaned_data$people_supported[cleaned_data$id == "B"], max(df_rrm$people_supported[df_rrm$id == "B"], na.rm = TRUE))

  # ✅ Debug: Check response start date
  print(cleaned_data$response_start_date)

  # ✅ Test 5: The response start date should be the earliest for each uuid
  expect_equal(cleaned_data$response_start_date[cleaned_data$id == "A"], min(df_rrm$response_start_date[df_rrm$id == "A"], na.rm = TRUE))

  # ✅ Test 6: The response end date should be computed correctly as response_start_date + max(date_gap)
  expected_date_gap <- max(df_rrm$response_end_date[df_rrm$id == "A"] - df_rrm$response_start_date[df_rrm$id == "A"], na.rm = TRUE)
  expect_equal(cleaned_data$response_end_date[cleaned_data$id == "A"], cleaned_data$response_start_date[cleaned_data$id == "A"] + expected_date_gap)

})
