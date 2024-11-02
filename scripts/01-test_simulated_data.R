#### Preamble ####
# Purpose: Tests the structure and validity of the simulated Australian 
  #electoral divisions dataset.
# Author: Xuanang Ren
# Date: 26 September 2024
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded
  # - 00-simulate_data.R must have been run
# Any other information needed? Make sure you are in the `starter_folder` rproj


#### Workspace setup ####
library(tidyverse)
library(testthat)

simulated_poll_data <- read_csv("data/00-simulated_data/simulated_poll_data.csv")

# Run tests
test_simulated_data <- function(simulated_poll_data) {
  
  # Check if data is loaded successfully
  if (is.null(simulated_poll_data) || nrow(simulated_poll_data) == 0) {
    stop("Data not loaded successfully or is empty. Please check data source.")
  } else {
    cat("Data loaded successfully.\n")
  }
  
  # 1. Check that the dataset has the expected number of rows and columns
  expect_equal(ncol(simulated_poll_data), 11, info = "Data should have 11 columns.")
  expect_equal(nrow(simulated_poll_data), 151, info = "Data should have 151 rows.")
  
  # 2. Test data types for each column
  expect_type(simulated_poll_data$poll_id, "double")
  expect_type(simulated_poll_data$pollster, "character")
  expect_type(simulated_poll_data$candidate_name, "character")
  expect_type(simulated_poll_data$sample_size, "double")
  expect_type(simulated_poll_data$pct, "double")
  expect_type(simulated_poll_data$start_date, "double")
  expect_type(simulated_poll_data$end_date, "double")
  expect_type(simulated_poll_data$transparency_score, "double")
  expect_type(simulated_poll_data$numeric_grade, "double")
  expect_type(simulated_poll_data$pollscore, "double")
  expect_type(simulated_poll_data$days_to_election, "double")
  
  # 3. Check value ranges
  expect_true(all(simulated_poll_data$sample_size >= 500 & simulated_poll_data$sample_size <= 3000),
              info = "Sample size should be between 500 and 3000.")
  expect_true(all(simulated_poll_data$pct >= 30 & simulated_poll_data$pct <= 70),
              info = "Percentage (pct) should be between 30% and 70%.")
  expect_true(all(simulated_poll_data$transparency_score >= 1 & simulated_poll_data$transparency_score <= 10),
              info = "Transparency score should be between 1 and 10.")
  expect_true(all(simulated_poll_data$numeric_grade >= 1.0 & simulated_poll_data$numeric_grade <= 4.0),
              info = "Numeric grade should be between 1.0 and 4.0.")
  expect_true(all(simulated_poll_data$pollscore >= -1.5 & simulated_poll_data$pollscore <= 1.5),
              info = "Poll score should be between -1.5 and 1.5.")
  expect_true(all(simulated_poll_data$days_to_election >= 0),
              info = "Days to election should be non-negative.")
  
  # 4. Check logical consistency
  expect_true(all(simulated_poll_data$end_date >= simulated_poll_data$start_date),
              info = "End date should be after start date.")
  
  # 5. Check for missing values in critical columns
  expect_false(any(is.na(simulated_poll_data$poll_id)), info = "Poll ID should not have missing values.")
  expect_false(any(is.na(simulated_poll_data$pollster)), info = "Pollster should not have missing values.")
  expect_false(any(is.na(simulated_poll_data$candidate_name)), info = "Candidate name should not have missing values.")
  expect_false(any(is.na(simulated_poll_data$start_date)), info = "Start date should not have missing values.")
  expect_false(any(is.na(simulated_poll_data$end_date)), info = "End date should not have missing values.")
  
  cat("All tests passed successfully!\n")
}

# Run the test function on simulated_poll_data
test_simulated_data(simulated_poll_data)

