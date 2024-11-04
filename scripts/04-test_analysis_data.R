#### Preamble ####
# Purpose: Tests the cleaned data for the Trump voting analysis
# Author: Xuanang Ren, Caichen Sun
# Date: 1 November 2024
# Contact: ang.ren@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `testthat`, `arrow`, `dplyr` packages must be installed and loaded
# Note: use Ctrl+Enter (Windows), Cmd+Enter (Mac) to run the test.


#### Workspace setup ####
library(testthat)
library(arrow)
library(dplyr)

# Load the cleaned data
file_path <- "data/02-analysis_data/trump_analysis_data.parquet"
if (file.exists(file_path)) {
  analysis_data <- read_parquet(file_path)
} else {
  stop("File not found: Ensure 'trump_analysis_data.parquet' is in 'data/02-analysis_data/'.")
}

#### Basic Tests ####

test_that("Data structure is correct", {
  # Check that the dataset has the expected columns
  expected_columns <- c("pollster", "numeric_grade", "pollscore", "state", "end_date", 
                        "sample_size", "pct", "num_trump")
  expect_equal(names(analysis_data), expected_columns)
  
  # Check that there are no extra columns
  expect_equal(ncol(analysis_data), length(expected_columns))
})

test_that("No missing values in critical columns", {
  # Ensure no missing values in essential columns
  critical_columns <- c("pollster", "numeric_grade", "state", "end_date", "sample_size", "pct", "num_trump")
  for (col in critical_columns) {
    expect_false(any(is.na(analysis_data[[col]])), info = paste("Missing values found in", col))
  }
})

test_that("Date column is in the correct format", {
  # Check that `end_date` is in Date format
  expect_true(all(class(analysis_data$end_date) == "Date"))
})

test_that("Percentage values are within expected range", {
  # Ensure `pct` values are between 0 and 100
  expect_true(all(analysis_data$pct >= 0 & analysis_data$pct <= 100))
})

test_that("Numeric values are within expected range", {
  # Check that numeric grades meet the threshold
  expect_true(all(analysis_data$numeric_grade >= 2.7))
})

test_that("State classification is correct", {
  # Check that states are either specified, "National", or "Other"
  unique_states <- unique(analysis_data$state)
  expect_true(all(unique_states %in% c("National", "Other", unique(data$state))))
})

cat("All tests for trump_analysis_data.parquet passed successfully!\n")
