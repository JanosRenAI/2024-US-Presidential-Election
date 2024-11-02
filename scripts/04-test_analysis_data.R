#### Preamble ####
# Purpose: Tests... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 26 September 2024 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(testthat)
library(arrow)
library(dplyr)

# Load the cleaned data
file_path <- "data/02-analysis_data/analysis_data.parquet"
if (file.exists(file_path)) {
  analysis_data <- read_parquet(file_path)
} else {
  stop("File not found: Ensure 'analysis_data.parquet' is in 'data/02-analysis_data/'.")
}

#### Basic Tests ####

test_that("Data structure is correct", {
  # Check that the dataset has the expected 9 columns
  expect_equal(ncol(analysis_data), 9)
  
  # Check that column names are as expected
  expected_columns <- c("poll_id", "pollster", "candidate_name", "sample_size", 
                        "pct", "state", "end_date", "num_harris", "numeric_grade")
  expect_equal(names(analysis_data), expected_columns)
})

test_that("No missing values in critical columns", {
  # Ensure no missing values in critical columns
  critical_columns <- c("poll_id", "pollster", "candidate_name", "sample_size", 
                        "pct", "state", "end_date", "num_harris", "numeric_grade")
  for (col in critical_columns) {
    expect_false(any(is.na(analysis_data[[col]])))
  }
})

cat("Basic tests for analysis_data.parquet passed successfully!\n")
