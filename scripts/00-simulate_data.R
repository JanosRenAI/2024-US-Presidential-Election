#### Preamble ####
# Purpose: Simulates a dataset of polling data for Australian electoral divisions
# Author: Xuanang Ren
# Date: 26 September 2024
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Prerequisites: The `tidyverse` package must be installed
# Notes: Run within the `starter_folder` R project.

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(readr)
set.seed(853) # Ensures reproducibility

#### Simulate data #### 

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(readr)
set.seed(853) # Ensures reproducibility

#### Simulate data #### 
# Define lists of pollsters, candidates, and dates for realistic sampling
pollsters <- c("Redfield & Wilton Strategies", "TIPP Insights", "Ipsos", "Gallup", "YouGov")
candidates <- c("Kamala Harris", "Donald Trump", "Joe Biden", "Nikki Haley")
date_sequence <- seq.Date(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")

# Generate the dataset with sampled values
simulated_poll_data <- tibble(
  poll_id = as.integer(sample(10000:20000, 151, replace = TRUE)),  # Unique poll IDs
  pollster = sample(pollsters, size = 151, replace = TRUE),        # Random pollster
  candidate_name = sample(candidates, size = 151, replace = TRUE),  # Random candidate
  sample_size = sample(500:3000, 151, replace = TRUE),              # Sample size within range
  pct = round(runif(151, 30, 70), 1),                              # Percentage support between 30% and 70%
  start_date = as.Date(sample(date_sequence, size = 151, replace = TRUE)),   # Random start date
  transparency_score = round(runif(151, 1, 10), 1),                # Transparency score from 1 to 10
  numeric_grade = round(runif(151, 1.0, 4.0), 1),                  # Poll quality grade from 1.0 to 4.0
  pollscore = round(runif(151, -1.5, 1.5), 1)                      # Poll score between -1.5 and 1.5
) %>%
  mutate(
    # Ensure end_date is after start_date within a 1-10 day range
    end_date = as.Date(start_date + days(sample(1:10, 151, replace = TRUE))),
    # Calculate days to election, non-negative
    days_to_election = pmax(0, as.numeric(as.Date("2024-11-05") - end_date))
  ) %>%
  # Assign candidates with a specific probability
  mutate(
    candidate_name = case_when(
      runif(n()) < 0.2 & candidate_name == "Kamala Harris" ~ "Kamala Harris",
      runif(n()) < 0.2 & candidate_name == "Donald Trump" ~ "Donald Trump",
      TRUE ~ candidate_name
    )
  )

#### Save data ####
# Set path to save data, create directory if necessary, and save CSV
save_path <- "data/00-simulated_data/simulated_poll_data.csv"
dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
glimpse(simulated_poll_data) # Check structure of the dataset
write_csv(simulated_poll_data, save_path)
