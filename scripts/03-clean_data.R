#### Preamble ####
# Purpose: Cleans the raw presidential polling data obtained from FiveThirtyEight
# Author: Xuanang Ren, Caichen Sun
# Date: 1 November 2024
# Contact: ang.ren@mail.utoronto.ca
# License: MIT
# Pre-requisites:
  # - The `tidyverse`, `arrow`, `janitor`, `lubridate`  package must be installed and loaded


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(janitor)
library(lubridate) 

#### Clean data ####
#### Prepare dataset ####
# Set threshold for high-quality polls
grade_threshold <- 2.7

# Read in the data and clean variable names
data <- read_csv("data/01-raw_data/raw_data.csv") |>
  clean_names()

# Filter data to include only Trump records with numeric_grade above threshold and after Trump declared
trump_data <- data |>
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= grade_threshold
  ) |>
  select(pollster, numeric_grade, pollscore, state, end_date, sample_size, pct) |>
  drop_na() |>
  mutate(
    state = if_else(is.na(state), "National", state) # Handle missing state values as "National"
  ) |>
  group_by(state) |>
  mutate(
    state_count = n() # Count the number of polls per state
  ) |>
  ungroup() |>
  mutate(
    state = if_else(state_count < 60, "Other", state) # Assign "Other" for states with fewer than 60 polls
  ) |>
  select(-state_count) # Remove the state_count column

# Convert date format and filter records after Trump’s declaration date
trump_data <- trump_data |>
  mutate(end_date = mdy(end_date)) |>
  filter(end_date >= as.Date("2024-07-21")) # Trump declared date

# Remove outliers (pct values greater than 100 or less than 0)
trump_data <- trump_data |>
  filter(pct >= 0 & pct <= 100)

# Calculate the number of Trump supporters and filter pollsters with more than 20 polls
trump_data <- trump_data |>
  mutate(
    num_trump = round((pct / 100) * sample_size, 0) # Convert pct to actual supporter numbers
  ) |>
  group_by(pollster) |>
  filter(n() > 20) |> # Keep pollsters with more than 20 polls
  ungroup()

#### Plot data ####
base_plot <- ggplot(trump_data, aes(x = end_date, y = pct)) +
  theme_classic() +
  labs(y = "Trump percent", x = "Date")

# Plot poll estimates with smoothing
base_plot +
  geom_point() +
  geom_smooth()

# Color by pollster to visualize variation among pollsters
base_plot +
  geom_point(aes(color = pollster)) +
  geom_smooth() +
  theme(legend.position = "bottom")

# Facet by pollster to view individual trends
base_plot +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(pollster), ncol = 3)

# Color by pollscore to observe quality variation
base_plot +
  geom_point(aes(color = factor(pollscore))) +
  geom_smooth() +
  theme(legend.position = "bottom")

#### Save data ####
write_csv(trump_data, "data/02-analysis_data/cleaned_trump_voting.csv")
write_parquet(trump_data, "data/02-analysis_data/trump_analysis_data.parquet")