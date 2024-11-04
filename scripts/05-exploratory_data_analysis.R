#### Preamble ####
# Purpose: Visualize the cleaned data
# Author: Xuanang Ren, Caichen Sun
# Date: 1 November 2024
# Contact: ang.ren@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, `rstanarm`, `ggplot2` ,`janitor`, `modelsummary` packages must be installed and loaded


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(janitor)
library(ggplot2)
library(modelsummary)

#### Read data ####
# Load cleaned data
analysis_data <- read_parquet("data/02-analysis_data/trump_analysis_data.parquet") |>
  clean_names()

# Display the structure of data
print("Data Structure:")
str(analysis_data)

# Summary statistics for numeric variables
summary(analysis_data)
print("Sample size summary:")
summary(analysis_data$sample_size)
print("Support percentage summary:")
summary(analysis_data$pct)
print("Numeric grade summary:")
summary(analysis_data$numeric_grade)

#### Visualizations of Key Variables ####
# Distribution of support percentages
ggplot(analysis_data, aes(x = pct)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Support for Donald Trump", x = "Support (%)", y = "Count") +
  theme_minimal()

# Distribution of Actual Supporter Count of Trump 
ggplot(analysis_data, aes(x = num_trump)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +
  labs(title = "Histogram of Trump Votes", x = "Number of Trump Votes", y = "Frequency") +
  theme_minimal()


# Distribution of sample size
ggplot(analysis_data, aes(x = sample_size)) +
  geom_histogram(binwidth = 100, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Sample Sizes", x = "Sample Size", y = "Count") +
  theme_minimal()

# Trend of Trump’s support over time by pollster
ggplot(analysis_data, aes(x = end_date, y = pct, color = pollster)) +
  geom_line() +
  labs(title = "Trend of Support for Donald Trump by Pollster", x = "Date", y = "Support (%)") +
  theme_minimal()

# Scatter plot: Relationship between pollster grade and support percentage
ggplot(analysis_data, aes(x = pct, y = numeric_grade)) +
  geom_point(alpha = 0.5) +
  labs(title = "Relationship between Support Percentage and Pollster Grade", x = "Support (%)", y = "Pollster Grade") +
  theme_minimal()



# Summary of logistic regression model
modelsummary(logistic_reg)

# Preparing data for Bayesian model
bayesian_data <- analysis_data |>
  mutate(
    num_trump = round((pct / 100) * sample_size, 0),  # Convert pct to count of Trump support
    state = factor(state)
  )

# Bayesian Model: Hierarchical model with pollster effect
model_formula <- cbind(num_trump, sample_size - num_trump) ~ (1 | pollster)

# Fit Bayesian model
bayesian_model <- stan_glmer(
  formula = model_formula,
  data = bayesian_data,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  seed = 123,
  adapt_delta = 0.95,
  cores = 4
)

# Posterior predictive checks
pp_check(bayesian_model)

# Summarize Bayesian model results
summary(bayesian_model)

# Plot random effects for pollster
plot(bayesian_model, pars = "(Intercept)", prob = 0.95) +
  ggtitle("Bayesian Model: Pollster Effect on Trump Support") +
  theme(plot.title = element_text(hjust = 0.5))

#### Save model ####
# Save logistic regression and Bayesian models for future analysis
saveRDS(logistic_reg, file = "models/logistic_reg.rds")
saveRDS(bayesian_model, file = "models/bayesian_model.rds")

#### Completion Message ####
print("EDA and modeling have been completed.")