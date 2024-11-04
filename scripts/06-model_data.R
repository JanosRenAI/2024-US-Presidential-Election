#### Preamble ####
# Purpose: Models the
# Author: Xuanang Ren, Caichen Sun
# Date: 1 November 2024
# Contact: ang.ren@mail.utoronto.ca
# License: MIT
# Pre-requisites:Run 03-clean_data.R
# - The `tidyverse`, `arrow`, `rstanarm`, `tibble`  package must be installed and loaded


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(tibble)
library(arrow)

#### Read data ####
just_trump_high_quality <- read_parquet("data/02-analysis_data/trump_analysis_data.parquet")


# Model 1: Simple linear model of pct as a function of end_date
model_date <- lm(pct ~ end_date, data = just_trump_high_quality)

# Model 2: Linear model of pct as a function of end_date and pollster
model_date_pollster <- lm(pct ~ end_date + pollster, data = just_trump_high_quality)

# Model 3: Adding sample size and poll score to improve accuracy
model_date_sample_pollscore <- lm(pct ~ end_date + sample_size + pollscore, data = just_trump_high_quality)

# Model 4: Hierarchical model with pollster as a random effect to account for pollster-specific bias
model_hierarchical <- stan_lmer(pct ~ end_date + (1 | pollster), data = just_trump_high_quality, 
                                prior = normal(0, 5), iter = 2000, chains = 4)

# Model 5: Interaction model with state and time to capture state-specific trends over time
model_date_state <- lm(pct ~ end_date * state + pollscore, data = just_trump_high_quality)

# Augment data with model predictions for visualization
just_trump_high_quality <- just_trump_high_quality |>
  mutate(
    fitted_date = predict(model_date),                     # Model 1 predictions
    fitted_date_pollster = predict(model_date_pollster),   # Model 2 predictions
    fitted_date_sample_pollscore = predict(model_date_sample_pollscore),  # Model 3 predictions
    fitted_date_state = predict(model_date_state)          # Model 5 predictions
  )

#### Visualization ####

# Plot 1: Simple model - Trump Support over Time
ggplot(just_trump_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black", alpha = 0.7) +
  geom_line(aes(y = fitted_date), color = "blue", linetype = "dotted", size = 1) +
  theme_classic() +
  labs(y = "Trump Support (%)", x = "Date", 
       title = "Model 1: Trump Support Over Time",
       subtitle = "Linear Model: pct ~ end_date")

# Plot 2: Model with Pollster - Trump Support Over Time by Pollster
ggplot(just_trump_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "darkgrey", alpha = 0.7) +
  geom_line(aes(y = fitted_date_pollster), color = "blue", linetype = "dashed", size = 1) +
  facet_wrap(vars(pollster), ncol = 3) +
  theme_classic() +
  labs(y = "Trump Support (%)", x = "Date", 
       title = "Model 2: Trump Support Over Time by Pollster",
       subtitle = "Linear Model: pct ~ end_date + pollster")

# Plot 3: Model with Sample Size and Pollscore - Trump Support over Time
ggplot(just_trump_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "darkgrey", alpha = 0.7) +
  geom_line(aes(y = fitted_date_sample_pollscore), color = "darkblue", linetype = "solid", size = 1) +
  theme_classic() +
  labs(y = "Trump Support (%)", x = "Date", 
       title = "Model 3: Trump Support Over Time (With Sample Size & Pollscore)",
       subtitle = "Linear Model: pct ~ end_date + sample_size + pollscore")

# Plot 4: Hierarchical Model by Pollster - Trump Support Over Time
# Hierarchical predictions may require using posterior_predict
just_trump_high_quality %>%
  group_by(pollster) %>%
  ggplot(aes(x = end_date, y = pct, color = pollster)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~ pollster, ncol = 3) +
  theme_minimal() +
  labs(y = "Trump Support (%)", x = "Date",
       title = "Model 4: Trump Support Over Time by Pollster",
       subtitle = "Hierarchical Model: pct ~ end_date + (1 | pollster)")

# Plot 5: State-Specific Trends - Trump Support Over Time by State
ggplot(just_trump_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "darkgrey", alpha = 0.6) +
  geom_line(aes(y = fitted_date_state, color = state), size = 1) +
  facet_wrap(~ state, scales = "free_y") +
  theme_classic() +
  labs(y = "Trump Support (%)", x = "Date",
       title = "Model 5: Trump Support Over Time by State",
       subtitle = "Interaction Model: pct ~ end_date * state + pollscore")

#### Model evaluation ####

# Create a summary list for each model and extract R-squared and AIC values
model_summaries <- list(
  Model_1 = summary(model_date),
  Model_2 = summary(model_date_pollster),
  Model_3 = summary(model_date_sample_pollscore),
  Model_4 = summary(model_hierarchical),
  Model_5 = summary(model_date_state)
)

# Extract and compare R-squared and AIC values
results <- tibble(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  R_squared = c(
    model_summaries$Model_1$r.squared,
    model_summaries$Model_2$r.squared,
    model_summaries$Model_3$r.squared,
    NA, # For hierarchical models, R-squared may not be directly applicable
    model_summaries$Model_5$r.squared
  ),
  AIC = c(
    AIC(model_date),
    AIC(model_date_pollster),
    AIC(model_date_sample_pollscore),
    NA, # AIC for hierarchical models may require custom calculation
    AIC(model_date_state)
  )
)

# Print model evaluation results
print(results)

#### Save models ####
saveRDS(model_date, file = "models/model_date.rds")
saveRDS(model_date_pollster, file = "models/model_date_pollster.rds")
saveRDS(model_date_sample_pollscore, file = "models/model_date_sample_pollscore.rds")
saveRDS(model_hierarchical, file = "models/model_hierarchical.rds")
saveRDS(model_date_state, file = "models/model_date_state.rds")
