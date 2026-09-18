
# Loading Required Libraries
library(tidyverse)
library(car)  # For Variance Inflation Factor (VIF) testing

# Loading the raw dataset 
raw_data <- read.csv("world-data-2023.csv", stringsAsFactors = FALSE)

# Cleaning the dataset to remove symbols that aren't required
clean_numeric <- function(column) {
  as.numeric(gsub("[$,%]", "", column))
}

# Applying Cleaning & Coerce Vector Types
df <- raw_data %>%
  mutate(
    Armed_Forces_size = clean_numeric(Armed.Forces.size),
    GDP               = clean_numeric(GDP),
    Population        = clean_numeric(Population),
    Land_Area         = clean_numeric(Land.Area.Km2.),
    Co2_Emissions     = clean_numeric(Co2.Emissions),
    Life_Expectancy   = clean_numeric(Life.expectancy)
  )

# Filtering out Zero, Missing and NA Values 
df_clean <- df %>%
  filter(
    !is.na(Armed_Forces_size) & Armed_Forces_size > 0,
    !is.na(GDP) & GDP > 0,
    !is.na(Population) & Population > 0
  )

# Applying Natural Log and defining new variables 
df_clean <- df_clean %>%
  mutate(
    log_military = log(Armed_Forces_size),
    log_gdp      = log(GDP),
    log_pop      = log(Population),
    log_land     = log(Land_Area),
    log_co2      = log(Co2_Emissions)
  )

# Descriptive Statistics Summary Table
summary_stats <- df_clean %>%
  select(log_military, log_gdp, log_pop) %>%
  summary()
print(summary_stats)

# Setting up OLS Multiple Linear Regression Model
military_model <- lm(log_military ~ log_gdp + log_pop, data = df_clean)

# Getting statistical data and its results through VIF testing
summary(military_model)  # Yields Coeffs, R-Squared, F-Statistic, p-values
vif(military_model)      # Checks for Multicollinearity 

# Bivariate Pearson Correlation coefficients
cor_matrix <- cor(
  df_clean %>% select(log_military, log_gdp, log_pop, log_land, log_co2, Life_Expectancy),
  use = "complete.obs"
)
print(round(cor_matrix, 3))

# Export image of 2x2 Model Diagnostic Plots 
png("regression_diagnostics.png", width = 1000, height = 800, res = 120)
par(mfrow = c(2, 2))
plot(military_model)
dev.off()

# Making Descriptive Summary Table & Histograms
library(psych)
describe(df_clean %>% select(Armed_Forces_size, GDP, Population, log_military, log_gdp, log_pop))

# Visualizations: Histograms & Scatter Plots
par(mfrow = c(2, 2))
hist(df_clean$Armed_Forces_size, main = "Raw Military Size", xlab = "Headcount")
hist(df_clean$log_military, main = "Log Military Size", xlab = "ln(Headcount)")
plot(df_clean$log_gdp, df_clean$log_military, main = "ln(Military) vs ln(GDP)", xlab = "ln(GDP)", ylab = "ln(Military)")
plot(df_clean$log_pop, df_clean$log_military, main = "ln(Military) vs ln(Population)", xlab = "ln(Population)", ylab = "ln(Military)")


# Output of Standard regression output (Coefficients, SE, t-stats, p-values, R-squared, F-stat)
summary(military_model)

# Output of VIF values for multicollinearity
vif(military_model)