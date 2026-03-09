library(fpp3)
library(readr)

data <- read_csv("BirthsAndFertilityRatesAnnual.csv")

# very basic visual inspections of the data
library(ggplot2)

process_year_values <- function(x) {
  ifelse(x == "na", NA, as.numeric(x)) # some values in the year columns are "na"
}                                     # so we need to convert them to NA

# We want the data to be in the form
# Timeseries, year, value
# Hence the data needs to be expanded as currently its in the form:
# timeseries, 2025 value, 2024 value, ..., 1960  value

# long format
data_long <- data %>%
  pivot_longer(
    cols = -DataSeries,
    names_to = "Year",
    values_to = "Value",
    values_transform = list(Value = as.character)
  ) %>%
  mutate(Year = as.integer(Year), Value = process_year_values(Value)) %>%
  filter(Year <= 2024) 


# pull out the two series early and rename nicely
tlb <- data_long %>%
  filter(DataSeries == "Total Live-Births") %>%
  select(Year, TLB = Value) %>%
  as_tsibble(index = Year)

tfr <- data_long %>%
  filter(DataSeries == "Total Fertility Rate (TFR)") %>%
  select(Year, TFR = Value) %>%
  as_tsibble(index = Year)

