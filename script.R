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


# Now I wanna create a dual axis graph of TLB and TFR over time so I need a scaling factor
# I need to put the data in year, tfr value, tlb value first

data_wide <- left_join(tlb, tfr, by = "Year")


scale_factor <- max(data_wide$TLB, na.rm = TRUE) / max(data_wide$TFR, na.rm = TRUE)

ggplot(data_wide, aes(x = Year)) +
  geom_line(aes(y = TLB, colour = "TLB"), linewidth = 1) +
  geom_line(aes(y = TFR * scale_factor, colour = "TFR"),
            linewidth = 1) +
  scale_y_continuous(name = "Total Live Births",
                     sec.axis = sec_axis( ~ . / scale_factor, name = "Total Fertility Rate")) +
  scale_colour_manual(name = NULL,
                      values = c("TLB" = "red", "TFR" = "blue")) +
  labs(x = "Year") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue")
  )


# The presence trend is easy to spot via visual inspection of this dual axis graph
# Since the data is not monthly or quarterly we are unlikely to spot any seasonal trends

