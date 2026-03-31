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


# Both series show a notable downwards trend over time, with a pronounced temporary break of 
# structure at year 1988. Since the data is annual, we generally wouldn't expect to see seasonal 
# trends. However due to a well documented cultural phenomenon, we tend to see modest spikes at 
# 12 year intervals from 1964 onwards. Superstitions around timing child births at specific 
# Chinese zodiac years, in particular the dragon year, is a well documented phenomenon.


# To further understand the dependence structure of both time series,
# I've decided to plot and analyse the partial auto correlation function and
# auto correlation functions

tlb %>% ACF(TLB) %>% autoplot() + labs(title = "ACF of TLB")
tlb %>% PACF(TLB) %>% autoplot() + labs(title = "PACF of TLB")

tfr %>% ACF(TFR) %>% autoplot() + labs(title = "ACF of TFR")
tfr %>% PACF(TFR) %>% autoplot() + labs(title = "PACF of TFR")

# The ACFs for both time series show a substantial number of autocorrelations 
# outside the 95% significance bounds. This supports the earlier conclusion from 
# visual inspection that both series are non-stationary. In particular, both ACFs 
# decay slowly across many lags, which is consistent with strong persistence in 
# trending series. The TLB ACF appears to decay slightly faster than the TFR ACF.

# The PACFs for both time series show a significant value at lag 1, followed by 
# non-significant values at higher lags. This suggests that most of the direct 
# dependence is concentrated at lag 1.

# Notably, the 12-year effect is visible in the raw plot, but it is not strong or regular 
# enough to overbear the autocorrelation structure of the raw series.


# Because this persistent trend can mask the underlying dependence structure, differencing 
# was applied to each series. This transforms the data into year-to-year changes, helping 
# to stabilise the mean and making the autocorrelation patterns easier to interpret.
# The number of differences required was then assessed using a unitroot_ndiffs test,
# applying the relevant transformations, and finally generating a simple time plot.

tlb %>% features(TLB, unitroot_ndiffs) # = 1
tfr %>% features(TFR, unitroot_ndiffs) # = 2


tlb_differenced <- tlb %>%
  mutate(D_TLB = difference(TLB, lag = 1)) %>%
  filter(!is.na(D_TLB))

tfr_differenced <- tfr %>%
  mutate(
    D_TFR = difference(TFR, lag = 1),
    DD_TFR = difference(D_TFR, lag = 1)
  ) %>%
  filter(!is.na(DD_TFR))

# differenced series plots
autoplot(tlb_differenced, D_TLB) +
  labs(title = "First-differenced TLB", y = "Change in TLB")

autoplot(tfr_differenced, DD_TFR) +
  labs(title = "Second-differenced TFR", y = "Second difference in TFR")

# Upon visual inspection, both series show promising results for their respected number 
# of recommended differences. The differencing has appeared to mostly remove the trend 
# seen previously, and aside from the spike observed in 1988, the fluctuations appear 
# mostly random.

# Next looking at the ACFs and PACFs

tlb_differenced %>%
  ACF(D_TLB) %>%
  autoplot() +
  labs(title = "ACF of first-differenced TLB")

tfr_differenced %>%
  ACF(DD_TFR) %>%
  autoplot() +
  labs(title = "ACF of second-differenced TFR")

tlb_differenced %>%
  PACF(D_TLB) %>%
  autoplot() +
  labs(title = "PACF of first-differenced TLB")


tfr_differenced %>%
  PACF(DD_TFR) %>%
  autoplot() +
  labs(title = "PACF of second-differenced TFR")

# Generally the ACFs of the differenced TLB and TFR have largely improved, with fewer values
# outside the 95% confidence bounds. The PACFs echo the ACF results, with both series 
# responding better to differencing. In both cases, the lag structure is more irregular 
# than in the original series, though the TFR still shows some values outside the 
# confidence bounds. 

# In particular, for all series we see significant deviations at or around lag 12 for 
# ACF and PACF values. This result strengthens our hypothesis that the trend component of 
# both series was overbearing our ability to spot the Chinese zodiac birth timing 
# phenomenon previously discussed.

# I wanted to confirm my visual observations by running another formal test

tlb %>% features(TLB, unitroot_kpss) # p = 0.01
tlb_differenced %>% features(D_TLB, unitroot_kpss) # p = 0.1

tfr %>% features(TFR, unitroot_kpss) # p = 0.01
tfr_differenced %>% features(DD_TFR, unitroot_kpss) # p = 0.1

# Overall, these results suggest that differencing is likely to be more appropriate
# than modelling the raw series directly, particularly for TLB.

# I fitted a range of candidate ARIMA models to the training subseries (1960-2012) and 
# examined their residual diagnostics. This was done to assess whether the fitted models 
# had adequately captured the temporal dependence remaining after differencing. In 
# particular, I considered the innovation residual plots, residual ACF values, residual 
# histograms, Ljung-Box test results, and the information criteria AIC, AICc and BIC.

tlb_train <- tlb %>%
  filter(Year <= 2012)

tfr_train <- tfr %>%
  filter(Year <= 2012)

tlb_fit <- tlb_train %>% model(
  arima_auto = ARIMA(TLB), # p= 0, d= 1, q= 0
  arima_110 = ARIMA(TLB ~ pdq(1, 1, 0)),
  #arima_011 = ARIMA(TLB ~ pdq(0, 1, 1)), removed because auto
  arima_111 = ARIMA(TLB ~ pdq(1, 1, 1))
) 

tfr_fit <- tfr_train %>% model(
  arima_auto = ARIMA(TFR), # p = 0, d= 2, q= 1
  #arima_021 = ARIMA(TFR ~ pdq(0, 2, 1)), removed because auto
  arima_120 = ARIMA(TFR ~ pdq(1, 2, 0)),
  arima_121 = ARIMA(TFR ~ pdq(1, 2, 1))
)


glance(tlb_fit)
glance(tfr_fit)

gg_tsresiduals(tlb_fit %>% select(arima_auto)) +
  labs(title = "Residuals: TLB ARIMA(0,1,0)")

augment(tlb_fit %>% select(arima_auto)) %>%
  features(.resid, ljung_box, lag = 10, dof = 0)


gg_tsresiduals(tlb_fit %>% select(arima_110)) +
  labs(title = "Residuals: TLB ARIMA(1,1,0)")

augment(tlb_fit %>% select(arima_110)) %>%
  features(.resid, ljung_box, lag = 10, dof = 1)


gg_tsresiduals(tlb_fit %>% select(arima_111)) +
  labs(title = "Residuals: TLB ARIMA(1,1,1)")

augment(tlb_fit %>% select(arima_111)) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)


gg_tsresiduals(tfr_fit %>% select(arima_auto)) +
  labs(title = "Residuals: TFR ARIMA(0,2,1)")

augment(tfr_fit %>% select(arima_auto)) %>%
  features(.resid, ljung_box, lag = 10, dof = 1)


gg_tsresiduals(tfr_fit %>% select(arima_120)) +
  labs(title = "Residuals: TFR ARIMA(1,2,0)")

augment(tfr_fit %>% select(arima_120)) %>%
  features(.resid, ljung_box, lag = 10, dof = 1)


gg_tsresiduals(tfr_fit %>% select(arima_121)) +
  labs(title = "Residuals: TFR ARIMA(1,2,1)")

augment(tfr_fit %>% select(arima_121)) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)

# The residual diagnostics return generally promising results across both time series. 
# For all fitted models, the innovation residuals fluctuate in a random looking pattern 
# around zero for most of the sample, though each model shows a noticeable disturbance 
# around 1988. The residual ACF plots further suggest that the temporal dependence within 
# the series has been substantially accounted for by the fitted models. However, for nearly 
# all models there remains some deviation around lag 12, indicating that traces of the 
# apparent 12-year birth timing phenomenon may still persist even after differencing and 
# model fitting. 

# The residual histograms also appear broadly acceptable. In most cases the residuals are 
# approximately centred and reasonably symmetric, suggesting that the normality assumption 
# is not severely violated. The main exception is for the TLB models, where a small number 
# of large positive residuals around +10000, remain visible.

# The formal diagnostic results support these visual impressions also. For TLB, all three 
# candidate models return acceptable Ljung-Box results, suggesting that the residuals are 
# broadly consistent with white noise. Among these, ARIMA(0,1,0) is preferred by AIC, AICc 
# and BIC. For TFR, all three models also produce acceptable Ljung-Box results, although 
# ARIMA(1,2,0) appears weakest in terms of residual independence. According to AIC, AICc 
# and BIC, ARIMA(0,2,1) is the preferred model for TFR. At this preliminary stage 
# ARIMA(0,1,0) and ARIMA(0,2,1) appear to be the strongest candidates for further forecasting 
# analysis.


# After some consideration of the previous analysis I decided to further investigate the specific 
# "mini baby boom" around 1988. This led me to develop the hypothesis that the change in policy 
# coinciding with the Chinese zodiac dragon year timing caused a temporary spike in children 
# being born. This prompted me to redo the analysis by modifying both series such that their 
# values for 1988 were an average of 1987 and 1989 like so:

tlb <- tlb %>%
  mutate(
    TLB = if_else(
      Year == 1988,
      (TLB[Year == 1987] + TLB[Year == 1989]) / 2,
      TLB
    )
  )

tfr <- tfr %>%
  mutate(
    TFR = if_else(
      Year == 1988,
      (TFR[Year == 1987] + TFR[Year == 1989]) / 2,
      TFR
    )
  )


tlb_train <- tlb %>%
  filter(Year <= 2012)

tfr_train <- tfr %>%
  filter(Year <= 2012)

tlb_fit <- tlb_train %>% model(
  arima_auto = ARIMA(TLB), # p= 0, d= 1, q= 0
  arima_110 = ARIMA(TLB ~ pdq(1, 1, 0)),
  #arima_011 = ARIMA(TLB ~ pdq(0, 1, 1)), removed because auto
  arima_111 = ARIMA(TLB ~ pdq(1, 1, 1))
) 

tfr_fit <- tfr_train %>% model(
  arima_auto = ARIMA(TFR), # p = 0, d= 2, q= 1
  #arima_021 = ARIMA(TFR ~ pdq(0, 2, 1)), removed because auto
  arima_120 = ARIMA(TFR ~ pdq(1, 2, 0)),
  arima_121 = ARIMA(TFR ~ pdq(1, 2, 1))
)


glance(tlb_fit)
glance(tfr_fit)

gg_tsresiduals(tlb_fit %>% select(arima_auto)) +
  labs(title = "Residuals: TLB ARIMA(0,1,0)")

augment(tlb_fit %>% select(arima_auto)) %>%
  features(.resid, ljung_box, lag = 10, dof = 0)


gg_tsresiduals(tlb_fit %>% select(arima_110)) +
  labs(title = "Residuals: TLB ARIMA(1,1,0)")

augment(tlb_fit %>% select(arima_110)) %>%
  features(.resid, ljung_box, lag = 10, dof = 1)


gg_tsresiduals(tlb_fit %>% select(arima_111)) +
  labs(title = "Residuals: TLB ARIMA(1,1,1)")

augment(tlb_fit %>% select(arima_111)) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)


gg_tsresiduals(tfr_fit %>% select(arima_auto)) +
  labs(title = "Residuals: TFR ARIMA(0,2,1)")

augment(tfr_fit %>% select(arima_auto)) %>%
  features(.resid, ljung_box, lag = 10, dof = 1)


gg_tsresiduals(tfr_fit %>% select(arima_120)) +
  labs(title = "Residuals: TFR ARIMA(1,2,0)")

augment(tfr_fit %>% select(arima_120)) %>%
  features(.resid, ljung_box, lag = 10, dof = 1)


gg_tsresiduals(tfr_fit %>% select(arima_121)) +
  labs(title = "Residuals: TFR ARIMA(1,2,1)")

augment(tfr_fit %>% select(arima_121)) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)

# Visual analysis of the tsresiduals plots, compared with the non-1988 averaged plots, show 
# modest improvements across all models in the innovation residuals, ACF values, and residual 
# skews. In particular, the innovation residuals over time no longer show a structural break 
# around 1988, the ACF values around lag 12 are closer to zero, and the outliers at 10,000 
# in the TLB residual skew plots are no longer present.

# In terms of the statistical tests, averaging the 1988 outlier year does not change the 
# overall differencing conclusions: TLB still requires one difference, while TFR still 
# requires two differences.

# For TLB, all three models continue to pass the Ljung-Box test, but their p-values increase 
# after smoothing the 1988 value, indicating residuals that remain consistent with white noise. 
# For TFR, the model fit statistics also improve overall, although the Ljung-Box results 
# improve less noticeably.

# Overall, the formal test results and visual inspections suggest that averaging out the 1988 
# anomaly does not change the preferred ARIMA model parameters, but it does lead to modest 
# improvements in residual behaviour and fit statistics, particularly for the TLB models.