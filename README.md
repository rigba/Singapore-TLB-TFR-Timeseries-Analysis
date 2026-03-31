# Singapore TLB and TFR Time Series Exploratory Data Analysis  

## Overview  
This repository contains the code and report for an exploratory data analysis of Singapore Total Live Births (TLB) and Total Fertility Rate (TFR) annual data from 1960 to 2024.  

## Files  
- `script.R` – main analysis script  
- `BirthsAndFertilityRatesAnnual.csv` – raw dataset  from https://data.gov.sg/datasets?query=birth+and+fertility+rates&resultId=d_e39eeaeadb571c0d0725ef1eec48d166&dataExplorerPage=2
## Full EDA Report
[Link to full report](https://drive.google.com/file/d/1a9q_KIdW52x8uGF-3UBIr5iY5kXZzb6-/view?usp=sharing)
## Aim  
The aim of this project is to investigate the temporal features of TLB and TFR, identify plausible time series models, and prepare for later forecasting of 2013–2024.  

## Methods used  
- data cleaning and reshaping  
- time plots  
- ACF and PACF  
- differencing  
- ARIMA model fitting  
- residual diagnostics  
- Ljung–Box tests  
- AIC, AICc, and BIC comparisons  

## Usage  
To reproduce the analysis:  
1. Download `BirthsAndFertilityRatesAnnual.csv` and place it in same folder as `script.R`
2. Open `script.R` in RStudio  
3. Install required packages, including `fpp3`, `readr`, `dplyr`, `tidyr`, and `ggplot2`  
4. Run the script from top to bottom

