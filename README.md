# Time-Series-Analysis-ARIMA-Modeling
Time series analysis using ARIMA models for forecasting and residual analysis.

This repository contains R code for performing time series analysis using ARIMA models. The main objective is to analyze and forecast a time series dataset, with a specific focus on model fitting, residual analysis, and prediction.

## Key Features:
- **Data Preprocessing**: Loading, differencing, and performing stationarity tests on the time series data.
- **Model Fitting**: ARIMA models of various orders are fitted, with a focus on finding the best model based on BIC.
- **Residual Analysis**: Residuals of the models are analyzed through ACF, PACF, and Ljung-Box tests to check for randomness.
- **Model Diagnostics**: Shapiro-Wilk test, QQ plots, and histograms are used for testing normality of residuals.
- **Forecasting**: ARIMA models are used for forecasting future values, with 10-step and 25-step predictions, along with 95% confidence intervals.

## Prerequisites:
- **R**: Make sure to have R installed on your system.
- **Libraries**: The script requires the following libraries:
  - `TSA`
  - `lmtest`
  - `tseries`

These can be installed using the following commands:
```r
install.packages("TSA")
install.packages("lmtest")
install.packages("tseries")
