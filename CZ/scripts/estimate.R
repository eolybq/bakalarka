rm(list = ls())

library(tseries)
library(forecast)
library(tidyverse)
library(urca)
library(vars)

load("data/cdata.RData")

ts_data <- tibble(
  "datum" = format(seq(as.Date("2005-1-01"), as.Date("2023-12-01"), "month"), "%Y-%m"),
  "aktiva" = window(asset_ts, start = c(2005, 1), end = c(2023, 12)),
  "forward_guidance_uvolneni" = window(fg_down_ts, start = c(2005, 1), end = c(2023, 12)),
  "forward_guidance_zprisneni" = window(fg_up_ts, start = c(2005, 1), end = c(2023, 12)),
  "nezamestnanost" = window(unemp_ts, start = c(2005, 1), end = c(2023, 12)),
  "urok" = window(ir_ts, start = c(2005, 1), end = c(2023, 12)),
  "inflace" = window(cpi_ts, start = c(2005, 1), end = c(2023, 12)),
  "oce_p" = window(ie_p_ts, start = c(2005, 1), end = c(2023, 12)),
  "oce_h" = window(ie_h_ts, start = c(2005, 1), end = c(2023, 12))
)


# předpoklady

# # stacionarni
# adf.test(data$forward_guidance_uvolneni)
# adf.test(data$inflace)
# # nestacionarni
# adf.test(data$oce_h)
# adf.test(data$oce_p)
# adf.test(data$forward_guidance_zprisneni)
# adf.test(data$urok)
# adf.test(data$aktiva)
# adf.test(data$nezamestnanost)


# NOTE: Funkce na stacionaritu a kointegraci z manahra_pred
# Stacionarita a tabulka p hodnot testu a doporucenych diferenci
check_stationarity_summary <- function(data) {
  results <- tibble(
    Variable = character(),
    KPSS_p_value = numeric(),
    ADF_p_value = numeric(),
    PP_p_value = numeric(),
    Recommended_Diffs = integer(),
  )

  for (var in names(data)) {
    var_data <- data[[var]]
    kpss_test <- kpss.test(var_data, null = "Level")
    adf_test <- adf.test(var_data, alternative = "stationary")
    pp_test <- pp.test(var_data, alternative = "stationary")
    ndiffs_val <- ndiffs(var_data)

    results <- rbind(results, tibble(
      Variable = var,
      KPSS_p_value = kpss_test$p.value,
      ADF_p_value = adf_test$p.value,
      PP_p_value = pp_test$p.value,
      Recommended_Diffs = ndiffs_val,
    ))
  }

  return(results)
}

variables <- ts_data[, c("aktiva", "forward_guidance_uvolneni", "forward_guidance_zprisneni", "oce_h", "oce_p", "inflace", "urok", "nezamestnanost")]
check_stationarity_summary(variables)

# Engle-Granger cointegration test

# Fit the regression model
coint_model <- lm(aktiva ~ forward_guidance_uvolneni + forward_guidance_zprisneni + oce_h + oce_p + inflace + urok + nezamestnanost, data = ts_data)

# Get the residuals
coint_residuals <- residuals(coint_model)

# Perform the ADF test on residuals
adf.test(coint_residuals, alternative = "stationary")








# vyber zpozdeni
lag_optimal <- VARselect(ts_data[, -1], lag.max = 10, type = "both")
lag_optimal

# model
var_model_prof <- VAR(ts_data[, -c(1, 3, 4, ncol(ts_data))], p = 10, exogen = tibble(ts_data$forward_guidance_uvolneni, ts_data$forward_guidance_zprisneni)) # TODO: type: Type of deterministic regressors to include.
summary(var_model_prof)

# diagnostika modelu
residua <- residuals(var_model)
adf.test(residua) # stacionarity residui
serial.test(var_model) # autokorelace residui
acf(residuals)
pacf(residuals)
