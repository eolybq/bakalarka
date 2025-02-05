rm(list = ls())

library(tseries)
library(forecast)
library(tidyverse)
library(urca)
library(vars)


load("data/tibble_data.RData")


# předpoklady
# TODO: dodělat i klasické předpoklady pro rezidua

# NOTE: Funkce na stacionaritu a kointegraci z manahra_pred
# Stacionarita a tabulka p hodnot testu a pocet diferenci k dosahnuti stacionarity
check_stationarity_summary <- function(data) {
  results <- tibble(
    Variable = character(),
    KPSS_p_value = numeric(),
    ADF_p_value = numeric(),
    PP_p_value = numeric(),
    Integration_Order = integer(),
  )

  for (var in names(data)) {
    var_data <- data[[var]]
    kpss_test <- kpss.test(var_data, null = "Level")
    adf_test <- adf.test(var_data, alternative = "stationary")
    pp_test <- pp.test(var_data, alternative = "stationary")
    ndiffs_val <- ndiffs(var_data, max.d = 10, alpha = 0.05, type = "level")

    results <- rbind(results, tibble(
      Variable = var,
      KPSS_p_value = kpss_test$p.value,
      ADF_p_value = adf_test$p.value,
      PP_p_value = pp_test$p.value,
      Integration_Order = ndiffs_val,
    ))
  }

  return(results)
}

variables <- drop_na(tibble_data[, c("aktiva", "forward_guidance_uvolneni", "forward_guidance_zprisneni", "oce_h", "oce_p", "inflace", "urok", "nezamestnanost")])
check_stationarity_summary(variables)

tibble_data <- tibble_data |>
  mutate(
    aktiva = log(aktiva),

)



# Johansen cointegration test








# vyber zpozdeni
lag_optimal <- VARselect(tibble_data[, -1], lag.max = 10, type = "both")
lag_optimal

# model
var_model_prof <- VAR(tibble_data[, -c(1, 3, 4, ncol(tibble_data))], p = 10, exogen = tibble(tibble_data$forward_guidance_uvolneni, tibble_data$forward_guidance_zprisneni)) # TODO: type: Type of deterministic regressors to include.
summary(var_model_prof)

# diagnostika modelu
residua <- residuals(var_model)
adf.test(residua) # stacionarity residui
serial.test(var_model) # autokorelace residui
acf(residuals)
pacf(residuals)
