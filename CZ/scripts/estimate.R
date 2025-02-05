rm(list = ls())

library(tseries)
library(forecast)
library(tidyverse)
library(urca)
library(vars)


load("data/tibble_data.RData")


# GRAF vsechny promenne ============
load("data/ts_data.RData")

# Create a list of all ts objects loaded from ts_data.RData
ts_objects <- list(
  cpi_ts = cpi_ts,
  asset_ts = asset_ts,
  fg_down_ts = fg_down_ts,
  fg_up_ts = fg_up_ts,
  ie_p_ts = ie_p_ts,
  ie_h_ts = ie_h_ts,
  ir_ts = ir_ts,
  unemp_ts = unemp_ts
)

ts_to_df <- function(ts_obj) {
  start_year <- start(ts_obj)[1]
  start_period <- start(ts_obj)[2]
  freq <- frequency(ts_obj)
  n <- length(ts_obj)

  start_date <- as.Date(paste0(start_year, "-", sprintf("%02d", start_period), "-01"))
  time_seq <- seq(from = start_date, by = "month", length.out = n)

  tibble(time = time_seq, value = as.numeric(ts_obj))
}


# Loop over each time series, convert to a tibble, and generate a ggplot graph
for (ts_name in names(ts_objects)) {
  df <- ts_to_df(ts_objects[[ts_name]])
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line(color = "steelblue", size = 1) +
    labs(
      title = paste("Time Series Plot:", ts_name),
      x = "Time",
      y = "Value"
    ) +
    theme_minimal()

  print(p)
}


# ACF - stacionarita | PACF - sezonnost ===========
acf_pacf <- function(data, maxlag) {
  if (class(data) %in% c("numeric", "ts")) {


    acf(as.numeric(data), lag.max = maxlag, main = paste("ACF for", deparse(substitute(data))))
    pacf(as.numeric(data), lag.max = maxlag, main = paste("PACF for", deparse(substitute(data))))

  } else if (class(data) %in% c("tibble", "data.frame")) {
    imap(data, function(x, y) {
      # par(mfrow = c(1, 2))

      acf(as.numeric(x), lag.max = maxlag, main = paste("ACF for", y))
      pacf(as.numeric(x), lag.max = maxlag, main = paste("PACF for", y))

    })
  }
}

# FIX: unemp_ts vypadá sezonne - resit
acf_pacf(ts_objects[["unemp_ts"]], 37)


sdiffed_unemp <- diff(ts_objects[["unemp_ts"]], lag = 12)
autoplot(sdiffed_unemp)
acf_pacf(sdiffed_unemp, 37)

s2diffed_unemp <- diff(sdiffed_unemp, lag = 13)
autoplot(s2diffed_unemp)
acf_pacf(s2diffed_unemp, 37)

check_stationarity_summary(ts_objects["unemp_ts"])


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
