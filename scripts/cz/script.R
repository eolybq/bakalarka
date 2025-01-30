rm(list = ls())

library(readr)
library(readxl)
library(tidyverse)
library(tseries)
library(urca)
library(vars)

# načtení dat
cpi1 <- read_excel("data/cz/cpi1.xlsx", n_max = 276, skip = 6)
cpi2 <- read_excel("data/cz/cpi2.xlsx", n_max = 82, skip = 6)
unemp <- read_excel("data/cz/unemp.xlsx", range = "A9:HU101")
b_sheet <- read_delim("data/cz/rozvaha_cnb.csv", delim = ";")
ir <- read_delim("data/cz/repo_prumer_m.csv", delim = ";")
fg_uncomplete <- read_excel("data/cz/fg.xlsx")
ie_p <- read_excel("data/cz/CZ_p_m.xlsx")
ie_h <- read_excel("data/cz/CZ_h_m.xlsx")

# doplnění implicitních chybějících hodnot ve FG a rozdeleni na uvoneni a zprisneni
fg_uncomplete$time <- format(fg_uncomplete$time, "%Y-%m")
fg_complete <- tibble("time" = seq(as.Date("2005-1-01"), as.Date("2024-12-01"), "month")) |>
  mutate(
    time = format(time, "%Y-%m")
  )
fg_down <- fg_complete |>
  left_join(fg_uncomplete[fg_uncomplete$type != "DI" & fg_uncomplete$dir == "D", ]) |>
  mutate(
    fg = replace_na(fg, 0)
  )
fg_up <- fg_complete |>
  left_join(fg_uncomplete[fg_uncomplete$type != "DI" & fg_uncomplete$dir == "U", ]) |>
  mutate(
    fg = replace_na(fg, 0)
  )


# převod na ts objekty

# spojení cpi1 a cpi2 + prevod na mezimesicni miru
cpi <- ts(c(cpi1[[2]], cpi2[[2]]), start = c(1995, 1), frequency = 12)
cpi_ts <- diff(log(cpi)) * 100

asset_ts <- ts(b_sheet[[13]][nrow(b_sheet):1], start = c(2002, 9), frequency = 12) # nolint
fg_down_ts <- ts(fg_down[[2]], start = c(2005, 1), end = c(2024, 11), frequency = 12)
fg_up_ts <- ts(fg_up[[2]], start = c(2005, 1), end = c(2024, 11), frequency = 12)
ie_p_ts <- ts(ie_p[[2]], start = c(1999, 5), frequency = 12)
ie_h_ts <- ts(ie_h[[2]], start = c(2001, 1), frequency = 12)
ir_ts <- ts(ir[[2]][nrow(ir):1], start = c(1995, 12), frequency = 12) # nolint: seq_linter.
unemp_ts <- ts(unlist(unemp[unemp[[1]] == "Celkem ČR", ][-1]), start = c(2005, 1), frequency = 12)

data <- tibble(
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

rm(list = ls()[ls() != "data"])

# předpoklady

# stacionarni
adf.test(data$forward_guidance_uvolneni)
adf.test(data$inflace)
# nestacionarni
adf.test(data$oce_h)
adf.test(data$oce_p)
adf.test(data$forward_guidance_zprisneni)
adf.test(data$urok)
adf.test(data$aktiva)
adf.test(data$nezamestnanost)

# vyber zpozdeni
lag_optimal <- VARselect(data[, -1], lag.max = 10, type = "both")
lag_optimal

# model
var_model_prof <- VAR(data[, -c(1, 3, 4, ncol(data))], p = 10, exogen = tibble(data$forward_guidance_uvolneni, data$forward_guidance_zprisneni)) # TODO: type: Type of deterministic regressors to include.
summary(var_model_prof)

# diagnostika modelu
residua <- residuals(var_model)
adf.test(residua) # stacionarity residui
serial.test(var_model) # autokorelace residui
acf(residuals)
pacf(residuals)

