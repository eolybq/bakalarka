library(readr)
library(readxl)
library(tidyverse)
library(tseries)
library(urca)
library(vars)

# načtení dat
hcpi <- read_excel("data/cz/hcpi.xlsx", n_max = 297, skip = 6)
unemp <- read_excel("data/cz/unemp.xlsx", range = "A9:HU101")
b_sheet <- read_delim("data/cz/rozvaha_cnb.csv", delim = ";")
ir <- read_delim("data/cz/repo_prumer_m.csv", delim = ";")
fg_uncomplete <- read_excel("data/cz/fg.xlsx")
ie_p <- read_excel("data/cz/CZ_p_m.xlsx")
ie_h <- read_excel("data/cz/CZ_h_m.xlsx")

#doplnění implicitních chybějících hodnot ve FG
fg_uncomplete$time <- format(fg_uncomplete$time, "%Y-%m")
fg_complete <- tibble("time" = seq(as.Date("2005-1-01"), as.Date("2024-12-01"), "month")) %>% 
    mutate(
        time = format(time, "%Y-%m")
    )
fg_final <- fg_complete %>% 
    left_join(fg_uncomplete) %>% 
    mutate(
        fg = replace_na(fg, 0)
    )
#mam zde nekdy dve pozorovani pro jeden cas i IR i DI, jak to resit? prozatim jsem to DI proste vyhodil v tom ts()

#převod na ts objekty
hcpi_ts <- ts(hcpi[[2]], start = c(2000, 1), frequency = 12)
unemp_ts <- ts(unlist(unemp[unemp[[1]] == "Celkem ČR", ][-1]), start = c(2005, 1), frequency = 12)
asset_ts <- ts(b_sheet[[13]][nrow(b_sheet):1], start = c(2002, 9), frequency = 12)
ir_ts <- ts(ir[[2]][nrow(ir):1], start = c(1995, 12), frequency = 12)
fg_ts <- ts(replace_na(unlist(fg_final[fg_final$type != "DI", 2]), 0), start = c(2005, 1), end = c(2024, 11), frequency = 12)
ie_p_ts <- ts(ie_p[[2]], start = c(1999, 5), frequency = 12)
ie_h_ts <- ts(ie_h[[2]], start = c(2001, 1), frequency = 12)

data <- tibble("datum" = format(seq(as.Date("2005-1-01"), as.Date("2023-12-01"), "month"), "%Y-%m"),
               "aktiva" = window(asset_ts, start = c(2005, 1), end = c(2023, 12)),
               "forward_guidance" = window(fg_ts, start = c(2005, 1), end = c(2023, 12)), 
               "nezamestnanost" = window(unemp_ts, start = c(2005, 1), end = c(2023, 12)),
               "urok" = window(ir_ts, start = c(2005, 1), end = c(2023, 12)),
               "inflace" = window(hcpi_ts, start = c(2005, 1), end = c(2023, 12)),
               "oce_p" = window(ie_p_ts, start = c(2005, 1), end = c(2023, 12)),
               "oce_h" = window(ie_h_ts, start = c(2005, 1), end = c(2023, 12))
)
#předpoklady

#stacionarni
adf.test(data$oce_p)
adf.test(data$oce_h)
#nestacionarni
adf.test(data$inflace)
adf.test(data$forward_guidacne)
adf.test(data$urok)
adf.test(data$aktiva)
adf.test(data$nezamestnanost)

#differenciace nestacionarnich

data$inflace <- c(NA, diff(data$inflace))
adf.test(na.remove(data$inflace))

data$aktiva <- c(NA, diff(data$aktiva))
adf.test(na.remove(data$aktiva))

data$urok <- c(NA, diff(data$urok))
adf.test(na.remove(data$urok))

data$nezam <- c(NA, diff(data$nezamestnanost))
adf.test(na.remove(data$nezam))

#vyber zpozdeni
lag_optimal <- VARselect(data[-1, ], lag.max = 10, type = "both")
lag_optimal

#model
data_omitted <- na.omit(data)
var_model <- VAR(data_omitted[, -2], p = 10, exogen = data_omitted$forward_guidance)
summary(var_model)

#diagnostika modelu
adf.test(residuals(var_model))
