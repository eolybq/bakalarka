rm(list = ls())

library(readxl)
library(tidyverse)

# načtení dat
cpi1 <- read_excel("data/rawdata/cpi1.xlsx", n_max = 276, skip = 6)
cpi2 <- read_excel("data/rawdata/cpi2.xlsx", n_max = 82, skip = 6)
unemp <- read_excel("data/rawdata/unemp.xlsx", range = "A9:HU101")
b_sheet <- read_delim("data/rawdata/rozvaha_cnb.csv", delim = ";")
ir <- read_delim("data/rawdata/repo_prumer_m.csv", delim = ";")
fg_uncomplete <- read_excel("data/rawdata/fg.xlsx")
ie_p <- read_excel("data/rawdata/CZ_p_m.xlsx")
ie_h <- read_excel("data/rawdata/CZ_h_m.xlsx")
hdp <- read_delim("data/rawdata/hdp.csv", delim = ";")

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


# spojení cpi1 a cpi2
# FIX: mozna udelat jako cpi_t = cpi_t+1 / cpi_t
cpi_ts <- ts(c(cpi1[[2]], cpi2[[2]]), start = c(1995, 1), frequency = 12)


asset_ts <- ts(b_sheet[[13]][nrow(b_sheet):1], start = c(2002, 9), frequency = 12) # nolint
fg_down_ts <- ts(fg_down[[2]], start = c(2005, 1), end = c(2024, 11), frequency = 12)
fg_up_ts <- ts(fg_up[[2]], start = c(2005, 1), end = c(2024, 11), frequency = 12)
ie_p_ts <- ts(ie_p[[2]], start = c(1999, 5), frequency = 12)
ie_h_ts <- ts(ie_h[[2]], start = c(2001, 1), frequency = 12)
ir_ts <- ts(ir[[2]][nrow(ir):1], start = c(1995, 12), frequency = 12) # nolint: seq_linter.
unemp_ts <- ts(unlist(unemp[unemp[[1]] == "Celkem ČR", ][-1]), start = c(2005, 1), frequency = 12)
hdp_ts <- ts(arrange(hdp, Období)[[2]], start = c(1995, 1), frequency = 4)

# Skalovana aktiva
hdp_month <- hdp_ts |>
    window(start = c(2005, 1), end = c(2023, 4)) |>
    rep(each = 3)
scl_aktiva <- window(asset_ts, start = c(2005, 1), end = c(2023, 12)) / hdp_month



tibble_data <- tibble(
    "datum" = format(seq(as.Date("2005-1-01"), as.Date("2023-12-01"), "month"), "%Y-%m"),
    "aktiva" = window(asset_ts, start = c(2005, 1), end = c(2023, 12)),
    "aktiva_scaled" = scl_aktiva,
    "forward_guidance_uvolneni" = window(fg_down_ts, start = c(2005, 1), end = c(2023, 12)),
    "forward_guidance_zprisneni" = window(fg_up_ts, start = c(2005, 1), end = c(2023, 12)),
    "nezam" = window(unemp_ts, start = c(2005, 1), end = c(2023, 12)),
    "urok" = window(ir_ts, start = c(2005, 1), end = c(2023, 12)),
    "inflace" = window(cpi_ts, start = c(2005, 1), end = c(2023, 12)),
    "oce_p" = window(ie_p_ts, start = c(2005, 1), end = c(2023, 12)),
    "oce_h" = window(ie_h_ts, start = c(2005, 1), end = c(2023, 12))
)

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


save(
    ts_objects,
    file = "data/ts_data.RData"
)

save(
    tibble_data,
    file = "data/tibble_data.RData"
)
