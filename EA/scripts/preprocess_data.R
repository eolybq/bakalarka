rm(list = ls())

library(readxl)
library(tidyverse)

# načtení dat
hicp <- read_tsv("data/rawdata/hicp.tsv")
unemp <- read_csv("data/rawdata/unemp.csv")
ipi <- read_csv("data/rawdata/ipi.csv")
securities <- read_csv("data/rawdata/ds_eurosystem.csv")
bs_sheet <- read_csv("data/rawdata/bs_eurosystem.csv")
ir <- read_excel("data/rawdata/ir.xlsx", sheet = 2)
# fg_uncomplete <- read_excel("data/rawdata/fg.xlsx")
ie_h <- read_excel("data/rawdata/EA_H_M.xlsx")
bund10 <- read_csv("data/rawdata/bund10.csv")
ilb <- read_csv("data/rawdata/ilb.csv")
gdp <- read_csv("data/rawdata/gdp.csv")

# doplnění implicitních chybějících hodnot ve FG a rozdeleni na uvoneni a zprisneni
# fg_uncomplete$time <- format(fg_uncomplete$time, "%Y-%m")
# fg_complete <- tibble("time" = seq(as.Date("2005-1-01"), as.Date("2024-12-01"), "month")) |>
#     mutate(
#         time = format(time, "%Y-%m")
#     )
# fg_down <- fg_complete |>
#     left_join(fg_uncomplete[fg_uncomplete$type != "DI" & fg_uncomplete$dir == "D", ]) |>
#     mutate(
#         fg = replace_na(fg, 0)
#     )
# fg_up <- fg_complete |>
#     left_join(fg_uncomplete[fg_uncomplete$type != "DI" & fg_uncomplete$dir == "U", ]) |>
#     mutate(
#         fg = replace_na(fg, 0)
#     )

# převod na ts objekty

hicp_clean <- hicp |> 
    mutate(across(-1, as.character)) |>
    mutate(across(-1, ~na_if(.x, ":"))) |>
    mutate(across(-1, as.numeric)) |>
    pivot_longer(cols = -1, names_to = "date", values_to = "value")
    
hicp_ts <- ts(hicp_clean[["value"]], start = c(1996, 1), end = c(2025, 2), frequency = 12)
unemp_ts <- ts(unemp[[3]], start = c(2000, 1), end = c(2025, 1), frequency = 12)
ipi_ts <- ts(ipi[[3]], start = c(1991, 1), end = c(2025, 1), frequency = 12)
securities_ts <- ts(securities[[3]], start = c(1997, 9), end = c(2025, 2), frequency = 12)
bs_sheet_ts <- ts(bs_sheet[[3]], start = c(1997, 9), end = c(2025, 2), frequency = 12)
# fg_down_ts <- ts(fg_down[[2]], start = c(2005, 1), end = c(2024, 11), frequency = 12)
# fg_up_ts <- ts(fg_up[[2]], start = c(2005, 1), end = c(2024, 11), frequency = 12)
ie_h_ts <- ts(ie_h[[2]], start = c(1997, 1), end = c(2023, 9), frequency = 12)

# DAILY
# FIX: udelat konec mesice
ir_ts <- ts(ir[[3]], start = c(1999, 1, 1), end = c(2025, 3, 29), frequency = 365)
# FIX: JE JEN PRACOVNI DNY - spis vzit steje prumer asi nebo tereticky konec ale psis prumer a pak BEI
bund10_ts <- ts(bund10[[3]], start = c(2015, 7, 15), end = c(2025, 3, 28), frequency = 365)
ilb_ts <- ts(ilb[[3]], start = c(2015, 3, 10), end = c(2025, 3, 28), frequency = 365)

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
    "ipp" = window(ipp_ts, start = c(2005, 1), end = c(2023, 12)),
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
    unemp_ts = unemp_ts,
    ipp_ts = ipp_ts
)


save(
    ts_objects,
    file = "data/ts_data.RData"
)

save(
    tibble_data,
    file = "data/tibble_data.RData"
)

