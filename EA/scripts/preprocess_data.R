rm(list = ls())

library(readxl)
library(tidyverse)

# načtení dat
hicp <- read_tsv("data/rawdata/hicp.tsv")
# unemp <- read_csv("data/rawdata/unemp.csv")
ipi <- read_csv("data/rawdata/ipi.csv")
sec <- read_csv("data/rawdata/ds_eurosystem.csv")
b_sheet <- read_csv("data/rawdata/b_eurosystem.csv")
ir <- read_excel("data/rawdata/ir.xlsx", sheet = 2)
fg_uncomplete <- read_excel("data/rawdata/fg.xlsx")
ie_h <- read_excel("data/rawdata/EA_H_M.xlsx")
bund10 <- read_csv("data/rawdata/bund10.csv")
ilb <- read_csv("data/rawdata/ilb.csv")
gdp <- read_csv("data/rawdata/gdp.csv")

# doplnění implicitních chybějících hodnot ve FG a rozdeleni na uvoneni a zprisneni
fg_uncomplete$time <- format(fg_uncomplete$time, "%Y-%m")
fg_complete <- tibble("time" = seq(as.Date("1999-1-01"), as.Date("2024-12-01"), "month")) |>
    mutate(
        time = format(time, "%Y-%m")
    )
fg_down <- fg_complete |>
    left_join(fg_uncomplete[fg_uncomplete$dir == "D", ]) |>
    mutate(
        fg = replace_na(fg, 0)
    )

# převod na ts objekty

hicp_clean <- hicp |> 
    mutate(across(-1, as.character)) |>
    mutate(across(-1, ~na_if(.x, ":"))) |>
    mutate(across(-1, as.numeric)) |>
    pivot_longer(cols = -1, names_to = "date", values_to = "value") |>
    drop_na()
    
hicp_ts <- ts(hicp_clean[["value"]], start = c(1999, 12), end = c(2025, 2), frequency = 12)
# unemp_ts <- ts(unemp[[3]], start = c(2000, 1), end = c(2025, 1), frequency = 12)
ipi_ts <- ts(ipi[[3]], start = c(1991, 1), end = c(2025, 1), frequency = 12)
sec_ts <- ts(sec[[3]], start = c(1997, 9), end = c(2025, 2), frequency = 12)
b_sheet_ts <- ts(b_sheet[[3]], start = c(1997, 9), end = c(2025, 2), frequency = 12)
fg_down_ts <- ts(fg_down[[2]], start = c(1999, 1), end = c(2024, 12), frequency = 12)
ie_h_ts <- ts(ie_h[[2]], start = c(1998, 1), end = c(2024, 9), frequency = 12)
gdp_ts <- ts(gdp[[3]], start = c(1995, 1), end = c(2024, 4), frequency = 4)


# DAILY
# NOTE: oboji prevod na last obs of month
ir_monthly <- ir |> 
    mutate(month = floor_date(as_date(DATE), "month")) |>
    group_by(month) |>
    filter(DATE == max(DATE))
ir_ts <- ts(ir_monthly[[3]], start = c(1999, 1), end = c(2025, 3), frequency = 12)
    

first_obs_bund10 <- min(bund10[[2]])
ilb_date_fix <- ilb |> 
    filter(`Yield (x)` >= first_obs_bund10)
exp_m_data <- bind_cols(
    "date" = bund10[[2]],
    "ilb" = ilb_date_fix[[3]],
    "bund10" = bund10[[3]]
)

bei <- exp_m_data |>
    mutate(month = floor_date(as_date(date), "month")) |>
    group_by(month) |>
    filter(date == max(date)) |>
    summarise(monthly_BEI = first(bund10 - ilb))

exp_m_ts <- ts(bei[[2]], start = c(2015, 7), end = c(2025, 3), frequency = 12)


# Skalovana aktiva
gdp_month <- gdp_ts |>
    window(start = c(1997, 4)) |>
    rep(each = 3)
scl_sec <- window(sec_ts, start = c(1997, 10), end = c(2024, 12)) / gdp_month
scl_b_sheet <- window(b_sheet_ts, start = c(1997, 10), end = c(2024, 12)) / (gdp_month * 4)



tibble_data <- tibble(
    "date" = format(seq(as.Date("1999-12-01"), as.Date("2024-09-01"), "month"), "%Y-%m"),
    "sec" = window(scl_sec, start = c(1999, 12), end = c(2024, 9)),
    "fg_u" = window(fg_down_ts, start = c(1999, 12), end = c(2024, 9)),
    "ipi" = window(ipi_ts, start = c(1999, 12), end = c(2024, 9)),
    "ir" = window(ir_ts, start = c(1999, 12), end = c(2024, 9)),
    "hicp" = window(hicp_ts, start = c(1999, 12), end = c(2024, 9)),
    "exp_h" = window(ie_h_ts, start = c(1999, 12), end = c(2024, 9)),
)
    


ts_objects <- list(
    assets_ts = b_sheet_ts,
    assets_scl_ts = scl_b_sheet,
    sec_ts = scl_sec,
    sec_abs = sec_ts,
    fg_u_ts = fg_down_ts,
    ipi_ts = ipi_ts,
    ir_ts = ir_ts,
    hicp_ts = hicp_ts,
    exp_h_ts = ie_h_ts,
    exp_m_ts = exp_m_ts,
    # unemp_ts = unemp_ts
)


save(
    ts_objects,
    file = "data/ts_data.RData"
)

save(
    tibble_data,
    file = "data/tibble_data.RData"
)


save(
    exp_m_ts,
    file = "data/exp_m_data.RData"
)

