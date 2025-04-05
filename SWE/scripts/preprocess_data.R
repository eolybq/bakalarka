rm(list = ls())

library(readxl)
library(tidyverse)

# načtení dat
cpi <- read_csv("data/rawdata/cpi.csv")
unemp <- read_excel("data/rawdata/unemp.xlsx", sheet = 3, range = "E10:E300")
ppi <- read_csv("data/rawdata/ppi.csv")
b_sheet <- read_delim("data/rawdata/b_sheet.csv", delim = ";")
ir <- read_delim("data/rawdata/ir.csv", delim = ";")
ie_h <- read_excel("data/rawdata/SE_h_m.xlsx")
ie_p <- read_excel("data/rawdata/SE_p_m.xlsx")
bei <- read_delim("data/rawdata/exp_m.csv", delim = ";", locale = locale(decimal_mark = ","))
gdp <- read_csv("data/rawdata/gdp.csv")


# převod na ts objekty
cpi_ts <- ts(cpi[[2]], start = c(1980, 1), end = c(2025, 2), frequency = 12)
unemp_ts <- ts(unemp[[1]], start = c(2001, 1), end = c(2025, 2), frequency = 12)
ppi_ts <- ts(ppi[[3]], start = c(1990, 1), end = c(2025, 2), frequency = 12)
ir_ts <- ts(ir[[5]], start = c(1995, 1), end = c(2025, 2), frequency = 12)
ie_h_ts <- ts(ie_h[[2]], start = c(2001, 12), end = c(2024, 10), frequency = 12)
ie_p_ts <- ts(ie_p[[2]], start = c(2010, 01), end = c(2024, 9), frequency = 12)
gdp_ts <- ts(as.numeric(gdp[[3]][49:length(gdp[[3]])]), start = c(1993, 1), end = c(2024, 4), frequency = 4)


# WEEKLY
# prevod na stocks (posledni hodnota v mesici)
securities_monthly <- b_sheet |>
    arrange(date) |>
    mutate(month = floor_date(as_date(date), "month")) |>
    group_by(month) |>
    filter(date == max(date))

securities_ts <- ts(securities_monthly[[2]], start = c(1999, 9), end = c(2025, 3), frequency = 12)

# Skalovana aktiva
gdp_month <- gdp_ts |>
    window(start = c(1999, 4), end = c(2024, 4)) |>
    rep(each = 3)
scl_securities <- window(securities_ts, start = c(1999, 10), end = c(2024, 12)) / gdp_month


# DAILY
# NOTE: prevod na last obs of month
exp_m_monthl <- bei |> 
    mutate(month = floor_date(as_date(date), "month")) |>
    group_by(month) |>
    filter(date == max(date))
exp_m_ts <- ts(exp_m_monthl[[2]], start = c(2011, 1), end = c(2023, 4), frequency = 12)





tibble_data <- tibble(
    "date" = format(seq(as.Date("2010-01-01"), as.Date("2024-09-01"), "month"), "%Y-%m"),
    "securities" =  window(securities_ts, start = c(2010, 1), end = c(2024, 9)),
    "securities_scl" = window(scl_securities, start = c(2010, 1), end = c(2024, 9)),
    "ppi" = window(ppi_ts, start = c(2010, 1), end = c(2024, 9)),
    "ir" = window(ir_ts, start = c(2010, 1), end = c(2024, 9)),
    "cpi" = window(cpi_ts, start = c(2010, 1), end = c(2024, 9)),
    "exp_h" = window(ie_h_ts, start = c(2010, 1), end = c(2024, 9)),
    "exp_p" = window(ie_p_ts, start = c(2010, 1), end = c(2024, 9)),
)



ts_objects <- list(
    securities_ts = securities_ts,
    securities_scl_ts = scl_securities,
    ppi_ts = ppi_ts,
    ir_ts = ir_ts,
    cpi_ts = cpi_ts,
    exp_h_ts = ie_h_ts,
    exp_p_ts = ie_p_ts,
    exp_m_ts = exp_m_ts,
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

save(
    exp_m_ts,
    file = "data/exp_m_data.RData"
)
