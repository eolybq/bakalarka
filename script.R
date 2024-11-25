library(readr)
library(readxl)
library(tidyverse)
library(tseries)
library(vars)

# načtení dat
hcpi <- read_excel("data/cz/hcpi.xlsx", n_max = 297, skip = 6)
unemp <- read_excel("data/cz/unemp.xlsx", n_max = 141, skip = 6)
b_sheet <- read_delim("data/cz/rozvaha_cnb.csv", delim = ";")
ir <- read_delim("data/cz/repo_prumer_m.csv", delim = ";")
fg_uncomplete <- read_excel("data/cz/fg.xlsx")
ie_p <- read_excel("data/cz/CZ_p_m.xlsx")
ie_h <- read_excel("data/cz/CZ_h_m.xlsx")

#doplnění implicitních chybějících hodnot ve FG
fg_uncomplete$time <- format(fg_uncomplete$time, "%Y-%m")
fg_complete <- tibble("time" = seq(as.Date("2012-11-01"), as.Date("2024-12-01"), "month")) %>% 
    mutate(
        time = format(time, "%Y-%m")
    )
fg_final <- fg_complete %>% 
    left_join(fg_uncomplete) %>% 
    mutate(
        fg = replace_na(fg, 0)
    )

#převod na ts objekty
hcpi_ts <- ts(hcpi[[2]], start = c(2000, 1), frequency = 12)
unemp_ts <- ts(unemp[[3]][nrow(unemp):1], start = c(2013, 1), frequency = 12)
asset_ts <- ts(b_sheet[[13]][nrow(b_sheet):1], start = c(2002, 9), frequency = 12)
ir_ts <- ts(ir[[2]][nrow(ir):1], start = c(1995, 12), frequency = 12)
fg_ts <- ts(fg_final[[2]], start = c(2012, 11), frequency = 12)
ie_p_ts <- ts(ie_p[[2]], start = c(1999, 5), frequency = 12)
ie_h_ts <- ts(ie_h[[2]], start = c(2001, 1), frequency = 12)

 