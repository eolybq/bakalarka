rm(list = ls())

library(tseries)
library(car)
library(forecast)
library(tidyverse)
library(urca)
library(vars)
library(seasonal)
library(gridExtra)
library(grid)


load("data/tibble_data.RData")
load("data/exp_m_data.RData")


# ========================================================
# Data
# ========================================================

# Stacionarita a tabulka p hodnot testu a pocet diferenci k dosahnuti stacionarity
check_stationarity <- function(data) {
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

# delsi vzorek (bez exp_m a exp_p)
variables <- tibble_data |>
    dplyr::select(
        -date,
    ) |>
    drop_na()
without_exp_m <- check_stationarity(variables)
# exp_m a exp_p
with_exp_m <- check_stationarity(tibble(exp_m = exp_m_ts))

rbind(without_exp_m, with_exp_m)



# ACF - stacionarita | PACF - sezonnost ===========
acf_pacf <- function(data, maxlag) {
    if (class(data)[1] %in% c("numeric", "ts")) {
        acf(as.numeric(data), lag.max = maxlag, main = paste("ACF for", deparse(substitute(data))))
        pacf(as.numeric(data), lag.max = maxlag, main = paste("PACF for", deparse(substitute(data))))
    } else if (class(data)[1] %in% c("tbl_df", "tbl", "data.frame")) {
        imap(data[, -1], function(x, y) {
            # par(mfrow = c(1, 2))
            
            acf(as.numeric(x), lag.max = maxlag, main = paste("ACF for", y))
            pacf(as.numeric(x), lag.max = maxlag, main = paste("PACF for", y))
        })
    }
}

acf_pacf(tibble_data, 40)
exp_m_ts |>
    acf_pacf(40)


# NOTE: konstanta pricitana k sec - vyhnuti se problemu s log()
c <- 0.00001

trans_tdata <- tibble_data |>
    mutate(
        # sec = final(seas(sec)), # NOTE: nefunguje, asi hodne 0 v datech
        # ir = final(seas(ir)), # NOTE: nefunguje ale asi neni sezonost takze idk
        cpi = final(seas(cpi)),
        ipi = final(seas(ipi)),
        exp_h = final(seas(exp_h)),
        exp_p = final(seas(exp_p))
    ) |>
    mutate(
        sec = c(NA, NA, diff(diff(log(sec + c)) * 100)),
        ipi = c(NA, NA, diff(diff(log(ipi)) * 100)),
        cpi = c(NA, NA, diff(diff(log(cpi)) * 100)),
        ir = c(NA, NA, diff(diff(ir))),
        exp_h = c(NA, diff(exp_h)),
        exp_p = c(NA, diff(exp_p))
    ) |>
    drop_na()
# Omezení na před Covid cpi
# filter(date < "2022-01")

# NOTE: transformace exp_m
exp_m_seas = final(seas(exp_m_ts))
exp_m = diff(exp_m_seas)


# delsi vzorek (bez exp_m a exp_p)
variables <- trans_tdata |>
    dplyr::select(
        -date,
    ) |>
    drop_na()
without_exp_m <- check_stationarity(variables)
# exp_m a exp_p
with_exp_m <- check_stationarity(tibble(exp_m = exp_m))

rbind(without_exp_m, with_exp_m)

acf_pacf(trans_tdata, 40)
exp_m |>
    acf_pacf(40)

# Graf všech proměnných v jednom grafu
trans_tdata |>
    dplyr::select(
        -sec,
    ) |>
    pivot_longer(cols = -date) |>
    ggplot(aes(x = date, y = value, color = name, group = name)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Plot všech proměnných v jednom grafu")


# ========================================================
# Model
# ========================================================

# DOMACNOSTI ==============
trans_tdata_h <- trans_tdata |>
    dplyr::select(
        -date,
        -exp_p,
    )

# vyber zpozdeni
lag_optimal_h <- trans_tdata_h |>
    VARselect(
        lag.max = 20,
        type = "const",
    )
lag_optimal_h

# odhad
var_model_h <- trans_tdata_h |>
    vars::VAR(
        p = 2,
        type = "const",
    )


summary(var_model_h)


# prepoklady

# diagnostika modelu
residuals_h <- residuals(var_model_h)

as_tibble(residuals_h) |>
    add_column(new_col = NA, .before = 1) |>
    acf_pacf(20)

serial.test(var_model_h) # Autokorelace
arch.test(var_model_h) # Heteroskedasticita
normality.test(var_model_h)

# KPSS test
kpss_results_h <- apply(residuals_h, 2, kpss.test)
# ADF test
adf_results_h <- apply(residuals_h, 2, adf.test)
# PP test
pp_results_h <- apply(residuals_h, 2, pp.test)
# Print the results
print(kpss_results_h)
print(adf_results_h)
print(pp_results_h)

# multikolinearita
# map(var_model_h$varresult, ~vif(.))




akt_exp_h_irf <- irf(
    var_model_h,
    impulse = "sec",
    response = "exp_h",
    n.ahead = 20,
    runs = 1000
)

horizons_h <- 0:(nrow(akt_exp_h_irf$irf$sec) - 1)

irf_df_h <- data.frame(
    horizon = horizons_h,
    response = akt_exp_h_irf$irf$sec[ , "exp_h"],
    lower = akt_exp_h_irf$Lower$sec[ , "exp_h"],
    upper = akt_exp_h_irf$Upper$sec[ , "exp_h"]
)
impulse_aktiva_h <- ggplot(irf_df_h, aes(x = horizon, y = response)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(
        x = "Časový horizont",
        y = NULL
    )




# Granger causality
causality(var_model_h, cause = "sec")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp_h <- fevd(var_model_h, n.ahead = 20)
plot(v_decomp_h)





# PROFESIONALOVE ==============

# Pripojeni exp_p - pridani NA

trans_tdata_p <- trans_tdata |>
    dplyr::select(
        -date,
        -exp_h,
    ) |>
    drop_na()

# vyber zpozdeni
lag_optimal_p <- trans_tdata_p |>
    VARselect(
        lag.max = 20,
        type = "const",
    )
lag_optimal_p

# odhad
var_model_p <- trans_tdata_p |>
    vars::VAR(
        p = 2,
        type = "const",
    )


summary(var_model_p)


# prepoklady

# diagnostika modelu
residuals_p <- residuals(var_model_p)

as_tibble(residuals_p) |>
    add_column(new_col = NA, .before = 1) |>
    acf_pacf(20)

serial.test(var_model_p) # Autokorelace
arch.test(var_model_p) # Heteroskedasticita
normality.test(var_model_p)

# KPSS test
kpss_results_p <- apply(residuals_p, 2, kpss.test)
# ADF test
adf_results_p <- apply(residuals_p, 2, adf.test)
# PP test
pp_results_p <- apply(residuals_p, 2, pp.test)
# Print the results
print(kpss_results_p)
print(adf_results_p)
print(pp_results_p)

# multikolinearita
# map(var_model_p$varresult, vif)




akt_exp_p_irf <- irf(
    var_model_p,
    impulse = "sec",
    response = "exp_p",
    n.ahead = 20,
    runs = 1000
)

horizons_p <- 0:(nrow(akt_exp_p_irf$irf$sec) - 1)

irf_df_p <- data.frame(
    horizon = horizons_p,
    response = akt_exp_p_irf$irf$sec[ , "exp_p"],
    lower = akt_exp_p_irf$Lower$sec[ , "exp_p"],
    upper = akt_exp_p_irf$Upper$sec[ , "exp_p"]
)
impulse_aktiva_p <- ggplot(irf_df_p, aes(x = horizon, y = response)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(
        x = "Časový horizont",
        y = NULL
    )




# Granger causality
causality(var_model_p, cause = "sec")

# FIX: zase hazi chybu asi - exogenni
# causality(res_var_model_p, cause = "fg_z")
# causality(res_var_model_p, cause = "fg_u")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp_p <- fevd(var_model_p, n.ahead = 20)
plot(v_decomp_p)






# TRH ==============
# Pripojeni exp_m - pridani NA
exp_m_date_tibble <- tibble(exp_m = exp_m)
exp_m_date_tibble$date <- format(seq(as.Date("2011-2-01"), as.Date("2023-4-01"), "month"), "%Y-%m")

trans_tdata_m <- trans_tdata |>
    left_join(exp_m_date_tibble) |>
    dplyr::select(
        -date,
        -exp_h,
        -exp_p
    ) |>
    drop_na()


# vyber zpozdeni
lag_optimal_m <- trans_tdata_m |>
    VARselect(
        lag.max = 20,
        type = "const",
    )
lag_optimal_m

# odhad
var_model_m <- trans_tdata_m |>
    vars::VAR(
        p = 2,
        type = "const",
    )


summary(var_model_m)


# prepoklady

# diagnostika modelu
residuals_m <- residuals(var_model_m)

as_tibble(residuals_m) |>
    add_column(new_col = NA, .before = 1) |>
    acf_pacf(20)

serial.test(var_model_m) # Autokorelace
arch.test(var_model_m) # Heteroskedasticita
normality.test(var_model_m)

# KPSS test
kpss_results_m <- apply(residuals_m, 2, kpss.test)
# ADF test
adf_results_m <- apply(residuals_m, 2, adf.test)
# PP test
pp_results_m <- apply(residuals_m, 2, pp.test)
# Print the results
print(kpss_results_m)
print(adf_results_m)
print(pp_results_m)

# multikolinearita
# map(var_model_m$varresult, vif)





akt_exp_m_irf <- irf(
    var_model_m,
    impulse = "sec",
    response = "exp_m",
    n.ahead = 20,
    runs = 1000
)
horizons_m <- 0:(nrow(akt_exp_m_irf$irf$sec) - 1)

irf_df_m <- data.frame(
    horizon = horizons_m,
    response = akt_exp_m_irf$irf$sec[ , "exp_m"],
    lower = akt_exp_m_irf$Lower$sec[ , "exp_m"],
    upper = akt_exp_m_irf$Upper$sec[ , "exp_m"]
)
impulse_aktiva_m <- ggplot(irf_df_m, aes(x = horizon, y = response)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(
        x = "Časový horizont",
        y = NULL
    )





# Granger causality
causality(var_model_m, cause = "sec")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp_m <- fevd(var_model_m, n.ahead = 20)
plot(v_decomp_m)



# NOTE: graf vsech irf
col1_label <- textGrob("Domácnosti", gp = gpar(fontsize = 14, fontface = "bold"))
col2_label <- textGrob("Profesionálové", gp = gpar(fontsize = 14, fontface = "bold"))
col3_label <- textGrob("Trh", gp = gpar(fontsize = 14, fontface = "bold"))

row1_label <- textGrob("Cenné papíry", rot = 90, gp = gpar(fontsize = 14, fontface = "bold"))

grid.arrange(
    # Horní řádek s popisky sloupců – první buňka prázdná
    arrangeGrob(
        textGrob(""), col1_label, col2_label, col3_label,
        ncol = 4,
        widths = c(1, 3, 3, 3)
    ),
    # První řádek grafů s řádkovým popiskem vlevo
    arrangeGrob(
        row1_label, impulse_aktiva_h, impulse_aktiva_p, impulse_aktiva_m,
        ncol = 4,
        widths = c(0.3, 3, 3, 3)
    ),
    nrow = 2,
    heights = c(0.3, 3)
)


