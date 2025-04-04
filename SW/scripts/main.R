rm(list = ls())

library(tseries)
library(car)
library(forecast)
library(tidyverse)
library(urca)
library(vars)
library(seasonal)


load("data/tibble_data.RData")
load("data/exp_p_data.RData")
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
# exp_m
with_exp_m <- check_stationarity(tibble(exp_m = exp_m_ts))
with_exp_p <- check_stationarity(tibble(exp_p = exp_p_ts))

rbind(without_exp_m, with_exp_p, with_exp_m)



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
exp_p_ts |>
    acf_pacf(40)
exp_m_ts |>
    acf_pacf(40)


trans_tdata <- tibble_data |>
    mutate(
        securities = final(seas(securities)),
        securities_scl = final(seas(securities_scl)),
        # ir = final(seas(ir)), # NOTE: nefunguje ale asi neni sezonost takze idk
        cpi = final(seas(cpi)),
        exp_h = final(seas(exp_h))
    ) |>
    mutate(
        securities = c(NA, NA, diff(diff(log(securities)) * 100)),
        securities_scl = c(NA, NA, diff(diff(log(securities_scl)) * 100)),
        ppi = c(NA, NA, diff(diff(log(ppi)) * 100)),
        cpi = c(NA, NA, diff(diff(log(cpi)) * 100)),
        ir = c(NA, diff(ir)),
        exp_h = c(NA, diff(exp_h)),
    ) |>
    drop_na()
# Omezení na před Covid cpi
# filter(date < "2022-01")

# NOTE: transformace exp_m
exp_m_seas = final(seas(exp_m_ts))
exp_m = diff(exp_m_seas)


# delsi vzorek (bez exp_m)
variables <- trans_tdata |>
    dplyr::select(
        -date,
    ) |>
    drop_na()
without_exp_m <- check_stationarity(variables)
# exp_m
with_exp_m <- check_stationarity(tibble(exp_m = exp_m))

rbind(without_exp_m, with_exp_m)
acf_pacf(trans_tdata, 40)
exp_m |>
    acf_pacf(40)

# Graf všech proměnných v jednom grafu
trans_tdata |>
    dplyr::select(
        -securities,
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
        
        
        # NOTE swap securities
        -securities,
        # -securities_scl
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
        p = 3,
        type = "const",
    )


var_count_h <- 5
max_var_lag_h <- 3


# NOTE: restrikce: FG se objevuje pouze v rovnici pro exp
res_matrix_h <- rbind(
    # eq securities_scl
    c(rep(1, var_count_h * max_var_lag_h + 1), 0),
    # eq ppi
    c(rep(1, var_count_h * max_var_lag_h + 1), 0),
    # eq ir
    c(rep(1, var_count_h * max_var_lag_h + 1), 0),
    # eq cpi
    c(rep(1, var_count_h * max_var_lag_h + 1), 0),
    # eq oce
    c(rep(1, var_count_h * max_var_lag_h + 1), 1)
)

res_var_model_h <- restrict(var_model_h, method = "manual", resmat = res_matrix_h)


summary(res_var_model_h)


# prepoklady

# diagnostika modelu
residuals_h <- residuals(res_var_model_h)

as_tibble(residuals_h) |>
    add_column(new_col = NA, .before = 1) |>
    acf_pacf(20)

serial.test(res_var_model_h) # Autokorelace
arch.test(res_var_model_h) # Heteroskedasticita
normality.test(res_var_model_h)

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
map(res_var_model_h$varresult, ~vif(.))




akt_exp_h_irf <- irf(
    res_var_model_h,
    
    # NOTE: swap za securities kdyztak
    impulse = "securities_scl",
    response = "exp_h",
    n.ahead = 20,
    ortho = FALSE,
    runs = 1000
)

plot(akt_exp_h_irf)


# Granger causality
# NOTE: swap za fx_res kdyztak
causality(res_var_model_h, cause = "securities_scl")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp_h <- fevd(res_var_model_h, n.ahead = 20)
plot(v_decomp_h)





# TRH ==============
# Pripojeni exp_m - pridani NA
exp_m_date_tibble <- tibble(exp_m = exp_m)
exp_m_date_tibble$date <- format(seq(as.Date("2015-8-01"), as.Date("2025-3-01"), "month"), "%Y-%m")

trans_tdata_m <- trans_tdata |>
    left_join(exp_m_date_tibble) |>
    dplyr::select(
        -date,
        -exp_h,
        
        # NOTE swap securities
        -securities,
        # -securities_scl
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
        p = 1,
        type = "const",
    )


var_count_m <- 5
max_var_lag_m <- 1


# NOTE: restrikce: FG se objevuje pouze v rovnici pro exp
res_matrix_m <- rbind(
    # eq securities_scl
    c(rep(1, var_count_m * max_var_lag_m + 1), 0),
    # eq ppi
    c(rep(1, var_count_m * max_var_lag_m + 1), 0),
    # eq ir
    c(rep(1, var_count_m * max_var_lag_m + 1), 0),
    # eq cpi
    c(rep(1, var_count_m * max_var_lag_m + 1), 0),
    # eq oce
    c(rep(1, var_count_m * max_var_lag_m + 1), 1)
)

res_var_model_m <- restrict(var_model_m, method = "manual", resmat = res_matrix_m)


summary(res_var_model_m)


# prepoklady

# diagnostika modelu
residuals_m <- residuals(res_var_model_m)

as_tibble(residuals_m) |>
    add_column(new_col = NA, .before = 1) |>
    acf_pacf(20)

serial.test(res_var_model_m) # Autokorelace
arch.test(res_var_model_m) # Heteroskedasticita
normality.test(res_var_model_m)

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
map(res_var_model_m$varresult, vif)





akt_exp_m_irf <- irf(
    res_var_model_m,
    
    # NOTE: swap za securities kdyztak
    impulse = "securities_scl",
    response = "exp_m",
    n.ahead = 20,
    ortho = FALSE,
    runs = 1000
)

plot(akt_exp_m_irf)


# Granger causality
# NOTE: swap za aktiva kdyztak
causality(res_var_model_m, cause = "securities_scl")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp_m <- fevd(res_var_model_m, n.ahead = 20)
plot(v_decomp_m)

