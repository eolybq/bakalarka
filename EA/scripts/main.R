rm(list = ls())

library(tseries)
library(car)
library(forecast)
library(tidyverse)
library(urca)
library(vars)
library(seasonal)


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

# delsi vzorek (bez exp_m)
variables <- tibble_data |>
    dplyr::select(
        -date,
        -fg_u,
        -fg_z
    ) |>
    drop_na()
without_exp_m <- check_stationarity(variables)
# exp_m
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


trans_tdata <- tibble_data |>
    mutate(
        securities = final(seas(securities)),
        securities_scl = final(seas(securities_scl)),
        assets = final(seas(assets)),
        assets_scl = final(seas(assets_scl)),
        # ir = final(seas(ir)), # NOTE: nefunguje ale asi neni sezonost takze idk
        hicp = final(seas(hicp)),
        exp_h = final(seas(exp_h))
    ) |>
    mutate(
        assets = c(NA, NA, diff(diff(log(assets)) * 100)),
        assets_scl = c(NA, NA, diff(diff(log(assets_scl)) * 100)),
        securities = c(NA, NA, diff(diff(log(securities)) * 100)),
        securities_scl = c(NA, NA, diff(diff(log(securities_scl)) * 100)),
        ipi = c(NA, NA, diff(diff(log(ipi)) * 100)),
        hicp = c(NA, NA, diff(diff(log(hicp)) * 100)),
        ir = c(NA, diff(ir)),
        exp_h = c(NA, diff(exp_h)),
        
        # NOTE: Zpozdeni FG
        fg_u = c(NA, fg_u[-length(fg_u)]),
        fg_z = c(NA, fg_z[-length(fg_z)])
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
        -fg_u,
        -fg_z
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
        -assets,
        -securities,
        -fg_u,
        -fg_z
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
        
        # NOTE: swap za assets
        -assets,
        # -assets_scl,
        
        # NOTE swap securities / assets
        -securities,
        -securities_scl
    )

# vyber zpozdeni
lag_optimal_h <- trans_tdata_h |>
     dplyr::select(
         -fg_u,
         -fg_z,
     ) |>
    VARselect(
        lag.max = 20,
        type = "const",
         exogen = tibble(
             fg_u = trans_tdata_h$fg_u,
             fg_z = trans_tdata_h$fg_z
         )
    )
lag_optimal_h

# odhad
var_model_h <- trans_tdata_h |>
    dplyr::select(
        -fg_u,
        -fg_z,
    ) |>
    vars::VAR(
        p = 4,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata_h$fg_u,
            fg_z = trans_tdata_h$fg_z
        )
    )


var_count_h <- 5
max_var_lag_h <- 4


# NOTE: restrikce: FG se objevuje pouze v rovnici pro exp
res_matrix_h <- rbind(
    # eq assets_scl
    c(rep(1, var_count_h * max_var_lag_h + 1), 0, 0),
    # eq ipi
    c(rep(1, var_count_h * max_var_lag_h + 1), 0, 0),
    # eq ir
    c(rep(1, var_count_h * max_var_lag_h + 1), 0, 0),
    # eq cpi
    c(rep(1, var_count_h * max_var_lag_h + 1), 0, 0),
    # eq oce
    c(rep(1, var_count_h * max_var_lag_h + 1), 1, 1)
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


# NOTE: HAC robustní odchylka pro FG promenne v rovnici exp_h hlavne pro IRF
hac_matrix_h <- vcovHAC(res_var_model_h$varresult$exp_h)

# HAC sd FG 
hac_sd_fg_u_h <- hac_matrix_h[nrow(hac_matrix_h) - 1, ncol(hac_matrix_h) - 1]
hac_sd_fg_z_h <- hac_matrix_h[nrow(hac_matrix_h), ncol(hac_matrix_h)]


# IRF
# CUSTOM IRF pro exogenni FG na exp_h (+ bootstrap IS)
irf_runs <- 1000
horizon <- 20

exogen_irf_h <- function(fg_type, fg_sd, var_count, max_var_lag) {
    set.seed(42) # Pro reprodukovatelnost
    
    
    all_coef_h <- coef(res_var_model_h)
    
    
    # Inicializace matice pro simulace IRF
    sim_irf_h <- array(0, dim = c(horizon + max_var_lag, var_count, irf_runs))
    
    for (sim in 1:irf_runs) {
        irf_exog_fg <- tibble(
            "assets_scl" = 0,
            "ipi" = 0,
            "ir" = 0,
            "hicp" = 0,
            "exp_h" = 0,
            .rows = horizon + max_var_lag
        )
        
        # Počáteční šok v první periodě (o 1 jednotku)
        irf_exog_fg[max_var_lag + 1, 5] <- fg_type[["Estimate"]] + rnorm(1, mean = 0, sd = fg_sd)
        
        # Simulace dopadu šoku v dalších periodách
        for (t in (max_var_lag + 2):(horizon + max_var_lag)) {
            for (k in 1:var_count) {
                irf_exog_fg[t, k] <- all_coef_h[[k]][, 1][1] * irf_exog_fg[t - 1, 1] +
                    all_coef_h[[k]][, 1][2] * irf_exog_fg[t - 1, 2] +
                    all_coef_h[[k]][, 1][3] * irf_exog_fg[t - 1, 3] +
                    all_coef_h[[k]][, 1][4] * irf_exog_fg[t - 1, 4] +
                    all_coef_h[[k]][, 1][5] * irf_exog_fg[t - 1, 5]
                
                all_coef_h[[k]][, 1][6] * irf_exog_fg[t - 2, 2] +
                    all_coef_h[[k]][, 1][7] * irf_exog_fg[t - 2, 2] +
                    all_coef_h[[k]][, 1][8] * irf_exog_fg[t - 2, 3] +
                    all_coef_h[[k]][, 1][9] * irf_exog_fg[t - 2, 4] +
                    all_coef_h[[k]][, 1][10] * irf_exog_fg[t - 2, 5]
                
                all_coef_h[[k]][, 1][11] * irf_exog_fg[t - 3, 2] +
                    all_coef_h[[k]][, 1][12] * irf_exog_fg[t - 3, 2] +
                    all_coef_h[[k]][, 1][13] * irf_exog_fg[t - 3, 3] +
                    all_coef_h[[k]][, 1][14] * irf_exog_fg[t - 3, 4] +
                    all_coef_h[[k]][, 1][15] * irf_exog_fg[t - 3, 5]
                
                all_coef_h[[k]][, 1][16] * irf_exog_fg[t - 4, 2] +
                    all_coef_h[[k]][, 1][17] * irf_exog_fg[t - 4, 2] +
                    all_coef_h[[k]][, 1][18] * irf_exog_fg[t - 4, 3] +
                    all_coef_h[[k]][, 1][19] * irf_exog_fg[t - 4, 4] +
                    all_coef_h[[k]][, 1][20] * irf_exog_fg[t - 4, 5]
            }
        }
        
        # Uložení simulace
        sim_irf_h[, , sim] <- as.matrix(irf_exog_fg)
    }
    
    # Výpočet mediánu a intervalů spolehlivosti
    irf_median_h <- apply(sim_irf_h, c(1, 2), median)
    irf_lower_h <- apply(sim_irf_h, c(1, 2), quantile, probs = 0.025)
    irf_upper_h <- apply(sim_irf_h, c(1, 2), quantile, probs = 0.975)
    
    return(
        tibble(
            time = 1:(horizon + max_var_lag),
            exp_h = irf_median_h[, 5],
            lower = irf_lower_h[, 5],
            upper = irf_upper_h[, 5]
        )
    )
}

fg_u_coef_h <- coef(res_var_model_h)$exp_h["fg_u", ] # Dopad exogenní proměnné fg u
fg_z_coef_h <- coef(res_var_model_h)$exp_h["fg_z", ] # Dopad exogenní proměnné fg z

fg_u_exp_h_irf_results <- exogen_irf_h(fg_u_coef_h, fg_sd = hac_sd_fg_u_h, var_count = var_count_h, max_var_lag = max_var_lag_h)
fg_z_exp_h_irf_results <- exogen_irf_h(fg_z_coef_h, fg_sd = hac_sd_fg_z_h, var_count = var_count_h, max_var_lag = max_var_lag_h)


# Plot s intervaly spolehlivosti
ggplot(fg_u_exp_h_irf_results[max_var_lag_h:nrow(fg_u_exp_h_irf_results), ], aes(x = time, y = exp_h)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        title = "Impulzní odezva v exp_h na jednotkový šok v uvolnění FG (s intervaly spolehlivosti)",
        x = "Časový horizont",
        y = "exp_h"
    )

ggplot(fg_z_exp_h_irf_results[max_var_lag_h:nrow(fg_z_exp_h_irf_results), ], aes(x = time, y = exp_h)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        title = "Impulzní odezva v exp_h na jednotkový šok v zpřísnění FG (s intervaly spolehlivosti)",
        x = "Časový horizont",
        y = "exp_h"
    )




akt_exp_h_irf <- irf(
    res_var_model_h,
    
    # NOTE: swap za assets kdyztak
    impulse = "assets_scl",
    response = "exp_h",
    n.ahead = 20,
    ortho = FALSE,
    runs = 1000
)

plot(akt_exp_h_irf)


# Granger causality
# NOTE: swap za fx_res kdyztak
causality(res_var_model_h, cause = "assets_scl")

# FIX: zase hazi chybu asi - exogenni
# causality(res_var_model_h, cause = "fg_z")
# causality(res_var_model_h, cause = "fg_u")


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
        
        # NOTE: swap za assets
        -assets,
        # -assets_scl,
        
        # NOTE swap securities / assets
        -securities,
        -securities_scl
    ) |>
    drop_na()


# vyber zpozdeni
lag_optimal_m <- trans_tdata_m |>
    dplyr::select(
        -fg_u,
        -fg_z,
    ) |>
    VARselect(
        lag.max = 20,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata_p$fg_u,
            fg_z = trans_tdata_p$fg_z
        )
    )
lag_optimal_m

# odhad
var_model_m <- trans_tdata_m |>
    dplyr::select(
        -fg_u,
        -fg_z,
    ) |>
    vars::VAR(
        p = 2,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata_p$fg_u,
            fg_z = trans_tdata_p$fg_z
        )
    )


var_count_m <- 5
max_var_lag_m <- 2


# NOTE: restrikce: FG se objevuje pouze v rovnici pro exp
res_matrix_m <- rbind(
    # eq assets_scl
    c(rep(1, var_count_m * max_var_lag_m + 1), 0, 0),
    # eq ipi
    c(rep(1, var_count_m * max_var_lag_m + 1), 0, 0),
    # eq ir
    c(rep(1, var_count_m * max_var_lag_m + 1), 0, 0),
    # eq cpi
    c(rep(1, var_count_m * max_var_lag_m + 1), 0, 0),
    # eq oce
    c(rep(1, var_count_m * max_var_lag_m + 1), 1, 1)
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


# NOTE: HAC robustní odchylka pro FG promenne v rovnici exp_p hlavne pro IRF
hac_matrix_m <- vcovHAC(res_var_model_m$varresult$exp_m)

# HAC sd FG 
hac_sd_fg_u_m <- hac_matrix_m[nrow(hac_matrix_m) - 1, ncol(hac_matrix_m) - 1]
hac_sd_fg_z_m <- hac_matrix_m[nrow(hac_matrix_m), ncol(hac_matrix_m)]


# IRF
irf_runs <- 1000
horizon <- 20

exogen_irf_m <- function(fg_type, fg_sd, var_count, max_var_lag) {
    # CUSTOM IRF pro exogenni FG na exp_p (+ bootstrap IS)
    set.seed(42) # Pro reprodukovatelnost
    
    
    all_coef_m <- coef(res_var_model_m)
    
    # Inicializace matice pro simulace IRF
    sim_irf_m <- array(0, dim = c(horizon + max_var_lag, var_count, irf_runs))
    
    for (sim in 1:irf_runs) {
        irf_exog_fg <- tibble(
            "assets_scl" = 0,
            "ipi" = 0,
            "ir" = 0,
            "cpi" = 0,
            "exp_m" = 0,
            .rows = horizon + max_var_lag
        )
        
        # Počáteční šok v první periodě (o 1 jednotku)
        irf_exog_fg[max_var_lag + 1, 5] <- fg_type[["Estimate"]] + rnorm(1, mean = 0, sd = fg_sd)
        
        # Simulace dopadu šoku v dalších periodách
        for (t in (max_var_lag + 2):(horizon + max_var_lag)) {
            for (k in 1:var_count) {
                irf_exog_fg[t, k] <- all_coef_m[[k]][, 1][1] * irf_exog_fg[t - 1, 1] +
                    all_coef_m[[k]][, 1][2] * irf_exog_fg[t - 1, 2] +
                    all_coef_m[[k]][, 1][3] * irf_exog_fg[t - 1, 3] +
                    all_coef_m[[k]][, 1][4] * irf_exog_fg[t - 1, 4] +
                    all_coef_m[[k]][, 1][5] * irf_exog_fg[t - 1, 5]
                
                all_coef_m[[k]][, 1][6] * irf_exog_fg[t - 2, 2] +
                    all_coef_m[[k]][, 1][7] * irf_exog_fg[t - 2, 2] +
                    all_coef_m[[k]][, 1][8] * irf_exog_fg[t - 2, 3] +
                    all_coef_m[[k]][, 1][9] * irf_exog_fg[t - 2, 4] +
                    all_coef_m[[k]][, 1][10] * irf_exog_fg[t - 2, 5]
            }
        }
        
        # Uložení simulace
        sim_irf_m[, , sim] <- as.matrix(irf_exog_fg)
    }
    
    
    # Výpočet mediánu a intervalů spolehlivosti
    irf_median_m <- apply(sim_irf_m, c(1, 2), median)
    irf_lower_m <- apply(sim_irf_m, c(1, 2), quantile, probs = 0.025)
    irf_upper_m <- apply(sim_irf_m, c(1, 2), quantile, probs = 0.975)
    
    return(
        tibble(
            time = 1:(horizon + max_var_lag),
            exp_p = irf_median_m[, 5],
            lower = irf_lower_m[, 5],
            upper = irf_upper_m[, 5]
        )
    )
}

fg_u_coef_m <- coef(res_var_model_m)$exp_m["fg_u", ] # Dopad exogenní proměnné fg u
fg_z_coef_m <- coef(res_var_model_m)$exp_m["fg_z", ] # Dopad exogenní proměnné fg z

fg_u_exp_m_irf_results <- exogen_irf_m(fg_u_coef_m, fg_sd = hac_sd_fg_u_m, var_count = var_count_m, max_var_lag = max_var_lag_m)
fg_z_exp_m_irf_results <- exogen_irf_m(fg_z_coef_m, fg_sd = hac_sd_fg_z_m, var_count = var_count_m, max_var_lag = max_var_lag_m)


# Plot s intervaly spolehlivosti
ggplot(fg_u_exp_m_irf_results, aes(x = time, y = exp_m)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        title = "Impulzní odezva v exp_m na jednotkový šok v uvolnění FG (s intervaly spolehlivosti)",
        x = "Časový horizont",
        y = "exp_m"
    )

ggplot(fg_z_exp_m_irf_results, aes(x = time, y = exp_m)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        title = "Impulzní odezva v exp_m na jednotkový šok v zpřísnění FG (s intervaly spolehlivosti)",
        x = "Časový horizont",
        y = "exp_m"
    )





akt_exp_m_irf <- irf(
    res_var_model_m,
    
    # NOTE: swap za assets kdyztak
    impulse = "assets_scl",
    response = "exp_m",
    n.ahead = 20,
    ortho = FALSE,
    runs = 1000
)

plot(akt_exp_m_irf)


# Granger causality
# NOTE: swap za aktiva kdyztak
causality(res_var_model_m, cause = "assets_scl")

# FIX: zase hazi chybu asi - exogenni
# causality(res_var_model_m, cause = "fg_z")
# causality(res_var_model_m, cause = "fg_u")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp_m <- fevd(res_var_model_m, n.ahead = 20)
plot(v_decomp_m)

