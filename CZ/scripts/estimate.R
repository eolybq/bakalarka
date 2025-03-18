rm(list = ls())

library(tseries)
library(forecast)
library(tidyverse)
library(urca)
library(vars)
library(seasonal)


load("data/tibble_data.RData")


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

variables <- drop_na(tibble_data[, -1])
check_stationarity(variables)


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


trans_tdata <- tibble_data |>
    mutate(
        aktiva = final(seas(aktiva)),
        aktiva_scaled = final(seas(aktiva_scaled)),
        nezam = final(seas(nezam)),
        # urok = final(seas(urok)), # NOTE: nefunguje ale asi stejne ocisteno takze idk
        inflace = final(seas(inflace)),
        oce_p = final(seas(oce_p)),
        oce_h = final(seas(oce_h))
    ) |>
    mutate(
        aktiva = c(NA, diff(log(aktiva), lag = 1) * 100),
        aktiva_scaled = c(NA, diff(aktiva_scaled, lag = 1)),
        nezam = c(NA, diff(nezam, lag = 1)),
        urok = c(NA, diff(urok, lag = 1)),
        inflace = c(NA, diff(log(inflace), lag = 1) * 100),
        oce_p = c(NA, diff(oce_p, lag = 1)),
        oce_h = c(NA, diff(oce_h, lag = 1)),

        # NOTE: Zpozdeni FG
        forward_guidance_uvolneni = c(forward_guidance_uvolneni[2:length(forward_guidance_uvolneni)], NA),
        forward_guidance_zprisneni = c(forward_guidance_zprisneni[2:length(forward_guidance_zprisneni)], NA)
    ) |>
    rename(
        # FIX: Přejmenovat ostatní proměnné na nějaký rozumný jména
        fg_u_t_1 = forward_guidance_uvolneni,
        fg_z_t_1 = forward_guidance_zprisneni
    ) |>
    drop_na() |>
    # Omezení na před Covid inflace
    filter(datum < "2022-01")

variables <- trans_tdata |>
    dplyr::select(
        -datum,
        -fg_z_t_1,
        -fg_u_t_1
    )
check_stationarity(variables)
acf_pacf(tibble_data, 40)

# Graf všech proměnných v jednom grafu
trans_tdata |>
    dplyr::select(
        -aktiva
    ) |>
    pivot_longer(cols = -datum) |>
    ggplot(aes(x = datum, y = value, color = name, group = name)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Plot všech proměnných v jednom grafu")


# ========================================================
# Model
# ========================================================

# DOMACNOSTI ==============
trans_tdata_h <- trans_tdata |>
    dplyr::select(
        -datum,
        -oce_p,

        # NOTE: swap za aktiva kdyztak
        -aktiva
    )

# vyber zpozdeni
lag_optimal_h <- trans_tdata_h |>
    dplyr::select(
        -fg_u_t_1,
        -fg_z_t_1,
    ) |>
    VARselect(
        lag.max = 20,
        type = "const",
        exogen = tibble(
            fg_u_t_1 = trans_tdata_h$fg_u_t_1,
            # fg_z_t_1 = trans_tdata_h$fg_z_t_1
        )
    )
lag_optimal_h

# odhad
var_model_h <- trans_tdata_h |>
    dplyr::select(
        -fg_u_t_1,
        -fg_z_t_1,
    ) |>
    vars::VAR(
        p = 1,
        type = "const",
        exogen = tibble(
            fg_u_t_1 = trans_tdata_h$fg_u_t_1,
            # fg_z_t_1 = trans_tdata_h$fg_z_t_1
        )
    )


# NOTE: restrikce: FG se objevuje pouze v rovnici pro exp
res_matrix_h <- rbind(
    # eq aktiva_scaled
    c(1, 1, 1, 1, 1, 1, 0),
    # eq nezam
    c(1, 1, 1, 1, 1, 1, 0),
    # eq urok
    c(1, 1, 1, 1, 1, 1, 0),
    # eq inflace
    c(1, 1, 1, 1, 1, 1, 0),
    # eq oce
    c(1, 1, 1, 1, 1, 1, 1)
)

res_var_model_h <- restrict(var_model_h, method = "manual", resmat = res_matrix_h)


# FIX: stale hazi tu stejnou chybu jak ve stack overflow
# vcovTEST <- lapply(res_var_model_h$varresult, function(eq) vcovHAC(eq))

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


# IRF
set.seed(42) # Pro reprodukovatelnost
irf_runs <- 1000

# CUSTOM IRF pro exogenni FG na oce_h (+ bootstrap IS)
var_count <- 5
max_var_lag <- 1
horizon <- 20

all_coef_h <- coef(res_var_model_h)
B_exog_h <- coef(res_var_model_h)$oce_h["fg_u_t_1", ] # Dopad exogenní proměnné na Y1
# C_exog_p <- coef(res_var_model_h)$oce_h["fg_z", ] # Dopad exogenní proměnné na Y2

# Inicializace matice pro simulace IRF
sim_irf_h <- array(0, dim = c(horizon + max_var_lag, var_count, irf_runs))

for (sim in 1:irf_runs) {

    irf_exog_fg_u <- tibble(
        "aktiva_scaled" = 0,
        "nezam" = 0,
        "urok" = 0,
        "inflace" = 0,
        "oce_h" = 0,
        .rows = horizon + max_var_lag
    )

    # Počáteční šok v první periodě (o 1 jednotku)
    irf_exog_fg_u[max_var_lag + 1, 5] <- B_exog_h[["Estimate"]] + rnorm(1, mean = 0, sd = B_exog_h[["Std. Error"]])

    # Simulace dopadu šoku v dalších periodách
    for (t in (max_var_lag + 2):(horizon + max_var_lag)) {
        for (k in 1:var_count) {
            irf_exog_fg_u[t, k] <- all_coef_h[[k]][, 1][1] * irf_exog_fg_u[t - 1, 1] +
                all_coef_h[[k]][, 1][2] * irf_exog_fg_u[t - 1, 2] +
                all_coef_h[[k]][, 1][3] * irf_exog_fg_u[t - 1, 3] +
                all_coef_h[[k]][, 1][4] * irf_exog_fg_u[t - 1, 4] +
                all_coef_h[[k]][, 1][5] * irf_exog_fg_u[t - 1, 5]
        }
    }

    # Uložení simulace
    sim_irf_h[, , sim] <- as.matrix(irf_exog_fg_u)
}

# Výpočet mediánu a intervalů spolehlivosti
irf_median_h <- apply(sim_irf_h, c(1, 2), median)
irf_lower_h <- apply(sim_irf_h, c(1, 2), quantile, probs = 0.05)
irf_upper_h <- apply(sim_irf_h, c(1, 2), quantile, probs = 0.95)

fg_u_oce_h_irf_results <- tibble(
    time = 1:(horizon + max_var_lag),
    oce_h = irf_median_h[, 5],
    lower = irf_lower_h[, 5],
    upper = irf_upper_h[, 5]
)

# Plot s intervaly spolehlivosti
ggplot(fg_u_oce_h_irf_results, aes(x = time, y = oce_h)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        title = "Impulzní odezva v oce_h na šok v fg_u (s intervaly spolehlivosti)",
        x = "Časový horizont",
        y = "oce_h"
    )




akt_oce_h_irf <- irf(
    res_var_model_h,

    # NOTE: swap za aktiva kdyztak
    impulse = "aktiva_scaled",
    response = "oce_h",
    n.ahead = 20,
    ortho = FALSE,
    runs = 100
)

plot(akt_oce_h_irf)


# Granger causality
# NOTE: swap za aktiva kdyztak
causality(res_var_model_h, cause = "aktiva_scaled")

# FIX: zase hazi chybu asi - exogenni
# causality(res_var_model_h, cause = "fg_z")
# causality(res_var_model_h, cause = "fg_u")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp_h <- fevd(res_var_model_h, n.ahead = 20)
plot(v_decomp_h)





# PROFESIONALOVE ==============
trans_tdata_p <- trans_tdata |>
    dplyr::select(
        -datum,
        -oce_h,

        # NOTE: swap za aktiva kdyztak
        -aktiva
    )

# vyber zpozdeni
lag_optimal_p <- trans_tdata_p |>
    dplyr::select(
        -fg_u_t_1,
        -fg_z_t_1,
    ) |>
    VARselect(
        lag.max = 20,
        type = "const",
        exogen = tibble(
            fg_u_t_1 = trans_tdata_p$fg_u_t_1,
            # fg_z_t_1 = trans_tdata_p$fg_z_t_1
        )
    )
lag_optimal_p

# odhad
var_model_p <- trans_tdata_p |>
    dplyr::select(
        -fg_u_t_1,
        -fg_z_t_1,
    ) |>
    vars::VAR(
        p = 1,
        type = "const",
        exogen = tibble(
            fg_u_t_1 = trans_tdata_p$fg_u_t_1,
            # fg_z_t_1 = trans_tdata_p$fg_z_t_1
        )
    )


# NOTE: restrikce: FG se objevuje pouze v rovnici pro exp
res_matrix_p <- rbind(
    # eq aktiva_scaled
    c(1, 1, 1, 1, 1, 1, 0),
    # eq nezam
    c(1, 1, 1, 1, 1, 1, 0),
    # eq urok
    c(1, 1, 1, 1, 1, 1, 0),
    # eq inflace
    c(1, 1, 1, 1, 1, 1, 0),
    # eq oce
    c(1, 1, 1, 1, 1, 1, 1)
)

res_var_model_p <- restrict(var_model_p, method = "manual", resmat = res_matrix_p)


# FIX: stale hazi tu stejnou chybu jak ve stack overflow
# vcovTEST <- lapply(res_var_model_p$varresult, function(eq) vcovHAC(eq))

summary(res_var_model_p)


# prepoklady

# diagnostika modelu
residuals_p <- residuals(res_var_model_p)

as_tibble(residuals_p) |>
    add_column(new_col = NA, .before = 1) |>
    acf_pacf(20)

serial.test(res_var_model_p) # Autokorelace
arch.test(res_var_model_p) # Heteroskedasticita
normality.test(res_var_model_p)

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


# IRF
irf_runs <- 1000

# CUSTOM IRF pro exogenni FG na oce_h (+ bootstrap IS)
var_count <- 5
max_var_lag <- 1
horizon <- 20

all_coef_p <- coef(res_var_model_p)
B_exog_p <- coef(res_var_model_p)$oce_p["fg_u_t_1", ] # Dopad exogenní proměnné na Y1
# C_exog_p <- coef(res_var_model_p)$oce_h["fg_z", ] # Dopad exogenní proměnné na Y2

# Inicializace matice pro simulace IRF
sim_irf_p <- array(0, dim = c(horizon + max_var_lag, var_count, irf_runs))

for (sim in 1:irf_runs) {

    irf_exog_fg_u <- tibble(
        "aktiva_scaled" = 0,
        "nezam" = 0,
        "urok" = 0,
        "inflace" = 0,
        "oce_h" = 0,
        .rows = horizon + max_var_lag
    )

    # Počáteční šok v první periodě (o 1 jednotku)
    irf_exog_fg_u[max_var_lag + 1, 5] <- B_exog_p[["Estimate"]] + rnorm(1, mean = 0, sd = B_exog_p[["Std. Error"]])

    # Simulace dopadu šoku v dalších periodách
    for (t in (max_var_lag + 2):(horizon + max_var_lag)) {
        for (k in 1:var_count) {
            irf_exog_fg_u[t, k] <- all_coef_p[[k]][, 1][1] * irf_exog_fg_u[t - 1, 1] +
                all_coef_p[[k]][, 1][2] * irf_exog_fg_u[t - 1, 2] +
                all_coef_p[[k]][, 1][3] * irf_exog_fg_u[t - 1, 3] +
                all_coef_p[[k]][, 1][4] * irf_exog_fg_u[t - 1, 4] +
                all_coef_p[[k]][, 1][5] * irf_exog_fg_u[t - 1, 5]
        }
    }

    # Uložení simulace
    sim_irf_p[, , sim] <- as.matrix(irf_exog_fg_u)
}

# Výpočet mediánu a intervalů spolehlivosti
irf_median_p <- apply(sim_irf_p, c(1, 2), median)
irf_lower_p <- apply(sim_irf_p, c(1, 2), quantile, probs = 0.05)
irf_upper_p <- apply(sim_irf_p, c(1, 2), quantile, probs = 0.95)

fg_u_oce_p_irf_results <- tibble(
    time = 1:(horizon + max_var_lag),
    oce_p = irf_median_p[, 5],
    lower = irf_lower_p[, 5],
    upper = irf_upper_p[, 5]
)

# Plot s intervaly spolehlivosti
ggplot(fg_u_oce_p_irf_results, aes(x = time, y = oce_p)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        title = "Impulzní odezva v oce_h na šok v fg_u (s intervaly spolehlivosti)",
        x = "Časový horizont",
        y = "oce_p"
    )



akt_oce_p_irf <- irf(
    res_var_model_p,

    # NOTE: swap za aktiva kdyztak
    impulse = "aktiva_scaled",
    response = "oce_p",
    n.ahead = 20,
    ortho = FALSE,
    runs = 100
)

plot(akt_oce_p_irf)


# Granger causality
# NOTE: swap za aktiva kdyztak
causality(res_var_model_p, cause = "aktiva_scaled")

# FIX: zase hazi chybu asi - exogenni
# causality(res_var_model_p, cause = "fg_z")
# causality(res_var_model_p, cause = "fg_u")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp_p <- fevd(res_var_model_p, n.ahead = 20)
plot(v_decomp_p)
