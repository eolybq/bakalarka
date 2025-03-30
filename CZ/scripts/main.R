rm(list = ls())

library(tseries)
library(car)
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

variables <- tibble_data |>
    dplyr::select(
        -datum,
        -forward_guidance_uvolneni,
        -forward_guidance_zprisneni,
    ) |>
    drop_na()
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
        ipp = final(seas(ipp)),
        # urok = final(seas(urok)), # NOTE: nefunguje ale asi neni sezonost takze idk
        inflace = final(seas(inflace)),
        oce_p = final(seas(oce_p)),
        oce_h = final(seas(oce_h))
    ) |>
    mutate(
        aktiva = c(NA, NA, diff(diff(log(aktiva)) * 100)),
        aktiva_scaled = c(NA, NA, diff(diff(log(aktiva_scaled)) * 100)),
        nezam = c(NA, diff(nezam)),
        ipp = c(NA, NA, diff(diff(log(ipp)) * 100)),
        urok = c(NA, diff(urok)),
        inflace = c(NA, NA, diff(diff(log(inflace)) * 100)),
        oce_p = c(NA, diff(oce_p)),
        oce_h = c(NA, diff(oce_h)),

        # NOTE: Zpozdeni FG
        forward_guidance_uvolneni = c(NA, forward_guidance_uvolneni[2:length(forward_guidance_uvolneni)]),
        forward_guidance_zprisneni = c(NA, forward_guidance_zprisneni[2:length(forward_guidance_zprisneni)])
    ) |>
    rename(
        assets = aktiva,
        assets_scl = aktiva_scaled,
        fg_u = forward_guidance_uvolneni,
        fg_z = forward_guidance_zprisneni,
        unemp = nezam,
        ir = urok,
        cpi = inflace,
        exp_h = oce_h,
        exp_p = oce_p
    ) |>
    drop_na()
# Omezení na před Covid inflace
# filter(datum < "2022-01")

variables <- trans_tdata |>
    dplyr::select(
        -datum,
        -fg_z,
        -fg_u
    )
check_stationarity(variables)
acf_pacf(tibble_data, 40)

# Graf všech proměnných v jednom grafu
trans_tdata |>
    dplyr::select(
        -assets,
        -fg_u,
        -fg_z
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
        -exp_p,

        # NOTE: swap za aktiva
        -assets,
        # -assets_scl,
        
        # NOTE: swap za unemp
        -unemp
        # -ipp
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
        p = 5,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata_h$fg_u,
            fg_z = trans_tdata_h$fg_z
        )
    )


var_count_h <- 5
max_var_lag_h <- 5


# NOTE: restrikce: FG se objevuje pouze v rovnici pro exp
res_matrix_h <- rbind(
    # eq assets_scl
    c(rep(1, var_count_h * max_var_lag_h + 1), 0, 0),
    # eq unemp/ipp
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
            # NOTE: swap za unemp
            # "unemp" = 0,
            "ipp" = 0,
            "ir" = 0,
            "cpi" = 0,
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

                    # FIX: dodelat pro spravny pocet zpozdeni

                # all_coef_h[[k]][, 1][11] * irf_exog_fg[t - 3, 2] +
                #     all_coef_h[[k]][, 1][12] * irf_exog_fg[t - 3, 2] +
                #     all_coef_h[[k]][, 1][13] * irf_exog_fg[t - 3, 3] +
                #     all_coef_h[[k]][, 1][14] * irf_exog_fg[t - 3, 4] +
                #     all_coef_h[[k]][, 1][15] * irf_exog_fg[t - 3, 5]
                # 
                # all_coef_h[[k]][, 1][16] * irf_exog_fg[t - 4, 2] +
                #     all_coef_h[[k]][, 1][17] * irf_exog_fg[t - 4, 2] +
                #     all_coef_h[[k]][, 1][18] * irf_exog_fg[t - 4, 3] +
                #     all_coef_h[[k]][, 1][19] * irf_exog_fg[t - 4, 4] +
                #     all_coef_h[[k]][, 1][20] * irf_exog_fg[t - 4, 5]
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

    # NOTE: swap za aktiva kdyztak
    impulse = "assets_scl",
    response = "exp_h",
    n.ahead = 20,
    ortho = FALSE,
    runs = 1000
)

plot(akt_exp_h_irf)


# Granger causality
# NOTE: swap za aktiva kdyztak
causality(res_var_model_h, cause = "assets_scl")

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
        -exp_h,

        # NOTE: swap za aktiva
        -assets,
       # -assets_scl,
       
        # NOTE: swap za unemp
        -unemp
       # -ipp
    )

# vyber zpozdeni
lag_optimal_p <- trans_tdata_p |>
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
lag_optimal_p

# odhad
var_model_p <- trans_tdata_p |>
    dplyr::select(
        -fg_u,
        -fg_z,
    ) |>
    vars::VAR(
        p = 1,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata_p$fg_u,
            fg_z = trans_tdata_p$fg_z
        )
    )


var_count_p <- 5
max_var_lag_p <- 1


# NOTE: restrikce: FG se objevuje pouze v rovnici pro exp
res_matrix_p <- rbind(
    # eq assets_scl
    c(rep(1, var_count_p * max_var_lag_p + 1), 0, 0),
    # eq unemp/ipp
    c(rep(1, var_count_p * max_var_lag_p + 1), 0, 0),
    # eq ir
    c(rep(1, var_count_p * max_var_lag_p + 1), 0, 0),
    # eq cpi
    c(rep(1, var_count_p * max_var_lag_p + 1), 0, 0),
    # eq oce
    c(rep(1, var_count_p * max_var_lag_p + 1), 1, 1)
)

res_var_model_p <- restrict(var_model_p, method = "manual", resmat = res_matrix_p)


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

# multikolinearita
map(res_var_model_h$varresult, vif)


# NOTE: HAC robustní odchylka pro FG promenne v rovnici exp_p hlavne pro IRF
hac_matrix_p <- vcovHAC(res_var_model_p$varresult$exp_p)

# HAC sd FG 
hac_sd_fg_u_p <- hac_matrix_p[nrow(hac_matrix_p) - 1, ncol(hac_matrix_p) - 1]
hac_sd_fg_z_p <- hac_matrix_p[nrow(hac_matrix_p), ncol(hac_matrix_p)]


# IRF
irf_runs <- 1000
horizon <- 20
    
exogen_irf_p <- function(fg_type, fg_sd, var_count, max_var_lag) {
    # CUSTOM IRF pro exogenni FG na exp_p (+ bootstrap IS)
    set.seed(42) # Pro reprodukovatelnost


    all_coef_p <- coef(res_var_model_p)

    # Inicializace matice pro simulace IRF
    sim_irf_p <- array(0, dim = c(horizon + max_var_lag, var_count, irf_runs))

    for (sim in 1:irf_runs) {
        irf_exog_fg <- tibble(
            "assets_scl" = 0,
            # NOTE: swap za unemp
            # "unemp" = 0,
            "ipp" = 0,
            "ir" = 0,
            "cpi" = 0,
            "exp_p" = 0,
            .rows = horizon + max_var_lag
        )

        # Počáteční šok v první periodě (o 1 jednotku)
        irf_exog_fg[max_var_lag + 1, 5] <- fg_type[["Estimate"]] + rnorm(1, mean = 0, sd = fg_sd)

        # Simulace dopadu šoku v dalších periodách
        for (t in (max_var_lag + 2):(horizon + max_var_lag)) {
            for (k in 1:var_count) {
                irf_exog_fg[t, k] <- all_coef_p[[k]][, 1][1] * irf_exog_fg[t - 1, 1] +
                    all_coef_p[[k]][, 1][2] * irf_exog_fg[t - 1, 2] +
                    all_coef_p[[k]][, 1][3] * irf_exog_fg[t - 1, 3] +
                    all_coef_p[[k]][, 1][4] * irf_exog_fg[t - 1, 4] +
                    all_coef_p[[k]][, 1][5] * irf_exog_fg[t - 1, 5]
            }
        }

        # Uložení simulace
        sim_irf_p[, , sim] <- as.matrix(irf_exog_fg)
    }


    # Výpočet mediánu a intervalů spolehlivosti
    irf_median_p <- apply(sim_irf_p, c(1, 2), median)
    irf_lower_p <- apply(sim_irf_p, c(1, 2), quantile, probs = 0.025)
    irf_upper_p <- apply(sim_irf_p, c(1, 2), quantile, probs = 0.975)

    return(
        tibble(
            time = 1:(horizon + max_var_lag),
            exp_p = irf_median_p[, 5],
            lower = irf_lower_p[, 5],
            upper = irf_upper_p[, 5]
        )
    )
}

fg_u_coef_p <- coef(res_var_model_p)$exp_p["fg_u", ] # Dopad exogenní proměnné fg u
fg_z_coef_p <- coef(res_var_model_p)$exp_p["fg_z", ] # Dopad exogenní proměnné fg z

fg_u_exp_p_irf_results <- exogen_irf_p(fg_u_coef_p, fg_sd = hac_sd_fg_u_p, var_count = var_count_p, max_var_lag = max_var_lag_p)
fg_z_exp_p_irf_results <- exogen_irf_p(fg_z_coef_p, fg_sd = hac_sd_fg_z_p, var_count = var_count_p, max_var_lag = max_var_lag_p)


# Plot s intervaly spolehlivosti
ggplot(fg_u_exp_p_irf_results, aes(x = time, y = exp_p)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        title = "Impulzní odezva v exp_p na jednotkový šok v uvolnění FG (s intervaly spolehlivosti)",
        x = "Časový horizont",
        y = "exp_p"
    )

ggplot(fg_z_exp_p_irf_results, aes(x = time, y = exp_p)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        title = "Impulzní odezva v exp_p na jednotkový šok v zpřísnění FG (s intervaly spolehlivosti)",
        x = "Časový horizont",
        y = "exp_p"
    )





akt_exp_p_irf <- irf(
    res_var_model_p,

    # NOTE: swap za aktiva kdyztak
    impulse = "assets_scl",
    response = "exp_p",
    n.ahead = 20,
    ortho = FALSE,
    runs = 1000
)

plot(akt_exp_p_irf)


# Granger causality
# NOTE: swap za aktiva kdyztak
causality(res_var_model_p, cause = "assets_scl")

# FIX: zase hazi chybu asi - exogenni
# causality(res_var_model_p, cause = "fg_z")
# causality(res_var_model_p, cause = "fg_u")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp_p <- fevd(res_var_model_p, n.ahead = 20)
plot(v_decomp_p)

