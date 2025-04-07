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
        -date,
        -fg_u,
        -fg_z,
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
        fx_res = final(seas(fx_res)),
        ipi = final(seas(ipi)),
        # ir = final(seas(ir)), # NOTE: nefunguje ale asi neni sezonost takze idk
        cpi = final(seas(cpi)),
        exp_p = final(seas(exp_p)),
        exp_h = final(seas(exp_h))
    ) |>
    mutate(
        fx_res = c(NA, NA, diff(diff(log(fx_res)) * 100)),
        ipi = c(NA, NA, diff(diff(log(ipi)) * 100)),
        ir = c(NA, NA, diff(diff(ir))),
        cpi = c(NA, NA, diff(diff(log(cpi)) * 100)),
        exp_p = c(NA, diff(exp_p)),
        exp_h = c(NA, diff(exp_h)),

        # NOTE: Zpozdeni FG
        fg_u = c(NA, fg_u[-length(fg_u)]),
        fg_z = c(NA, fg_z[-length(fg_z)])
    ) |>
    drop_na()
# Omezení na před Covid cpi
# filter(date < "2022-01")

variables <- trans_tdata |>
    dplyr::select(
        -date,
        -fg_z,
        -fg_u
    )
check_stationarity(variables)
acf_pacf(tibble_data, 40)

# Graf všech proměnných v jednom grafu
trans_tdata |>
    dplyr::select(
        -fx_res,
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
        -exp_p,
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
    # eq fx_res
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
            "fx_res" = 0,
            "ipi" = 0,
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
impulse_fg_u_h <- ggplot(fg_u_exp_h_irf_results[max_var_lag_h:nrow(fg_u_exp_h_irf_results), ], aes(x = time - max_var_lag_h, y = exp_h)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        x = "Časový horizont",
        y = "EXP_H"
    )

impulse_fg_z_h <- ggplot(fg_z_exp_h_irf_results[max_var_lag_h:nrow(fg_z_exp_h_irf_results), ], aes(x = time - max_var_lag_h, y = exp_h)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        x = "Časový horizont",
        y = "EXP_H"
    )




akt_exp_h_irf <- irf(
    res_var_model_h,
    impulse = "fx_res",
    response = "exp_h",
    n.ahead = 20,
    ortho = FALSE,
    runs = 1000
)

horizons_h <- 0:(nrow(akt_exp_h_irf$irf$fx_res) - 1)

irf_df_h <- data.frame(
    horizon = horizons_h,
    response = akt_exp_h_irf$irf$fx_res[ , "exp_h"],
    lower = akt_exp_h_irf$Lower$fx_res[ , "exp_h"],
    upper = akt_exp_h_irf$Upper$fx_res[ , "exp_h"]
)
impulse_aktiva_h <- ggplot(irf_df_h, aes(x = horizon, y = response)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        x = "Časový horizont",
        y = "EXP_H"
    )





# Granger causality
causality(res_var_model_h, cause = "fx_res")

# FIX: zase hazi chybu asi - exogenni
# causality(res_var_model_h, cause = "fg_z")
# causality(res_var_model_h, cause = "fg_u")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp_h <- fevd(res_var_model_h, n.ahead = 20)
plot(v_decomp_h)





# PROFESIONALOVE ==============
trans_tdata_p <- trans_tdata |>
    dplyr::select(
        -date,
        -exp_h,
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
        p = 2,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata_p$fg_u,
            fg_z = trans_tdata_p$fg_z
        )
    )


var_count_p <- 5
max_var_lag_p <- 2


# NOTE: restrikce: FG se objevuje pouze v rovnici pro exp
res_matrix_p <- rbind(
    # eq fx_res
    c(rep(1, var_count_p * max_var_lag_p + 1), 0, 0),
    # eq ipi
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
map(res_var_model_p$varresult, vif)


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
            "fx_res" = 0,
            "ipi" = 0,
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
                
                all_coef_p[[k]][, 1][6] * irf_exog_fg[t - 2, 2] +
                    all_coef_p[[k]][, 1][7] * irf_exog_fg[t - 2, 2] +
                    all_coef_p[[k]][, 1][8] * irf_exog_fg[t - 2, 3] +
                    all_coef_p[[k]][, 1][9] * irf_exog_fg[t - 2, 4] +
                    all_coef_p[[k]][, 1][10] * irf_exog_fg[t - 2, 5]
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
impulse_fg_u_p <- ggplot(fg_u_exp_p_irf_results[max_var_lag_p:nrow(fg_u_exp_p_irf_results), ], aes(x = time - max_var_lag_p, y = exp_p)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        x = "Časový horizont",
        y = "EXP_P"
    )

impulse_fg_z_p <- ggplot(fg_z_exp_p_irf_results[max_var_lag_p:nrow(fg_u_exp_p_irf_results), ], aes(x = time - max_var_lag_p, y = exp_p)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        x = "Časový horizont",
        y = "EXP_P"
    )





akt_exp_p_irf <- irf(
    res_var_model_p,
    impulse = "fx_res",
    response = "exp_p",
    n.ahead = 20,
    ortho = FALSE,
    runs = 1000
)

horizons_p <- 0:(nrow(akt_exp_p_irf$irf$fx_res) - 1)

irf_df_p <- data.frame(
    horizon = horizons_p,
    response = akt_exp_p_irf$irf$fx_res[ , "exp_p"],
    lower = akt_exp_p_irf$Lower$fx_res[ , "exp_p"],
    upper = akt_exp_p_irf$Upper$fx_res[ , "exp_p"]
)
impulse_aktiva_p <- ggplot(irf_df_p, aes(x = horizon, y = response)) +
    geom_line(color = "black", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    theme_minimal() +
    labs(
        x = "Časový horizont",
        y = "EXP_P"
    )




# Granger causality
causality(res_var_model_p, cause = "fx_res")

# FIX: zase hazi chybu asi - exogenni
# causality(res_var_model_p, cause = "fg_z")
# causality(res_var_model_p, cause = "fg_u")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp_p <- fevd(res_var_model_p, n.ahead = 20)
plot(v_decomp_p)



# NOTE: graf vsech irf
col1_label <- textGrob("Domácnosti", gp = gpar(fontsize = 14, fontface = "bold"))
col2_label <- textGrob("Profesionálové", gp = gpar(fontsize = 14, fontface = "bold"))

row1_label <- textGrob("FX_RES", rot = 90, gp = gpar(fontsize = 14, fontface = "bold"))
row2_label <- textGrob("FG_U", rot = 90, gp = gpar(fontsize = 14, fontface = "bold"))
row3_label <- textGrob("FG_Z", rot = 90, gp = gpar(fontsize = 14, fontface = "bold"))

grid.arrange(
    # Horní řádek s popisky sloupců – první buňka prázdná
    arrangeGrob(
        textGrob(""), col1_label, col2_label,
        ncol = 3,
        widths = c(1, 3, 3)
    ),
    # První řádek grafů s řádkovým popiskem vlevo
    arrangeGrob(
        row1_label, impulse_aktiva_h, impulse_aktiva_p,
        ncol = 3,
        widths = c(0.3, 3, 3)
    ),
    # Druhý řádek grafů s řádkovým popiskem vlevo
    arrangeGrob(
        row2_label, impulse_fg_u_h, impulse_fg_u_p,
        ncol = 3,
        widths = c(0.3, 3, 3)
    ),
    arrangeGrob(
        row3_label, impulse_fg_z_h, impulse_fg_z_p,
        ncol = 3,
        widths = c(0.3, 3, 3)
    ),
    nrow = 4,
    heights = c(0.7, 3, 3, 3)
)

