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

# NOTE: Funkce na stacionaritu a kointegraci z manahra_pred
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


# diffed_unemp <- diff(tibble_data[["nezam"]], lag = 1)
# autoplot(diffed_unemp)
# acf_pacf(diffed_unemp, 37)
#
# # NOTE: Je toto potřeba?
# s2diffed_unemp <- diff(diffed_unemp, lag = 12)
# autoplot(s2diffed_unemp)
# acf_pacf(s2diffed_unemp, 37)

# check_stationarity(tibble(first_diff = diffed_unemp))
# check_stationarity(tibble(s_diff = s2diffed_unemp))


# TODO: Johansen cointegration test



trans_tdata <- tibble_data |>
    mutate(
        aktiva = final(seas(aktiva)),
        aktiva_scl = final(seas(aktiva_scl)),
        nezam = final(seas(nezam)),
        # urok = final(seas(urok)), # NOTE: nefunguje ale asi stejne ocisteno takze idk
        inflace = final(seas(inflace)),
        oce_p = final(seas(oce_p)),
        oce_h = final(seas(oce_h))
    ) |>
    mutate(
        aktiva = c(NA, diff(log(aktiva) * 100, lag = 1)),
        aktiva_scl = c(NA, diff(aktiva_scl, lag = 1)),
        nezam = c(NA, diff(nezam, lag = 1)),
        urok = c(NA, diff(urok, lag = 1)),
        inflace = c(NA, diff(log(inflace) * 100, lag = 1)),
        oce_p = c(NA, diff(oce_p, lag = 1)),
        oce_h = c(NA, diff(oce_h, lag = 1)),

        # NOTE: Zpozdeni FG
        forward_guidance_uvolneni = c(forward_guidance_uvolneni[2:length(forward_guidance_uvolneni)], NA),
        forward_guidance_zprisneni = c(forward_guidance_zprisneni[2:length(forward_guidance_zprisneni)], NA)
    ) |>
    drop_na()

variables <- trans_tdata[, -1]
check_stationarity(variables)

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
    filter(datum < "2022-01") |>
    dplyr::select(
        -datum,
        -oce_p,

        # NOTE: swap za aktiva kdyztak
        -aktiva
    )

# vyber zpozdeni
lag_optimal <- trans_tdata_h |>
    dplyr::select(
        -forward_guidance_uvolneni,
        -forward_guidance_zprisneni,
    ) |>
    VARselect(
        lag.max = 20,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata_h$forward_guidance_uvolneni,
            fg_z = trans_tdata_h$forward_guidance_zprisneni
        )
    )
lag_optimal

# odhad
var_model_h <- trans_tdata_h |>
    dplyr::select(
        -forward_guidance_uvolneni,
        -forward_guidance_zprisneni,
    ) |>
    vars::VAR(
        p = 3,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata_h$forward_guidance_uvolneni,
            fg_z = trans_tdata_h$forward_guidance_zprisneni
        )
    )


# NOTE: restrikce: FG se objevuje pouze v rovnici pro exp
res_matrix_h <- rbind(
    # eq akriva_scl
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0),
    # eq nezam
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0),
    # eq urok
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0),
    # eq inflace
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0),
    # eq oce
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

res_var_model_h <- restrict(var_model_h, method = "manual", resmat = res_matrix_h)


# FIX: stale hazi tu stejnou chybu jak ve stack overflow
# vcovTEST <- lapply(var_model_h$varresult, function(eq) vcovHAC(eq))

summary(var_model_h)


# prepoklady
# TODO: dodelat asi klasicke predpoklady jak u klasicke regrese


# diagnostika modelu
residuals <- residuals(res_var_model_h)

serial.test(res_var_model_h) # Autokorelace
arch.test(res_var_model_h) # Heteroskedasticita
normality.test(res_var_model_h)

# KPSS test
kpss_results <- apply(residuals, 2, kpss.test)
# ADF test
adf_results <- apply(residuals, 2, adf.test)
# PP test
pp_results <- apply(residuals, 2, pp.test)
# Print the results
print(kpss_results)
print(adf_results)
print(pp_results)


# IRF
# FIX: impulse musi byt endogenni??

# CUSTOM IRF pro exogenni FG na oce_h
var_count <- 5
max_var_lag <- 3
horizon <- 20

all_coef <- coef(res_var_model_h)
B_exog <- coef(res_var_model_h)$oce_h["fg_u", ] # Dopad exogenní proměnné na Y1
C_exog <- coef(res_var_model_h)$oce_h["fg_z", ] # Dopad exogenní proměnné na Y2

irf_exog_fg_u <- tibble(
    "aktiva_scl" = 0,
    "nezam" = 0,
    "urok" = 0,
    "inflace" = 0,
    "oce_h" = 0,
    .rows = horizon + max_var_lag
)

# Počáteční šok v první periodě (o 1 jednotku)
irf_exog_fg_u[max_var_lag + 1, 5] <- B_exog[["Estimate"]]

# Simulace dopadu šoku v dalších periodách
for (t in max_var_lag + 2:horizon + max_var_lag) {
    for (k in 1:var_count) {
        irf_exog_fg_u[t, k] <- all_coef[[k]][, 1][1] * irf_exog_fg_u[t - 1, 1] +
            all_coef[[k]][, 1][2] * irf_exog_fg_u[t - 1, 2] +
            all_coef[[k]][, 1][3] * irf_exog_fg_u[t - 1, 3] +
            all_coef[[k]][, 1][4] * irf_exog_fg_u[t - 1, 4] +
            all_coef[[k]][, 1][5] * irf_exog_fg_u[t - 1, 5] +
            all_coef[[k]][, 1][6] * irf_exog_fg_u[t - 2, 1] +
            all_coef[[k]][, 1][7] * irf_exog_fg_u[t - 2, 2] +
            all_coef[[k]][, 1][8] * irf_exog_fg_u[t - 2, 3] +
            all_coef[[k]][, 1][9] * irf_exog_fg_u[t - 2, 4] +
            all_coef[[k]][, 1][10] * irf_exog_fg_u[t - 2, 5] +
            all_coef[[k]][, 1][11] * irf_exog_fg_u[t - 3, 1] +
            all_coef[[k]][, 1][12] * irf_exog_fg_u[t - 3, 2] +
            all_coef[[k]][, 1][13] * irf_exog_fg_u[t - 3, 3] +
            all_coef[[k]][, 1][14] * irf_exog_fg_u[t - 3, 4] +
            all_coef[[k]][, 1][15] * irf_exog_fg_u[t - 3, 5]
    }
}

# Plot IRF
irf_exog_fg_u$time <- 1:horizon
irf_exog_fg_u %>%
  pivot_longer(cols = -time, names_to = "Variable", values_to = "Response") %>%
  ggplot(aes(x = time, y = Response, color = Variable)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(title = "Impulzní odezva na šok v exogenní proměnné (restrikovaný VAR)",
       x = "Časový horizont",
       y = "Odezva")



oce_h_akt_irf <- irf(
    var_model_h,

    # NOTE: swap za aktiva kdyztak
    impulse = "aktiva_scl",
    response = "oce_h",
    n.ahead = 20,
    ortho = FALSE,
    runs = 100
)

plot(oce_h_akt_irf)


# Granger causality
# NOTE: swap za aktiva kdyztak
causality(var_model_h, cause = "aktiva_scl")

# FIX: zase hazi chybu asi - exogenni
# causality(var_model_h, cause = "fg_z")
# causality(var_model_h, cause = "fg_u")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp <- fevd(var_model_h, n.ahead = 20)
plot(v_decomp)





# PROFESIONALOVE ==============
trans_tdata_p <- trans_tdata |>
    filter(datum < "2022-01") |>
    dplyr::select(
        -datum,
        -oce_h,

        # NOTE: swap za aktiva kdyztak
        -aktiva
    )

# vyber zpozdeni
lag_optimal <- trans_tdata_p |>
    dplyr::select(
        -forward_guidance_uvolneni,
        -forward_guidance_zprisneni,
    ) |>
    VARselect(
        lag.max = 20,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata_h$forward_guidance_uvolneni,
            fg_z = trans_tdata_h$forward_guidance_zprisneni
        )
    )
lag_optimal

# odhad
var_model_p <- trans_tdata_h |>
    dplyr::select(
        -forward_guidance_uvolneni,
        -forward_guidance_zprisneni,
    ) |>
    vars::VAR(
        p = 14,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata_h$forward_guidance_uvolneni,
            fg_z = trans_tdata_h$forward_guidance_zprisneni
        )
    )

# NOTE: restrikce: FG se objevuje pouze v rovnici pro exp
res_matrix_p <- rbind(
    # eq akriva_scl
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0),
    # eq nezam
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0),
    # eq urok
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0),
    # eq inflace
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0),
    # eq oce
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

res_var_model_p <- restrict(var_model_p, method = "manual", resmat = res_matrix_p)


summary(var_model_p)

# prepoklady
# TODO: dodelat asi klasicke predpoklady jak u klasicke regrese


# diagnostika modelu
residuals <- residuals(var_model_p)

serial.test(var_model_p)
arch.test(var_model_p)
normality.test(var_model_p)

# KPSS test
kpss_results <- apply(residuals, 2, kpss.test)
# ADF test
adf_results <- apply(residuals, 2, adf.test)
# PP test
pp_results <- apply(residuals, 2, pp.test)
# Print the results
print(kpss_results)
print(adf_results)
print(pp_results)


# IRF
# FIX: impulse musi byt endogenni??
# oce_p_fg_z_irf <- irf(var_model_p,
#     impulse = "fg_z", response = "oce_p",
#     n.ahead = 20, ortho = FALSE
# )
#
# plot(oce_p_fg_z_irf)

oce_p_akt_irf <- irf(
    var_model_p,

    # NOTE: swap za aktiva kdyztak
    impulse = "aktiva_scl",
    response = "oce_p",
    n.ahead = 20,
    ortho = FALSE,
    runs = 100
)

plot(oce_p_akt_irf)


# Granger causality
# NOTE: swap za aktiva kdyztak
causality(var_model_p, cause = "aktiva_scl")

# FIX: zase hazi chybu asi - exogenni
# causality(var_model_p, cause = "fg_z")
# causality(var_model_p, cause = "fg_u")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp <- fevd(var_model_p, n.ahead = 20)
plot(v_decomp)
