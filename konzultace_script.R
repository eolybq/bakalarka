rm(list = ls())

library(tseries)
library(forecast)
library(tidyverse)
library(urca)
library(vars)


load("tibble_data.RData")


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


diffed_unemp <- diff(tibble_data[["nezam"]], lag = 1)
autoplot(diffed_unemp)
acf_pacf(diffed_unemp, 37)

# NOTE: Je toto potřeba?
s2diffed_unemp <- diff(diffed_unemp, lag = 12)
autoplot(s2diffed_unemp)
acf_pacf(s2diffed_unemp, 37)

check_stationarity(tibble(first_diff = diffed_unemp))
check_stationarity(tibble(s_diff = s2diffed_unemp))


# TODO: Johansen cointegration test



trans_tdata <- tibble_data |>
    mutate(
        aktiva = c(NA, diff(log(aktiva), lag = 1)),
        aktiva_scaled = c(NA,NA, diff(diff(log(aktiva_scaled), lag = 1)*100)),
        nezam = c(NA,NA, diff(diff(nezam, lag = 1))),
        urok = c(NA,NA, diff(diff(urok, lag = 1))),
        inflace = c(NA,NA, diff(diff(log(inflace), lag = 1))*100),
        oce_p = c(NA,NA, diff(diff(oce_p, lag = 1))),
        oce_h = c(NA, diff(oce_h, lag = 1))
    ) |>
    dplyr::select(
        -datum,
    ) |>
    drop_na()

variables <- trans_tdata
check_stationarity(variables)



# ========================================================
# Model
# ========================================================

# DOMACNOSTI ==============
trans_tdata_h <- trans_tdata |>
    dplyr::select(
        -oce_p,
        -forward_guidance_uvolneni,
        -forward_guidance_zprisneni,

        # NOTE: swap za aktiva kdyztak
        -aktiva
    )

# vyber zpozdeni
lag_optimal <- VARselect(
    trans_tdata_h,
    lag.max = 20,
    type = "const",
    exogen = tibble(
        fg_u = trans_tdata$forward_guidance_uvolneni,
        fg_z = trans_tdata$forward_guidance_zprisneni
    )
)
lag_optimal

# odhad
var_model_domac <- trans_tdata_h |>
    VAR(
        p = 2,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata$forward_guidance_uvolneni,
            fg_z = trans_tdata$forward_guidance_zprisneni
        )
    )

summary(var_model_domac)


# prepoklady
# TODO: dodelat asi klasicke predpoklady jak u klasicke regrese


# diagnostika modelu
residuals <- residuals(var_model_domac)

serial.test(var_model_domac)
arch.test(var_model_domac)
normality.test(var_model_domac)

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
# oce_h_fg_z_irf <- irf(var_model_domac,
#     impulse = "fg_z", response = "oce_h",
#     n.ahead = 20, ortho = FALSE
# )
#
# plot(oce_h_fg_z_irf)

oce_h_akt_irf <- irf(
    var_model_domac,

    # NOTE: swap za aktiva kdyztak
    impulse = "aktiva_scaled",

    response = "oce_h",
    n.ahead = 20,
    ortho = FALSE,
    runs = 100
)

plot(oce_h_akt_irf)


# Granger causality
# NOTE: swap za aktiva kdyztak
causality(var_model_domac, cause = "aktiva_scaled")

# FIX: zase hazi chybu asi - exogenni
# causality(var_model_domac, cause = "fg_z")
# causality(var_model_domac, cause = "fg_u")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp <- fevd(var_model_domac, n.ahead = 20)
plot(v_decomp)





# PROFESIONALOVE ==============
trans_tdata_p <- trans_tdata |>
    dplyr::select(
        -oce_h,
        -forward_guidance_uvolneni,
        -forward_guidance_zprisneni,

        # NOTE: swap za aktiva kdyztak
        -aktiva
    )

# vyber zpozdeni
lag_optimal <- VARselect(
    trans_tdata_p,
    lag.max = 20,
    type = "const",
    exogen = tibble(
        fg_u = trans_tdata$forward_guidance_uvolneni,
        fg_z = trans_tdata$forward_guidance_zprisneni
    )
)
lag_optimal

# odhad
var_model_prof <- trans_tdata_p |>
    VAR(
        p = 14,
        type = "const",
        exogen = tibble(
            fg_u = trans_tdata$forward_guidance_uvolneni,
            fg_z = trans_tdata$forward_guidance_zprisneni
        )
    )

summary(var_model_prof)


# prepoklady
# TODO: dodelat asi klasicke predpoklady jak u klasicke regrese


# diagnostika modelu
residuals <- residuals(var_model_prof)

serial.test(var_model_prof)
arch.test(var_model_prof)
normality.test(var_model_prof)

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
# oce_p_fg_z_irf <- irf(var_model_prof,
#     impulse = "fg_z", response = "oce_p",
#     n.ahead = 20, ortho = FALSE
# )
#
# plot(oce_p_fg_z_irf)

oce_p_akt_irf <- irf(
    var_model_prof,

    # NOTE: swap za aktiva kdyztak
    impulse = "aktiva_scaled",

    response = "oce_p",
    n.ahead = 20,
    ortho = FALSE,
    runs = 100
)

plot(oce_p_akt_irf)


# Granger causality
# NOTE: swap za aktiva kdyztak
causality(var_model_domac, cause = "aktiva_scaled")

# FIX: zase hazi chybu asi - exogenni
# causality(var_model_prof, cause = "fg_z")
# causality(var_model_prof, cause = "fg_u")


# Variancni dekompozice - jak moc je promenna ovlivnena sokem ostatnich promennych
v_decomp <- fevd(var_model_prof, n.ahead = 20)
plot(v_decomp)
