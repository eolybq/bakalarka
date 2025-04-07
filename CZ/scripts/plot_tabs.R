rm(list = ls())

library(tidyverse)


load("data/tibble_data.RData")
load("data/ts_data.RData")

tibble_data <- tibble_data |>
    mutate(date = as.Date(paste0(date, "-01")))


# GRAF vsechny promenne -jejich cely vzorek ============
ts_to_df <- function(ts_obj) {
    start_year <- start(ts_obj)[1]
    start_period <- start(ts_obj)[2]
    freq <- frequency(ts_obj)
    n <- length(ts_obj)

    start_date <- as.Date(paste0(start_year, "-", sprintf("%02d", start_period), "-01"))
    time_seq <- seq(from = start_date, by = "month", length.out = n)

    tibble(time = time_seq, value = as.numeric(ts_obj))
}

# Loop over each time series, convert to a tibble, and generate a ggplot graph
for (ts_name in names(ts_objects)) {
    df <- ts_to_df(ts_objects[[ts_name]])
    p <- ggplot(df, aes(x = time, y = value)) +
        geom_line(color = "steelblue", linewidth = 1) +
        labs(
            title = paste("Time Series Plot:", ts_name),
            x = "Time",
            y = "Value"
        ) +
        theme_minimal()

    print(p)
}



# GRAF VLIV FG a AKTIVA na OCE ==========
tibble_data_vliv <- tibble_data |>
    dplyr::select(
        -c(cpi, ipi, exp_h, exp_p, fx_res)
    )

tibble_data_vliv$fx_res_abs <- tibble_data_vliv$fx_res_abs / 1000000

# Převod do dlouhého formátu vhodného pro ggplot2
tibble_data_long1 <- tibble_data_vliv |>
    pivot_longer(cols = -date, names_to = "serie", values_to = "hodnota")

# Odstraníme FG řady pro čárový graf
data_lines <- tibble_data_long1 |>
    filter(!(serie %in% c("fg_u", "fg_z")))

# Vypočteme intervaly pro FG_uvolneni
fg_u_intervals <- tibble_data |>
    filter(fg_u == 1) |>
    arrange(date) |>
    mutate(
        gap = as.numeric(difftime(date, lag(date, default = first(date)), units = "days")),
        new_interval = if_else(is.na(gap) | gap > 35, 1, 0),
        interval_id = cumsum(new_interval)
    ) |>
    group_by(interval_id) |>
    summarise(start = min(date), end = max(date) + days(30)) |>
    ungroup()

# Vypočteme intervaly pro FG_zprisneni
fg_z_intervals <- tibble_data |>
    filter(fg_z == 1) |>
    arrange(date) |>
    mutate(
        gap = as.numeric(difftime(date, lag(date, default = first(date)), units = "days")),
        new_interval = if_else(is.na(gap) | gap > 35, 1, 0),
        interval_id = cumsum(new_interval)
    ) |>
    group_by(interval_id) |>
    summarise(start = min(date), end = max(date) + days(30)) |>
    ungroup()

ggplot() +
    geom_rect(
        data = fg_u_intervals, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
        fill = "green", alpha = 0.3
    ) +
    geom_rect(
        data = fg_z_intervals, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
        fill = "red", alpha = 0.2
    ) +
    geom_line(data = data_lines, aes(x = date, y = hodnota, color = serie)) +

    # Přejmenování názvů proměnných v legendě (přizpůsob si podle aktuálních názvů)
    scale_color_discrete(labels = c(
        "fx_res_abs" = "Devizové rezervy",
        "ir" = "Úrokové sazby"
    )) +

    # Nastavení osy x tak, aby se zobrazovala data každý měsíc
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(
        name = "Procento", 
        sec.axis = sec_axis(
            name = "Mil. Kč",  # Pravá osa pro druhou proměnnou
            trans = ~ . * 1000000
        )
    ) +
    labs(
        x = "Rok", y = "Procento", color = "Řada"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

