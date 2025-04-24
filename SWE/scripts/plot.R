rm(list = ls())

library(tidyverse)


load("data/tibble_data.RData")
load("data/ts_data.RData")

tibble_data <- tibble_data |>
    mutate(date = as.Date(paste0(date, "-01")))

swe_tibble <- tibble_data

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
plot_data <- tibble(
    date = seq(as.Date("1999-09-01"), as.Date("2025-02-01"), "month"),
    sec_abs = window(ts_objects$sec_abs, start = c(1999, 9), end = c(2025,2)),
    ir = window(ts_objects$ir_ts, start = c(1999, 9), end = c(2025, 2))
)


plot_data$sec_abs <- plot_data$sec_abs / 100000

# Převod do dlouhého formátu vhodného pro ggplot2
tibble_data_long1 <- plot_data |>
    pivot_longer(cols = -date, names_to = "serie", values_to = "hodnota")

# Odstraníme FG řady pro čárový graf
data_lines <- tibble_data_long1 |>
    filter(!(serie %in% c("fg_u", "fg_z")))



ggplot() +
    geom_line(data = data_lines, aes(x = date, y = hodnota, color = serie)) +
    
    scale_color_manual(
        labels = c(
        "sec_abs" = "Držené tuzemské cenné papíry",
        "ir" = "Policy rate"
        ), 
        values = c(
            "ir" = "steelblue",      
            "sec_abs" = "darkorange"
        ),
    ) +
    
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(
        name = "Procento", 
        breaks = scales::pretty_breaks(),
        # labels = scales::number_format(accuracy = 1),
        sec.axis = sec_axis(
            name = "Mld. SEK",
            trans = ~ . * 100
        )
    ) +
    labs(
        x = "Rok", y = "Procento", color = "Řada"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")


# GRAF oce_m SWE + EA
swe_exp_m <- ts_objects$exp_m_ts
load("../EA/data/ts_data.RData")
ea_exp_m <- ts_objects$exp_m_ts

plot_data_exp <- tibble(
    date = seq(as.Date("2011-01-01"), as.Date("2024-09-01"), "month"),
    Švédsko = c(swe_exp_m, rep(NA, 17)),
    Eurozóna = c(rep(NA, 54), window(ea_exp_m, end = c(2024, 9)))
) |>
    pivot_longer(cols = -date) |>
    drop_na()

ggplot(plot_data_exp, aes(x = date, y = value)) +
    geom_line() +
    facet_wrap(~ name, scales = "free_x") + 
    # scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
        x = "Rok",
        y = "Procento"
    ) +
    theme_minimal() +
    theme(
        strip.text = element_text(face = "bold", size = 14)
    )



# GRAF vsechny IE
load("../EA/data/tibble_data.RData")
tibble_data <- tibble_data |>
    mutate(date = as.Date(paste0(date, "-01")))
ea_tibble <- tibble_data

load("../CZ/data/tibble_data.RData")
tibble_data <- tibble_data |>
    mutate(date = as.Date(paste0(date, "-01")))
cz_tibble <- tibble_data


ie_plot_data <- swe_tibble |>
    dplyr::select(date, exp_h, exp_p)
ie_plot_data$country <- rep("Švédsko", nrow(swe_tibble))
ie_plot_data <- ie_plot_data |> 
    pivot_longer(cols = -c(date, country))

ie_plot_data2 <- ea_tibble |>
    dplyr::select(date, exp_h)
ie_plot_data2$country <- rep("Eurozóna", nrow(ea_tibble))
ie_plot_data2 <- ie_plot_data2 |> 
    pivot_longer(cols = -c(date, country))

ie_plot_data3 <- cz_tibble |>
    dplyr::select(date, exp_h, exp_p)
ie_plot_data3$country <- rep("ČR", nrow(cz_tibble))
ie_plot_data3 <- ie_plot_data3 |> 
    pivot_longer(cols = -c(date, country))
    

ie_plot_data_final <- bind_rows(ie_plot_data, ie_plot_data2, ie_plot_data3)

ggplot() +
    facet_wrap(~ country, scales = "free_x") + 
    geom_line(data = ie_plot_data_final, aes(x = date, y = value, color = name)) +
    scale_color_discrete(labels = c(
        "exp_h" = "Domácnosti",
        "exp_p" = "Profesionálové"
    )) +
    labs(
        x = "Rok", y = "Procento", color = "Řada"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    theme(
        strip.text = element_text(face = "bold", size = 14)
    )
