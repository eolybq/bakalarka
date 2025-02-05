rm(list = ls())

library(tidyverse)


load("data/tibble_data.RData")

tibble_data <- tibble_data |>
  mutate(datum = as.Date(paste0(datum, "-01")))


# GRAF VLIV FG a AKTIVA na OCE ==========
tibble_data_vliv <- tibble_data |>
  mutate(
    aktiva = c(NA, diff(log(aktiva)) * 100),
    # inflace_monthy_change = c(NA, inflace / stats::lag(inflace) * 100)
  ) |>
  select(
    -c(inflace, nezamestnanost, urok)
  )




# Převod do dlouhého formátu vhodného pro ggplot2
tibble_data_long1 <- tibble_data_vliv |>
  pivot_longer(cols = -datum, names_to = "serie", values_to = "hodnota")




# Odstraníme FG řady pro čárový graf
data_lines <- tibble_data_long1 |>
  filter(!(serie %in% c("forward_guidance_uvolneni", "forward_guidance_zprisneni")))

# Vypočteme intervaly pro FG_uvolneni
fg_u_intervals <- tibble_data |>
  filter(forward_guidance_uvolneni == 1) |>
  arrange(datum) |>

  mutate(gap = as.numeric(difftime(datum, lag(datum, default = first(datum)), units = "days")),
         new_interval = if_else(is.na(gap) | gap > 35, 1, 0),
         interval_id = cumsum(new_interval)) |>
  group_by(interval_id) |>
  summarise(start = min(datum), end = max(datum) + days(30)) |>
  ungroup()

# Vypočteme intervaly pro FG_zprisneni
fg_z_intervals <- tibble_data |>
  filter(forward_guidance_zprisneni == 1) |>
  arrange(datum) |>
  mutate(gap = as.numeric(difftime(datum, lag(datum, default = first(datum)), units = "days")),
         new_interval = if_else(is.na(gap) | gap > 35, 1, 0),
         interval_id = cumsum(new_interval)) |>
  group_by(interval_id) |>
  summarise(start = min(datum), end = max(datum) + days(30)) |>
  ungroup()

ggplot() +
  geom_rect(data = fg_u_intervals, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.3) +
  geom_rect(data = fg_z_intervals, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +
  geom_line(data = data_lines, aes(x = datum, y = hodnota, color = serie)) +
  
  # Přejmenování názvů proměnných v legendě (přizpůsob si podle aktuálních názvů)
  scale_color_discrete(labels = c(
    "aktiva" = "Aktiva", 
    "oce_h" = "I. očekávání - domácnosti", 
    "oce_p" = "I. očekávání - profesionálové"
  )) +

  # Nastavení osy x tak, aby se zobrazovala data každý měsíc
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Časové řady s indikací FG uvolnění (oranžová) a FG zpřísnění (červená)",
       x = "Datum", y = "Hodnota", color = "Řada") +
  theme_minimal() +
  theme(legend.position = "bottom")
