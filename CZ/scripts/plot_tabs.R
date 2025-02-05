rm(list = ls())

library(tidyverse)


load("data/tibble_data.RData")


# TODO: presunout do plot_tabs
tibble_data_plot <- tibble_data %>%
  mutate(datum = as.Date(paste0(datum, "-01")))

# Transformace dat do "dlouhého" formátu pro ggplot2:
tibble_data_long <- tibble_data_plot %>%
  pivot_longer(cols = -datum, names_to = "serie", values_to = "hodnota")

# Varianta 1: Vykreslení všech řad na jednom grafu.
ggplot(tibble_data_long, aes(x = datum, y = hodnota, color = serie)) +
  geom_line() +
  labs(title = "Časové řady", x = "Datum", y = "Hodnota") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Varianta 2: Vykreslení řad pomocí facet_wrap (každá řada v samostatném panelu).
ggplot(tibble_data_long, aes(x = datum, y = hodnota)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ serie, scales = "free_y") +
  labs(title = "Časové řady (facety)", x = "Datum", y = "Hodnota") +
  theme_minimal()
