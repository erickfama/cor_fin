### ROC curves ###

# Librerias ----
library(tidyverse)
library(fst)
source("./src/3_ROC_function.R")

# Egresos ----

## Lectura ----
eg_inc <- read_fst("./data/3_final/egresos19_inc_clean.fst")

## ROC ----
roc_eg_inc <- ROC(eg_inc, method_name = "Incidencia", props_values = seq(0, 20, length.out = 21))
roc_eg <- rbind(roc_eg_inc)

## Escritura ----
write_csv(roc_eg, "./data/2_interim/roc_eg.csv")

## plot ----
roc_eg <- read_csv("./data/2_interim/roc_eg.csv", locale = locale(encoding = "UTF-8"))

roc_eg %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_point() +
  geom_line() +
  ggrepel::geom_text_repel(aes(label = round(prop, 2)), show.legend = FALSE) +
  scale_x_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_discrete(name = "Modelo") +
  theme_bw() +
  labs(title = "Curvas ROC",
       subtitle = "Egresos",
       caption = "Elaboración propia.")

# Ingresos ----

## Lectura ----
ig_inc <- read_fst("./data/3_final/ingresos19_inc_clean.fst")

## ROC ----
roc_ig_inc <- ROC(ig_inc, method_name = "Incidencia", props_values = seq(0, 20, length.out = 21))
roc_ig <- rbind(roc_ig_inc)

## Escritura ----
write_csv(roc_ig, "./data/2_interim/roc_ig.csv")

## plot ----
roc_ig <- read_csv("./data/2_interim/roc_ig.csv", locale = locale(encoding = "UTF-8"))

roc_ig %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_point() +
  geom_line() +
  ggrepel::geom_text_repel(aes(label = round(prop, 2)), show.legend = FALSE) +
  scale_x_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_discrete(name = "Modelo") +
  theme_bw() +
  labs(title = "Curvas ROC",
       subtitle = "Ingresos",
       x = "FPR (False Positive Rate)",
       y = "TPR (True Positive Rate - Sensibilidad)",
       caption = "Elaboración propia.")
