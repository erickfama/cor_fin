### Mapas ### 

# Librerias ----
library(tidyverse)
library(leaflet)
library(rgdal)
library(sf)
library(RColorBrewer)

# Lectura ----

# Valores reales
egresos_real <- read_csv("./data/3_final/egresos19_mun_clean.csv")
# Valores predichos
egresos_predict <- read_csv("./data/3_final/egresos_predict.csv")
egresos_predict_PERINC <- read_csv("./data/3_final/egresos_predict_PERINC.csv")

# Lectura del mapa
mun_mapa <- sf::read_sf("./data/maps/muni.shp")

# Merge ----

# Se agregan las etiquetas reales y predichas
mun_mapa <- mun_mapa %>% 
  rename(mun_inegi = CVEGEO) %>%
  left_join(select(egresos_real, c("mun_inegi", "corrup")), by = "mun_inegi") %>%
  left_join(select(egresos_predict, c("mun_inegi", "corrup_hat")), by = "mun_inegi") %>%
  left_join(select(egresos_predict_PERINC, c("mun_inegi", "corrup_hat_per", "corrup_hat_inc")), by = "mun_inegi") %>%
  mutate(across(c(corrup_hat, corrup_hat_per, corrup_hat_inc), ~ ifelse(.x == "corrupto", 1, 0)),
         across(c(corrup, corrup_hat, corrup_hat_per, corrup_hat_inc), ~ factor(.x, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))))

# Graficos ----

## Mapa de etiquetas reales ----
mun_mapa %>%
  ggplot(aes(fill = corrup)) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información")) +
  theme_bw() +
  labs(title = bquote("Clasificación"~bold("real")~"de corrupción de los municipios"),
       subtitle = glue::glue("Total de municipios encuestados en la ENCIG: {nrow(egresos_real)}"),
       caption = "Elaboración propia con datos de la ENCIG 2019.")

## Percepcion sin normalizar ----
mun_mapa %>%
  ggplot(aes(fill = corrup_hat)) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información")) +
  theme_bw() +
  labs(title = bquote("Clasificación"~bold("predicha")~"de corrupción de los municipios"),
       subtitle = glue::glue("Total de municipios modelados: {nrow(egresos_predict)}\n
                             Sensibilidad del modelo: {round(cm$byClass[c('Sensitivity')], 2)} | Especificidad: {round(cm$byClass[c('Specificity')], 2)}"),
       caption = "Elaboración propia con base en el modelo de Gradient Boosting.")

## Percepcion ----

mun_mapa %>%
  ggplot(aes(fill = factor(corrup_hat_per, levels = c("corrupto", "no_corrupto")))) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información"), type = c("#FFA600", "#00BFC4")) +
  theme_bw() +
  labs(title = bquote("Clasificación"~bold("predicha")~"de corrupción de los municipios"),
       subtitle = glue::glue("Modelo de percepción\n
                             Total de municipios modelados: {nrow(egresos_predict_PERINC)}\n
                             Sensibilidad del modelo: {round(cm_per$byClass[c('Sensitivity')], 2)} | Especificidad: {round(cm_per$byClass[c('Specificity')], 2)}"),
       caption = "Elaboración propia con base en el modelo de Gradient Boosting.")

## Incidencia ----
mun_mapa %>%
  ggplot(aes(fill = factor(corrup_hat_inc, levels = c("corrupto", "no_corrupto")))) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información"), type = c("#FF0000", "#00BFC4")) +
  theme_bw() +
  labs(title = bquote("Clasificación"~bold("predicha")~"de corrupción de los municipios"),
       subtitle = glue::glue("Modelo de incidencia\n
                             Total de municipios modelados: {nrow(egresos_predict_PERINC)}\n
                             Sensibilidad del modelo: {round(cm_inc$byClass[c('Sensitivity')], 2)} | Especificidad: {round(cm_inc$byClass[c('Specificity')], 2)}"),
       caption = "Elaboración propia con base en el modelo de Gradient Boosting.")
