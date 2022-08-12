### Mapas ### 

# Librerias ----
library(tidyverse)
library(ggtext)
# library(hcandersenr)
# library(tidytext)
# library(RColorBrewer)

# Lectura ----

## Egresos ----
eg_per_predicted <- read_csv("./data/3_final/eg_per_predicted.csv") %>%
  rename(corrup_hat_eg_per = corrup_hat, corrup_eg_per = corrup)
eg_inc_predicted <- read_csv("./data/3_final/eg_inc_predicted.csv") %>%
  rename(corrup_hat_eg_inc = corrup_hat, corrup_eg_inc = corrup)

## Ingresos ----
ig_per_predicted <- read_csv("./data/3_final/ig_per_predicted.csv") %>%
  rename(corrup_hat_ig_per = corrup_hat, corrup_ig_per = corrup)
ig_inc_predicted <- read_csv("./data/3_final/ig_inc_predicted.csv") %>%
  rename(corrup_hat_ig_inc = corrup_hat, corrup_ig_inc = corrup)

## Modelos ----
model_eg_per <- readRDS("./models/model_eg_per.rds")
model_eg_inc <- readRDS("./models/model_eg_inc.rds")
model_ig_per <- readRDS("./models/model_ig_per.rds")
model_ig_inc <- readRDS("./models/model_ig_inc.rds")

## Mapa ----
mun_mapa <- sf::read_sf("./data/maps/muni.shp")

# cutoff 
best_cutoff <- function(tipo_fin, method){
  if(tipo_fin == "eg"){
    cutoff <- read_csv("./data/2_interim/eval_eg.csv", locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
      select(method, best_cutoff) 
    if(method == "per"){
      value <- cutoff %>% filter(method == "Percepción") %>% select(best_cutoff) %>% pull() %>% unique() %>% ifelse(is.null(.), 0, .)
    } else {
      value <- cutoff %>% filter(method == "Incidencia") %>% select(best_cutoff) %>% pull() %>% unique() %>% ifelse(is.null(.), 0, .)
    }
  } 
  if(tipo_fin == "ig"){
    cutoff <- read_csv("./data/2_interim/eval_ig.csv", locale = locale(encoding = "latin1"), show_col_types = FALSE) %>%
      select(method, best_cutoff) 
    if(method == "per"){
      value <- cutoff %>% filter(method == "Percepción") %>% select(best_cutoff) %>% pull() %>% unique() %>% ifelse(is.null(.), 0, .)
    } else {
      value <- cutoff %>% filter(method == "Incidencia") %>% select(best_cutoff) %>% pull() %>% unique() %>% ifelse(is.null(.), 0, .)
    }
  }
  value <- ifelse(is.null(value), 0, value)
  return(value)
}

### Merge ----
mun_mapa <- mun_mapa %>%
  rename(mun_inegi = CVEGEO) %>%
  left_join(eg_per_predicted, by = "mun_inegi") %>%
  left_join(eg_inc_predicted, by = "mun_inegi") %>%
  left_join(ig_per_predicted, by = "mun_inegi") %>%
  left_join(ig_inc_predicted, by = "mun_inegi") %>%
  select(mun_inegi:geometry, starts_with("corrup")) %>%
  mutate(across(starts_with("corrup"), ~ ifelse(.x == "corrupto", 1, 0)),
         across(starts_with("corrup"), ~ factor(.x, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))))


# Graficos ----

## Egresos ----

### Percepcion ----

#### Etiquetas reales ----
mun_mapa %>%
  ggplot(aes(fill = corrup_eg_per)) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información"), type = c("#ff6d00", "#0092ff")) +
  labs(title = "Clasificación<b>real</b> de corrupción de los municipios con base en sus <span style='color:#008dff;'>egresos</span>",
       subtitle = glue::glue("Total de municipios encuestados en la ENCIG: {sum(!is.na(eg_per_predicted$corrup_eg_per))}\n
                             Cutoff de proporción: {best_cutoff('eg', 'per')}"),
       caption = "Elaboración propia con datos de la ENCIG 2019.") +
  theme_bw() +
  theme(plot.title = element_markdown())

#### Predichas ----
mun_mapa %>%
  ggplot(aes(fill = factor(corrup_hat_eg_per, levels = c("corrupto", "no_corrupto")))) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información"), type = c("#ff6d00", "#0092ff")) +
  labs(title = "Clasificación<b>predicha</b> de corrupción de los municipios con base en sus <span style='color:#008dff;'>egresos</span>",
       subtitle = glue::glue("<br>Modelo de <b><span style='color:#0092ff;'>percepción</span></b></br>\n
                             Total de municipios modelados: {nrow(eg_per_predicted)}\n
                             Sensibilidad del modelo: {round(model_eg_per$cm$byClass[c('Sensitivity')], 2)} | Especificidad: {round(model_eg_per$cm$byClass[c('Specificity')], 2)}\n
                             Cutoff de proporción: {best_cutoff('eg', 'per')}"),
       caption = "Elaboración propia con base en el modelo de Gradient Boosting.") +
  theme_bw() +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

### Incidencia ----

#### Etiquetas reales ----
mun_mapa %>%
  ggplot(aes(fill = corrup_eg_inc)) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información"), type = c("#ffed00", "#0012ff")) +
  labs(title = "Clasificación<b>real</b> de corrupción de los municipios con base en sus <span style='color:#008dff;'>egresos</span>",
       subtitle = glue::glue("Total de municipios encuestados en la ENCIG: {sum(!is.na(eg_inc_predicted$corrup_eg_inc))}\n
                             Cutoff de proporción: {best_cutoff('eg', 'inc')}"),
       caption = "Elaboración propia con datos de la ENCIG 2019.") +
  theme_bw() +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

#### Etiquetas predichas ----
mun_mapa %>%
  ggplot(aes(fill = factor(corrup_hat_eg_inc, levels = c("corrupto", "no_corrupto")))) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información"), type = c("#ffed00", "#0012ff")) +
  theme_bw() +
  labs(title = "Clasificación<b>predicha</b> de corrupción de los municipios con base en sus <span style='color:#008dff;'>egresos</span>",
       subtitle = glue::glue("Modelo de <b><span style='color:#0012ff;'>incidencia</span></b>\n
                             Total de municipios modelados: {nrow(eg_inc_predicted)}\n
                             Sensibilidad del modelo: {round(model_eg_inc$cm$byClass[c('Sensitivity')], 2)} | Especificidad: {round(model_eg_inc$cm$byClass[c('Specificity')], 2)}\n
                             Cutoff de proporción: {best_cutoff('eg', 'inc')}"),
       caption = "Elaboración propia con base en el modelo de Gradient Boosting.") +
  theme_bw() +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

## Ingresos ----

### Percepcion ----

#### Etiquetas reales ----
mun_mapa %>%
  ggplot(aes(fill = corrup_ig_per)) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información"), type = c("#9c002e", "#009c6e")) +
  labs(title = "Clasificación<b>real</b> de corrupción de los municipios con base en sus <span style='color:#006d16;'>ingresos</span>",
       subtitle = glue::glue("Total de municipios encuestados en la ENCIG: {sum(!is.na(ig_per_predicted$corrup_ig_per))}\n
                             Cutoff de proporción: {best_cutoff('ig', 'per')}"),
       caption = "Elaboración propia con datos de la ENCIG 2019.") +
  theme_bw() +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

#### Predichas ----
mun_mapa %>%
  ggplot(aes(fill = factor(corrup_hat_ig_per, levels = c("corrupto", "no_corrupto")))) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información"), type = c("#9c002e", "#009c6e")) +
  labs(title = "Clasificación predicha de corrupción de los municipios con base en sus <span style='color:#006d16;'>ingresos</span>",
       subtitle = glue::glue("Modelo de <b><span style='color:#4cdd69;'>percepción</span></b>\n
                             Total de municipios modelados: {nrow(ig_per_predicted)}\n
                             Sensibilidad del modelo: {round(model_ig_per$cm$byClass[c('Sensitivity')], 2)} | Especificidad: {round(model_ig_per$cm$byClass[c('Specificity')], 2)}\n
                             Cutoff de proporción: {best_cutoff('ig', 'per')}"),
       caption = "Elaboración propia con base en el modelo de Gradient Boosting.") +
  theme_bw() +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

### Incidencia ----

#### Etiquetas reales ----
mun_mapa %>%
  ggplot(aes(fill = corrup_ig_inc)) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información"), type = c("#9c007c", "#009C20")) +
  labs(title = "Clasificación<b>real</b> de corrupción de los municipios con base en sus <span style='color:#006d16;'>ingresos</span>",
       subtitle = glue::glue("Total de municipios encuestados en la ENCIG: {sum(!is.na(ig_inc_predicted$corrup_ig_inc))}\n
                             Cutoff de proporción: {best_cutoff('ig', 'inc')}"),
       caption = "Elaboración propia con datos de la ENCIG 2019.") +
  theme_bw() +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

#### Etiquetas predichas ----
mun_mapa %>%
  ggplot(aes(fill = factor(corrup_hat_ig_inc, levels = c("corrupto", "no_corrupto")))) +
  geom_sf() +
  scale_fill_discrete(name = "", labels = c("Corrupto", "No corrupto", "Sin información"), type = c("#9c007c", "#009C20")) +
  labs(title = "Clasificación predicha de corrupción de los municipios con base en sus <span style='color:#006d16;'>ingresos</span>",
       subtitle = glue::glue("Modelo de <b><span style='color:#009C20;'>incidencia</span></b>\n
                             Total de municipios modelados: {nrow(ig_inc_predicted)}\n
                             Sensibilidad del modelo: {round(model_ig_inc$cm$byClass[c('Sensitivity')], 2)} | Especificidad: {round(model_ig_inc$cm$byClass[c('Specificity')], 2)}\n
                             Cutoff de proporción: {best_cutoff('ig', 'inc')}"),
       caption = "Elaboración propia con base en el modelo de Gradient Boosting.") +
  theme_bw() +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())


