### Deployment ###

# Librerias ----
library(tidyverse)
library(caret)

# Percepcion sin normalizar ----
egresos_predict <- read_csv("./data/2_interim/efipem19_clean.csv") %>%
  filter(tema == "egresos") %>% 
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, 
              values_from = valor, 
              values_fn = list(valor = sum), 
              values_fill = 0)

# Predict
corrup_hat <- model_xgbTree %>% 
  predict(egresos_predict %>% select(starts_with("partida")))

egresos_predict$corrup_hat <- corrup_hat

# Percepcion ----
egresos_predict_PERINC <- read_csv("./data/2_interim/efipemCor19_per_raw.csv") %>%
  filter(tema == "egresos") %>%
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, 
              values_from = valor, 
              values_fn = list(valor = sum), 
              values_fill = 0)

# Predict
corrup_hat_per <- model_xgbTree_per %>% 
  predict(egresos_predict_PERINC %>% select(starts_with("partida"), pobtot, graproes))

egresos_predict_PERINC$corrup_hat_per <- corrup_hat_per

# Incidencia ----

# Predict
corrup_hat_inc <- model_xgbTree_inc %>% 
  predict(egresos_predict_PERINC %>% select(starts_with("partida"), pobtot, graproes))

egresos_predict_PERINC$corrup_hat_inc <- corrup_hat_inc


# Escritura ----
write_csv(egresos_predict, "./data/3_final/egresos_predict.csv")
write_csv(egresos_predict_PERINC, "./data/3_final/egresos_predict_PERINC.csv")
