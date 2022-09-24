### ML ###

# Librerias ----
library(tidyverse)
library(fst)
library(caret)
library(xgboost)
source("./src/3_ML_function.R", encoding = "UTF-8")

# Lectura ----
eg_inc <- read_fst("./data/3_final/eg_inc_all_clean.fst") %>%
  filter(!is.na(prop_corrup)) # IMPORTANTE para entrenar

ig_inc <- read_fst("./data/3_final/ig_inc_all_clean.fst") %>%
  filter(!is.na(prop_corrup))

# Egresos ----

## incidencia ----
model_eg_inc <- gb_model(eg_inc, cutoff = best_cutoff("eg", "inc"), seed = TRUE)

# Guardado
saveRDS(model_eg_inc, file = "./models/model_eg_inc.rds")

# Ingresos ----

## Incidencia ----
model_ig_inc <- gb_model(ig_inc, cutoff = best_cutoff("ig", "inc"), seed = TRUE)
 
# Guardado
saveRDS(model_ig_inc, file = "./models/model_ig_inc.rds")

