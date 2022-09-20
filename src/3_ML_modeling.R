### ML ###

# Librerias ----
library(tidyverse)
library(fst)
library(caret)
library(xgboost)
source("./src/3_ML_function.R", encoding = "UTF-8")

# Lectura ----
eg_inc <- read_fst("./data/3_final/egresos19_inc_clean.fst")

ig_inc <- read_fst("./data/3_final/ingresos19_inc_clean.fst")

# Egresos ----

## incidencia ----
model_eg_inc <- gb_model(eg_inc, cutoff = best_cutoff("eg", "inc"))

# Guardado
saveRDS(model_eg_inc, file = "./models/model_eg_inc.rds")

# Ingresos ----

## Incidencia ----
model_ig_inc <- gb_model(ig_inc, cutoff = best_cutoff("ig", "inc"), seed = TRUE)
 
# Guardado
saveRDS(model_ig_inc, file = "./models/model_ig_inc.rds")

