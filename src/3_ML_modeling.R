### ML ###

# Librerias ----
library(tidyverse)
library(fst)
library(caret)
library(xgboost)
source("./src/3_ML_function.R")

# Lectura ----
eg_per <- read_fst("./data/3_final/egresos19_per_clean.fst")
eg_inc <- read_fst("./data/3_final/egresos19_inc_clean.fst")

ig_per <- read_fst("./data/3_final/ingresos19_per_clean.fst")
ig_inc <- read_fst("./data/3_final/ingresos19_inc_clean.fst")

# Egresos ----

## Percepcion ----
model_eg_per <- gb_model(eg_per, cutoff = best_cutoff("eg", "per"))

## incidencia ----
model_eg_inc <- gb_model(eg_inc, cutoff = best_cutoff("eg", "inc"))

# Ingresos ----

## Percepcion ----
model_ig_per <- gb_model(ig_per, cutoff = best_cutoff("ig", "per"), seed = TRUE)

## Incidencia ----
model_ig_inc <- gb_model(ig_inc, cutoff = best_cutoff("ig", "inc"), seed = TRUE)

