### ML ###

# Librerias ----
library(tidyverse)
library(caret)
library(xgboost)
source("./src/3_ML_function.R")

# Lectura ----
ingresos_per <- read_csv("./data/3_final/ingresos19_per_clean.csv", locale = locale(encoding = "latin1"))
ingresos_inc <- read_csv("./data/3_final/ingresos19_inc_clean.csv", locale = locale(encoding = "latin1"))

# Modelos ----
model_ig_per <- gb_model(ingresos_per, cutoff = 0, seed = TRUE)
model_ig_inc <- gb_model(ingresos_inc, cutoff = 4, seed = TRUE)

