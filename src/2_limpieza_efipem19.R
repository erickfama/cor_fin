### Limpieza efipem 2019 ###

# Librerias ----
library(tidyverse)
library(stringi)

# Lectura ----
efipem19_raw <- read_csv("./data/1_raw/efipem19_raw.csv")

# Limpieza ----
efipem19_clean <- efipem19_raw %>%
  mutate(mun_inegi = paste(id_entidad, id_municipio, sep = ""),
         across(c("tema", "categoria", "descripcion_categoria"), ~ stri_trans_general(str_replace_all(str_replace_all(str_to_lower(.x), "\\s", "_"), ",", ""), id = "Latin-ASCII"))) %>%
  select(-c("prod_est", "cobertura", "anio", "estatus"))

# Escritura ----
write_csv(efipem19_clean, "./data/2_interim/efipem19_clean.csv")
