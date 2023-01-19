### Limpieza Efipem 18-21 ###

# Librerias ----
library(tidyverse)
library(stringi)

# Lectura ----
efipem_raw <- read_csv("./data/1_raw/efipem_raw.csv")
censo <- read_csv("./data/2_interim/censo2020_mun_clean.csv")

# Limpieza ----
efipem_clean <- efipem_raw %>%
  mutate(mun_inegi = paste(id_entidad, id_municipio, sep = ""),
         across(c("tema", "categoria", "descripcion_categoria"), ~ stri_trans_general(str_replace_all(str_replace_all(str_to_lower(.x), "\\s", "_"), ",", ""), id = "Latin-ASCII"))) %>%
  select(mun_inegi, everything(), -c("prod_est", "cobertura", "estatus")) %>%
  left_join(censo %>% select(mun_inegi, pobtot), by = "mun_inegi")

# Escritura ----
write_csv(efipem_clean, "./data/2_interim/efipem_clean.csv")

rm(efipem_raw, censo)
