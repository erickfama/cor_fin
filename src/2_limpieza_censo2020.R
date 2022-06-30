### Limpieza Censo ###

# Librerias ----
library(tidyverse)

# Lectura ----
censo2020_raw <- read_csv("./data/1_raw/conjunto_de_datos_iter_00CSV20.csv") %>%
  janitor::clean_names()


# Agregado por municipio ----
censo2020_mun <- censo2020_raw %>%
  unite(col = "mun_inegi", entidad, mun, sep = "") %>% 
  filter(mun_inegi != "00000" & nom_loc == "Total del Municipio")

# Escritura censo por municipio ----
write.csv(censo2020_mun, "./data/2_interim/censo2020_mun_clean.csv")
