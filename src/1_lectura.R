### Lectura ###

# Librerias ----
library(tidyverse)

# Lectura ----

## Encig 2019 ----
encig19_raw <- read_csv("./data/1_raw/conjunto_de_datos_encig2019_01_sec1_3_4_5_8_9_10.csv") %>%
  janitor::clean_names()

## Ingresos y egresos muni 2019 ----
efipem19_raw <- read_csv("./data/1_raw/efipem_municipal_anual_tr_cifra_2019.csv") %>% 
  janitor::clean_names()

# Escritura ----
write_csv(encig19_raw, "./data/1_raw/encig19_raw.csv")
write_csv(efipem19_raw, "./data/1_raw/efipem19_raw.csv")


