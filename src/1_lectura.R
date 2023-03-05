### Lectura ###

# Librerias ----
library(tidyverse)

# Lectura ----

## Encig 2017 ---- 
encig17_raw <- read.csv("./data/1_raw/encig2017_01_sec1_3_4_5_8_9_10.csv") %>%
  janitor::clean_names()

## Encig 2019 ----
encig19_raw <- read_csv("./data/1_raw/conjunto_de_datos_encig2019_01_sec1_3_4_5_8_9_10.csv") %>%
  janitor::clean_names()

## Encig 2021 ----
encig21_raw <- read.csv("./data/1_raw/conjunto_de_datos_encig2021_01_sec1_A_3_4_5_8_9_10.csv") %>%
  janitor::clean_names()

## Ingresos y egresos muni 2018, 2019, 2020, 2021 ----
efipem18_raw <- read_csv("./data/1_raw/efipem_municipal_anual_tr_cifra_2018.csv") %>%
  janitor::clean_names()

efipem19_raw <- read_csv("./data/1_raw/efipem_municipal_anual_tr_cifra_2019.csv") %>% 
  janitor::clean_names()

efipem20_raw <- read_csv("./data/1_raw/efipem_municipal_anual_tr_cifra_2020.csv") %>% 
  janitor::clean_names()

efipem21_raw <- read_csv("./data/1_raw/efipem_municipal_anual_tr_cifra_2021.csv") %>% 
  janitor::clean_names()

efipem_raw <- bind_rows(efipem18_raw, efipem19_raw, efipem20_raw, efipem21_raw)

# Escritura ----
write.csv(encig17_raw, "./data/1_raw/encig17_raw.csv", row.names = FALSE)
write_csv(encig19_raw, "./data/1_raw/encig19_raw.csv")
write.csv(encig21_raw, "./data/1_raw/encig21_raw.csv", row.names = FALSE)
write_csv(efipem19_raw, "./data/1_raw/efipem19_raw.csv")
write_csv(efipem_raw, "./data/1_raw/efipem_raw.csv")

