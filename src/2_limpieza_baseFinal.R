### Limpieza base de datos final ###

# Librerias ----
library(tidyverse)
library(stringi)

# Lectura ----

## Encig 
encig19_mun_clean <- read_csv("./data/2_interim/encig19_mun_clean.csv")

## Efipem 
efipem19_clean <- read_csv("./data/2_interim/efipem19_clean.csv")

# Merge ----
efipemCor19_raw <- efipem19_clean %>%
  left_join(encig19_mun_clean, by = "mun_inegi")

# Limpieza ----
efipemCor19_clean <- efipemCor19_raw %>%
  filter(!is.na(prop_corrup)) %>%
  select(mun_inegi, nom_ent, nom_mun, prop_corrup, tema, categoria, descripcion_categoria, valor) %>%
  mutate(across(c("tema", "categoria", "descripcion_categoria"), ~ stri_trans_general(str_replace_all(str_replace_all(str_to_lower(.x), "\\s", "_"), ",", ""), id = "Latin-ASCII")),
         corrup = ifelse(prop_corrup >= mean(prop_corrup), 1, 0)) # Dummy de corrupcion

# Escritura base completa ----
write_csv(efipemCor19_raw, "./data/2_interim/efipemCor19_raw.csv")
write_csv(efipemCor19_clean, "./data/3_final/efipemCor19_clean.csv")

# Base Egresos ----
egresos19_mun_clean <- efipemCor19_clean %>%
  filter(tema == "egresos") %>% 
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, values_from = valor, values_fn = list(valor = sum), values_fill = 0)

# Base Ingresos ----
ingresos19_mun_clean <- efipemCor19_clean %>%
  filter(tema == "ingresos") %>% 
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, values_from = valor, values_fn = list(valor = sum), values_fill = 0)

# Escritura ingresos/egresos ----
write_csv(egresos19_mun_clean, "./data/3_final/egresos19_mun_clean.csv")
write_csv(ingresos19_mun_clean, "./data/3_final/ingresos19_mun_clean.csv")

# Lectura Percepcion ----

## Encig ----
encig19_mun_clean <- read_csv("./data/2_interim/encig19_mun_clean.csv")

## Efipem ----
efipem19_clean <- read_csv("./data/2_interim/efipem19_clean.csv")

## Censo ----
censo2020_mun_clean <- read_csv("./data/2_interim/censo2020_mun_clean.csv") %>%
  select(mun_inegi, pobtot, pea, graproes)

## Merge percepcion ----
efipemCor19_per_raw <- efipem19_clean %>%
  left_join(encig19_mun_clean, by = "mun_inegi") %>%
  left_join(censo2020_mun_clean, by = "mun_inegi") %>%
  select(-c("id_entidad", "id_municipio"))

## Limpieza percepcion ----
efipemCor19_per_clean <- efipemCor19_per_raw %>%
  filter(!is.na(prop_corrup)) %>%
  select(mun_inegi, nom_ent, nom_mun, prop_corrup, tema, categoria, descripcion_categoria, valor, pobtot, pea, graproes) %>%
  mutate(across(c("tema", "categoria", "descripcion_categoria"), ~ stri_trans_general(str_replace_all(str_replace_all(str_to_lower(.x), "\\s", "_"), ",", ""), id = "Latin-ASCII")),
         valor = valor/pea) # Gasto per capita PEA

## Escritura base completa percepcion ----
write.csv(efipemCor19_per_raw, "./data/2_interim/efipemCor19_per_raw.csv", row.names = FALSE)
write.csv(efipemCor19_per_clean, "./data/2_interim/efipemCor19_per_clean.csv")

# Escritura ingresos/egresos ----
write_csv(egresos19_per_clean, "./data/3_final/egresos19_per_clean.csv")
write_csv(ingresos19_per_clean, "./data/3_final/ingresos19_per_clean.csv")

# Base Egresos ----
egresos19_per_clean <- efipemCor19_per_clean %>%
  filter(tema == "egresos") %>% 
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, values_from = valor, values_fn = list(valor = sum), values_fill = 0) %>%
  select(-pea)

# Base Ingresos ----
ingresos19_per_clean <- efipemCor19_per_clean %>%
  filter(tema == "ingresos") %>% 
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, values_from = valor, values_fn = list(valor = sum), values_fill = 0) %>%
  select(-pea)

# Lectura Incidencia ----

## Encig ----
encig19_inc_clean <- read_csv("./data/2_interim/encig19_inc_clean.csv")

## Efipem ----
efipem19_clean <- read_csv("./data/2_interim/efipem19_clean.csv")

## Censo ----
censo2020_mun_clean <- read_csv("./data/2_interim/censo2020_mun_clean.csv") %>%
  select(mun_inegi, pobtot, pea, graproes)

## Merge incidencia ----
efipemCor19_inc_raw <- efipem19_clean %>%
  left_join(encig19_inc_clean, by = "mun_inegi") %>%
  left_join(censo2020_mun_clean, by = "mun_inegi") %>%
  select(-c("id_entidad", "id_municipio"))

## Limpieza incidencia ----
efipemCor19_inc_clean <- efipemCor19_inc_raw %>%
  filter(!is.na(prop_corrup)) %>%
  select(mun_inegi, nom_ent, nom_mun, prop_corrup, tema, categoria, descripcion_categoria, valor, pobtot, pea, graproes) %>%
  mutate(across(c("tema", "categoria", "descripcion_categoria"), ~ stri_trans_general(str_replace_all(str_replace_all(str_to_lower(.x), "\\s", "_"), ",", ""), id = "Latin-ASCII")),
         valor = valor/pea) # Gasto per capita PEA
  

## Escritura base completa incidencia ----
write.csv(efipemCor19_inc_raw, "./data/2_interim/efipemCor19_inc_raw.csv")
write.csv(efipemCor19_inc_clean, "./data/2_interim/efipemCor19_inc_clean.csv")

# Base Egresos ----
egresos19_inc_clean <- efipemCor19_inc_clean %>%
  filter(tema == "egresos") %>% 
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, values_from = valor, values_fn = list(valor = sum), values_fill = 0) %>%
  select(-pea)

# Base Ingresos ----
ingresos19_inc_clean <- efipemCor19_inc_clean %>%
  filter(tema == "ingresos") %>% 
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, values_from = valor, values_fn = list(valor = sum), values_fill = 0) %>%
  select(-pea)

# Escritura ingresos/egresos ----
write_csv(egresos19_inc_clean, "./data/3_final/egresos19_inc_clean.csv")
write_csv(ingresos19_inc_clean, "./data/3_final/ingresos19_inc_clean.csv")