### Limpieza base de datos final ###

# Librerias ----
library(tidyverse)
library(stringi)

# Lectura Percepcion ----

## Encig ----
encig19_per_clean <- read_csv("./data/2_interim/encig19_per_clean.csv")

## Efipem ----
efipem19_clean <- read_csv("./data/2_interim/efipem19_clean.csv")

## Censo ----
censo2020_mun_clean <- read_csv("./data/2_interim/censo2020_mun_clean.csv") %>%
  select(mun_inegi, pobtot, pea, graproes)

## Censo economico 2019 ----
censo_econ2019_clean <- fst::read_fst("./data/2_interim/censo_econ2019_clean.fst")

## Merge percepcion ----
efipemCor19_per_raw <- efipem19_clean %>%
  left_join(encig19_per_clean, by = "mun_inegi") %>%
  left_join(censo2020_mun_clean, by = "mun_inegi") %>%
  left_join(censo_econ2019_clean, by = "mun_inegi") %>%
  select(-c("id_entidad", "id_municipio"))

## Limpieza percepcion ----
efipemCor19_per_clean <- efipemCor19_per_raw %>%
  filter(!is.na(prop_corrup)) %>%
  mutate(estado = substr(mun_inegi, 1, 2)) %>%
  select(mun_inegi, estado, everything()) %>%
  mutate(across(c("tema", "categoria", "descripcion_categoria"), ~ stri_trans_general(str_replace_all(str_replace_all(str_to_lower(.x), "\\s", "_"), ",", ""), id = "Latin-ASCII")),
         valor = valor/pea) # Gasto per capita PEA

# Municipios sin datos economicos 
mun_NA <- unique(efipemCor19_per_clean$mun_inegi[is.na(efipemCor19_per_clean$a111a_23)])
mean_mun <- censo_econ2019_clean %>%
  mutate(estado = substr(mun_inegi, 1, 2)) %>%
  filter(estado %in% substr(mun_NA, 1, 2)) %>%
  group_by(estado) %>%
  summarise(across(everything(), mean)) %>%
  select(-mun_inegi)

# Se agregan las medias estatales a los municipios con NA

for(muni in mun_NA){
  index <- which(efipemCor19_per_clean$mun_inegi == muni)
  index_mean <- which(mean_mun$estado == substr(muni, 1, 2))
  for(var in select(efipemCor19_per_clean, starts_with(c("a", "h", "j", "k"))) %>% names()){
    efipemCor19_per_clean[index, var] <- mean_mun[index_mean, var]
  }
}
efipemCor19_clean <- efipemCor19_per_clean %>%
  select(-estado)

## Escritura base completa percepcion ----
# write.csv(efipemCor19_per_raw, "./data/2_interim/efipemCor19_per_raw.csv", row.names = FALSE)
# write.csv(efipemCor19_per_clean, "./data/2_interim/efipemCor19_per_clean.csv", row.names = FALSE)
fst::write_fst(efipemCor19_per_raw, "./data/2_interim/efipemCor19_per_raw.fst")
fst::write_fst(efipemCor19_per_clean, "./data/2_interim/efipemCor19_per_clean.fst")

## Base Egresos ----
egresos19_per_clean <- efipemCor19_per_clean %>%
  filter(tema == "egresos") %>% 
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, values_from = valor, values_fn = list(valor = sum), values_fill = 0) %>%
  select(-pea)

## Base Ingresos ----
ingresos19_per_clean <- efipemCor19_per_clean %>%
  filter(tema == "ingresos") %>% 
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, values_from = valor, values_fn = list(valor = sum), values_fill = 0) %>%
  select(-pea)

## Escritura ingresos/egresos ----
# write_csv(egresos19_per_clean, "./data/3_final/egresos19_per_clean.csv")
# write_csv(ingresos19_per_clean, "./data/3_final/ingresos19_per_clean.csv")

fst::write_fst(egresos19_per_clean, "./data/3_final/egresos19_per_clean.fst")
fst::write_fst(ingresos19_per_clean, "./data/3_final/ingresos19_per_clean.fst")

# Lectura Incidencia ----

## Encig ----
encig19_inc_clean <- read_csv("./data/2_interim/encig19_inc_clean.csv")

## Efipem ----
efipem19_clean <- read_csv("./data/2_interim/efipem19_clean.csv")

## Censo ----
censo2020_mun_clean <- read_csv("./data/2_interim/censo2020_mun_clean.csv") %>%
  select(mun_inegi, pobtot, pea, graproes)

## Censo economico 2019 ----
censo_econ2019_clean <- fst::read_fst("./data/2_interim/censo_econ2019_clean.fst")

## Merge incidencia ----
efipemCor19_inc_raw <- efipem19_clean %>%
  left_join(encig19_inc_clean, by = "mun_inegi") %>%
  left_join(censo2020_mun_clean, by = "mun_inegi") %>%
  left_join(censo_econ2019_clean, by = "mun_inegi") %>%
  select(-c("id_entidad", "id_municipio"))

## Limpieza incidencia ----
efipemCor19_inc_clean <- efipemCor19_inc_raw %>%
  filter(!is.na(prop_corrup)) %>%
  mutate(estado = substr(mun_inegi, 1, 2)) %>%
  select(mun_inegi, estado, everything()) %>%
  mutate(across(c("tema", "categoria", "descripcion_categoria"), ~ stri_trans_general(str_replace_all(str_replace_all(str_to_lower(.x), "\\s", "_"), ",", ""), id = "Latin-ASCII")),
         valor = valor/pea) # Gasto per capita PEA

# Municipios sin datos economicos 
mun_NA <- unique(efipemCor19_inc_clean$mun_inegi[is.na(efipemCor19_inc_clean$a111a_23)])
mean_mun <- censo_econ2019_clean %>%
  mutate(estado = substr(mun_inegi, 1, 2)) %>%
  filter(estado %in% substr(mun_NA, 1, 2)) %>%
  group_by(estado) %>%
  summarise(across(everything(), mean)) %>%
  select(-mun_inegi)

# Se agregan las medias estatales a los municipios con NA

for(muni in mun_NA){
  index <- which(efipemCor19_inc_clean$mun_inegi == muni)
  index_mean <- which(mean_mun$estado == substr(muni, 1, 2))
  for(var in select(efipemCor19_inc_clean, starts_with(c("a", "h", "j", "k"))) %>% names()){
    efipemCor19_inc_clean[index, var] <- mean_mun[index_mean, var]
  }
}
efipemCor19_clean <- efipemCor19_inc_clean %>%
  select(-estado)

## Escritura base completa incidencia ----
fst::write_fst(efipemCor19_inc_raw, "./data/2_interim/efipemCor19_inc_raw.fst")
fst::write_fst(efipemCor19_inc_clean, "./data/2_interim/efipemCor19_inc_clean.fst")

## Base Egresos ----
egresos19_inc_clean <- efipemCor19_inc_clean %>%
  filter(tema == "egresos") %>% 
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, values_from = valor, values_fn = list(valor = sum), values_fill = 0) %>%
  select(-pea)

## Base Ingresos ----
ingresos19_inc_clean <- efipemCor19_inc_clean %>%
  filter(tema == "ingresos") %>% 
  unite(col = "cat_desc", categoria, descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = cat_desc, values_from = valor, values_fn = list(valor = sum), values_fill = 0) %>%
  select(-pea)

## Escritura ingresos/egresos ----
fst::write_fst(egresos19_inc_clean, "./data/3_final/egresos19_inc_clean.fst")
fst::write_fst(ingresos19_inc_clean, "./data/3_final/ingresos19_inc_clean.fst")
