 ### Limpieza ENCIG 2019 ###

# Librerias ----
library(tidyverse)
library(survey)

# Lectura ----
encig19_raw <- read_csv("./data/1_raw/encig19_raw.csv")

# Limpieza ----
encig19_clean <- encig19_raw %>%
  filter(p3_2 != 9 & p9_1 != 9) %>%
  mutate(mun_inegi = paste(ent, mun, sep = ""), # Se agrega la clave de municipio
         corrup = ifelse(p3_2 == 4, 1, 0), # Dummy que capturara la respuesta muy frecuente
         corrup_fac = corrup * fac_p18,
         no_corrup_fac = fac_p18 * ifelse(p3_2 != 4, 1, 0),
         inc_corrup = ifelse(p9_1  == 1, 1, 0), # Incidencia de corrupcion: 
         inc_no_corrup_fac = fac_p18 * ifelse(p9_1 != 1, 1, 0),
         inc_corrup_fac = inc_corrup * fac_p18
         ) %>% 
  select(starts_with("id"), est_dis, mun_inegi, nom_ent, nom_mun, corrup, corrup_fac, no_corrup_fac, starts_with("p3"), inc_corrup, inc_corrup_fac, inc_no_corrup_fac, starts_with("p9"), starts_with("fac")) # Se seleccionan las var de corru

## Percepcion corrupcion ----

## Nivel municipio
encig19_mun_clean <- encig19_clean %>%
  group_by(mun_inegi, nom_ent, nom_mun) %>%
  summarise(frec_corrup = sum(corrup_fac),
            frec_no_corrup = sum(no_corrup_fac),
            prop_corrup = (frec_corrup/frec_no_corrup)*100)

## Incidencia corrupcion ----

encig19_inc_clean <- encig19_clean %>%
  group_by(mun_inegi, nom_ent, nom_mun) %>%
  summarise(frec_inc_corrup = sum(inc_corrup_fac),
            frec_no_corrup = sum(no_corrup_fac),
            prop_corrup = (frec_inc_corrup/frec_no_corrup)*100)

# Escritura ----

# Base 
write_csv(encig19_clean, "./data/2_interim/encig19_clean.csv")
# Percepcion
write_csv(encig19_mun_clean, "./data/2_interim/encig19_mun_clean.csv")
# Incidencia 
write_csv(encig19_inc_clean, "./data/2_interim/encig19_inc_clean.csv")
