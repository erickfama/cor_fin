 ### Limpieza ENCIG 2019 ###

# Librerias ----
library(tidyverse)

# Lectura ----
encig17_raw <- read_csv("./data/1_raw/encig17_raw.csv")
encig19_raw <- read_csv("./data/1_raw/encig19_raw.csv")
encig21_raw <- read_csv("./data/1_raw/encig21_raw.csv")

# Limpieza ----

## Encig 17 ----
encig17_clean <- encig17_raw %>%
  filter(p3_2 != 9 & p9_1 != 9) %>%
  mutate(across(starts_with("p8_3"), ~ ifelse(.x == 1, 1, 0)), 
         inc_corrup17_tot= p8_3_1 + p8_3_2 + p8_3_3) %>%
  mutate(mun_inegi = paste(case_when(nchar(ent) == 1 ~ paste("0", ent, sep = ""),
                                     nchar(ent) == 2 ~ paste("", ent, sep = "")),
                           case_when(nchar(mun) == 1 ~ paste("00", mun, sep = ""),
                                     nchar(mun) == 2 ~ paste("0", mun, sep = ""),
                                     nchar(mun) == 3 ~ paste("", mun, sep = "")),
                           sep = ""), # Se agrega la clave de municipio
         corrup = ifelse(p3_2 == 1, 1, 0), # Dummy que capturara la respuesta muy frecuente. El valor 1 corresponde a la respuesta "Muy frecuente"
         corrup_fac = corrup * fac_p18,
         no_corrup_fac = fac_p18 * ifelse(p3_2 != 1, 1, 0),
         inc_corrup17 = ifelse(inc_corrup17_tot > mean(inc_corrup17_tot), 1, 0),
         inc_corrup = ifelse(p9_1  == 1, 1, 0), # Incidencia de corrupcion: 
         inc_no_corrup_fac = fac_p18 * ifelse(p9_1 != 1, 1, 0),
         inc_corrup_fac = inc_corrup * fac_p18
         ) %>% 
  select(starts_with("id"), est_dis, mun_inegi, nom_ent, nom_mun, corrup, corrup_fac, no_corrup_fac, starts_with("p3"), inc_corrup17_tot, inc_corrup17, inc_corrup, inc_corrup_fac, inc_no_corrup_fac, starts_with("p9"), starts_with("fac")) # Se seleccionan las var de corru

## Encig 19 ----
encig19_clean <- encig19_raw %>%
  filter(p3_2 != 9 & p9_1 != 9) %>%
  mutate(across(starts_with("p8_3"), ~ ifelse(.x == 1, 1, 0)), 
         inc_corrup19_tot= p8_3_1 + p8_3_2 + p8_3_3) %>%
  mutate(mun_inegi = paste(case_when(nchar(ent) == 1 ~ paste("0", ent, sep = ""),
                                     nchar(ent) == 2 ~ paste("", ent, sep = "")),
                           case_when(nchar(mun) == 1 ~ paste("00", mun, sep = ""),
                                     nchar(mun) == 2 ~ paste("0", mun, sep = ""),
                                     nchar(mun) == 3 ~ paste("", mun, sep = "")),
                           sep = ""), # Se agrega la clave de municipio
           corrup = ifelse(p3_2 == 1, 1, 0), # Dummy que capturara la respuesta muy frecuente
         corrup_fac = corrup * fac_p18,
         no_corrup_fac = fac_p18 * ifelse(p3_2 != 1, 1, 0),
         inc_corrup19 = ifelse(inc_corrup19_tot > mean(inc_corrup19_tot), 1, 0),
         inc_corrup = ifelse(p9_1  == 1, 1, 0), # Incidencia de corrupcion: 
         inc_no_corrup_fac = fac_p18 * ifelse(p9_1 != 1, 1, 0),
         inc_corrup_fac = inc_corrup * fac_p18
         ) %>% 
  select(starts_with("id"), est_dis, mun_inegi, nom_ent, nom_mun, corrup, corrup_fac, no_corrup_fac, starts_with("p3"), inc_corrup19_tot, inc_corrup19, inc_corrup, inc_corrup_fac, inc_no_corrup_fac, starts_with("p9"), starts_with("fac")) # Se seleccionan las var de corru

## Encig 21 ----
encig21_clean <- encig21_raw %>%
  mutate(across(c(nom_ent, nom_mun), ~ str_to_sentence(.x))) %>%
  filter(p3_2 != 9 & p9_1 != 9) %>%
  mutate(across(starts_with("p8_3"), ~ ifelse(.x == 1, 1, 0)), 
         inc_corrup21_tot= p8_3_1 + p8_3_2 + p8_3_3) %>% # Se calcula con las experiencias vividas por el encuestado durante 2021, son varios tipos de experiencias
  mutate(mun_inegi = paste(case_when(nchar(ent) == 1 ~ paste("0", ent, sep = ""),
                                     nchar(ent) == 2 ~ paste("", ent, sep = "")),
                           case_when(nchar(mun) == 1 ~ paste("00", mun, sep = ""),
                                     nchar(mun) == 2 ~ paste("0", mun, sep = ""),
                                     nchar(mun) == 3 ~ paste("", mun, sep = "")),
                           sep = ""), # Se agrega la clave de municipio
         corrup = ifelse(p3_2 == 1, 1, 0), # Dummy que capturara la respuesta muy frecuente
         corrup_fac = corrup * fac_p18,
         no_corrup_fac = fac_p18 * ifelse(p3_2 != 1, 1, 0),
         inc_corrup21 = ifelse(inc_corrup21_tot > mean(inc_corrup21_tot), 1, 0), # Si el total de experiencias es arriba del total de la media se asigna como corrupcion total
         inc_corrup = ifelse(p9_1  == 1, 1, 0), # Incidencia de corrupcion: # Se calcula con las experiencias vividas por el encuestado en los ultimos 5 anios
         inc_no_corrup_fac = fac_p18 * ifelse(p9_1 != 1, 1, 0),
         inc_corrup_fac = inc_corrup * fac_p18
         ) %>% 
  select(starts_with("id"), est_dis, mun_inegi, nom_ent, nom_mun, corrup, corrup_fac, no_corrup_fac, starts_with("p3"), inc_corrup21_tot, inc_corrup21, inc_corrup, inc_corrup_fac, inc_no_corrup_fac, starts_with("p9"), starts_with("fac")) # Se seleccionan las var de corru

## Percepcion corrupcion ----

### Encig 17 ----

## Nivel municipio
encig17_per_clean <- encig17_clean %>%
  group_by(mun_inegi, nom_ent, nom_mun) %>%
  summarise(frec_corrup = sum(corrup_fac),
            frec_no_corrup = sum(no_corrup_fac),
            prop_corrup_per17 = ((frec_corrup/(frec_corrup + frec_no_corrup))*100))%>%
  ungroup() %>% 
  mutate(anio = "2018") %>%
  select(mun_inegi, anio, nom_ent, nom_mun, starts_with("prop"))

## Incidencia corrupcion ----

encig17_inc_clean <- encig17_clean %>%
  group_by(mun_inegi, nom_ent, nom_mun) %>%
  summarise(frec_inc_corrup = sum(inc_corrup_fac),
            frec_no_corrup = sum(no_corrup_fac),
            prop_corrup_inc17 = mean(inc_corrup17)*100, # Porcentaje de los encuestados que reportaron haber vivido experiencias de corrupcion en 2017
            prop_corrup5_inc17 = (frec_inc_corrup/(frec_inc_corrup + frec_no_corrup))*100) %>%
  ungroup()%>% 
  mutate(anio = "2018") %>%
  select(mun_inegi, anio, nom_ent, nom_mun, starts_with("prop"))

## Combinado 17 ----
encig17_clean <- encig17_per_clean %>%
  left_join(encig17_inc_clean %>% select(mun_inegi, starts_with("prop")), by = "mun_inegi")

### Encig 19 ----

## Nivel municipio
encig19_per_clean <- encig19_clean %>%
  group_by(mun_inegi, nom_ent, nom_mun) %>%
  summarise(frec_corrup = sum(corrup_fac),
            frec_no_corrup = sum(no_corrup_fac),
            prop_corrup_per19 = ((frec_corrup/(frec_corrup + frec_no_corrup))*100))%>%
  ungroup()%>% 
  mutate(anio = "2019") %>%
  select(mun_inegi, anio, nom_ent, nom_mun, starts_with("prop"))


## Incidencia corrupcion ----
encig19_inc_clean <- encig19_clean %>%
  group_by(mun_inegi, nom_ent, nom_mun) %>%
  summarise(frec_inc_corrup = sum(inc_corrup_fac),
            frec_no_corrup = sum(no_corrup_fac),
            prop_corrup_inc19 = mean(inc_corrup19)*100,# Porcentaje de los encuestados que reportaron haber vivido experiencias de corrupcion en 2019
            prop_corrup5_inc19 = (frec_inc_corrup/(frec_inc_corrup + frec_no_corrup))*100)%>%
  ungroup()%>% 
  mutate(anio = "2019") %>%
  select(mun_inegi, anio, nom_ent, nom_mun, starts_with("prop"))

## Combinado 19 ----
encig19_clean <- encig19_per_clean %>%
  left_join(encig19_inc_clean %>% select(mun_inegi, starts_with("prop")), by = "mun_inegi")

### Encig 21 ----

## Nivel municipio
encig21_per_clean <- encig21_clean %>%
  group_by(mun_inegi, nom_ent, nom_mun) %>%
  summarise(frec_corrup = sum(corrup_fac),
            frec_no_corrup = sum(no_corrup_fac),
            prop_corrup_per21 = ((frec_corrup/(frec_corrup + frec_no_corrup))*100))%>%
  ungroup()%>% 
  mutate(anio = "2021") %>%
  select(mun_inegi, anio, nom_ent, nom_mun, starts_with("prop"))


## Incidencia corrupcion ----
encig21_inc_clean <- encig21_clean %>%
  group_by(mun_inegi, nom_ent, nom_mun) %>%
  summarise(frec_inc_corrup = sum(inc_corrup_fac),
            frec_no_corrup = sum(no_corrup_fac),
            prop_corrup_inc21 = mean(inc_corrup21)*100, # Porcentaje de los encuestados que reportaron haber vivido experiencias de corrupcion en 2021
            prop_corrup5_inc21 = (frec_inc_corrup/(frec_inc_corrup + frec_no_corrup))*100) %>%
  ungroup()%>% 
  mutate(anio = "2021") %>%
  select(mun_inegi, anio, nom_ent, nom_mun, starts_with("prop"))

## Combinado 21 ----
encig21_clean <- encig21_per_clean %>%
  left_join(encig21_inc_clean %>% select(mun_inegi, starts_with("prop")), by = "mun_inegi")


# Escritura ----

## Encig 17 ----

# Base 
write_csv(encig17_clean, "./data/2_interim/encig17_clean.csv")
# Percepcion
write_csv(encig17_per_clean, "./data/2_interim/encig17_per_clean.csv")
# Incidencia 
write_csv(encig17_inc_clean, "./data/2_interim/encig17_inc_clean.csv")

## Encig 19 ----

# Base 
write_csv(encig19_clean, "./data/2_interim/encig19_clean.csv")
# Percepcion
write_csv(encig19_per_clean, "./data/2_interim/encig19_per_clean.csv")
# Incidencia 
write_csv(encig19_inc_clean, "./data/2_interim/encig19_inc_clean.csv")

## Encig 21 ----

# Base 
write_csv(encig21_clean, "./data/2_interim/encig21_clean.csv")
# Percepcion
write_csv(encig21_per_clean, "./data/2_interim/encig21_per_clean.csv")
# Incidencia 
write_csv(encig21_inc_clean, "./data/2_interim/encig21_inc_clean.csv")


# Plots porcentajes corrupcion ----
encig17_per_clean %>%
  ggplot(aes(prop_corrup_per17)) +
  geom_histogram(fill = "green") + 
  geom_vline(xintercept = encig17_per_clean %>% pull(prop_corrup_per17) %>% mean()) +
  labs(title = "per 17")

encig19_per_clean %>%
  ggplot(aes(prop_corrup_per19)) +
  geom_histogram(fill = "blue") + 
  geom_vline(xintercept = encig19_per_clean %>% pull(prop_corrup_per19) %>% mean()) +
  labs(title = "per 19")

encig21_per_clean %>%
  ggplot(aes(prop_corrup_per21)) +
  geom_histogram(fill = "red") + 
  geom_vline(xintercept = encig21_per_clean %>% pull(prop_corrup_per21) %>% mean()) +
  labs(title = "per 21")


encig17_inc_clean %>%
  ggplot(aes(prop_corrup_inc17)) +
  geom_histogram(fill = "green") + 
  geom_vline(xintercept = encig17_inc_clean %>% pull(prop_corrup_inc17) %>% mean()) +
  scale_x_continuous(limits = c(0, 60)) + 
  labs(title = "inc 17")

encig19_inc_clean %>%
  ggplot(aes(prop_corrup_inc19)) +
  geom_histogram(fill = "blue") + 
  geom_vline(xintercept = encig19_inc_clean %>% pull(prop_corrup_inc19) %>% mean()) +
  scale_x_continuous(limits = c(0, 60)) + 
  labs(title = "inc 19")

encig21_inc_clean %>%
  ggplot(aes(prop_corrup_inc21)) +
  geom_histogram(fill = "red") + 
  geom_vline(xintercept = encig21_inc_clean %>% pull(prop_corrup_inc21) %>% mean()) +
  scale_x_continuous(limits = c(0, 60)) + 
  labs(title = "inc 21")
