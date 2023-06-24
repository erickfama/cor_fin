### Nueva base ###

# Liberías ----
library(tidyverse)

# Lectura ----
fs_raw <- readxl::read_xlsx("./data/1_raw/fs_multilevel.xlsx")
pibe_a1_raw <- read_csv("./data/1_raw/conjunto_de_datos_piber_actividad_ap2021_p.csv") %>% janitor::clean_names()
pibe_a2_raw <- read_csv("./data/1_raw/conjunto_de_datos_piber_actividad_as2021_p.csv")%>% janitor::clean_names()
pibe_a3_raw <- read_csv("./data/1_raw/conjunto_de_datos_piber_actividad_at2021_p.csv")%>% janitor::clean_names()

# Limpieza ----
pibe_a1_clean <- pibe_a1_raw %>%
  mutate(indicador = str_extract(descriptores, "(?<=\\|)(.*?)(?=\\|)"),
         estado = str_extract(descriptores, "(?<=\\|)([A-Za-zá-ú\\s]+)(?=<C1>)")) %>%
  filter(indicador == "Millones de pesos" & estado != "Estados Unidos Mexicanos")

pibe_a1_clean %>% 
  summarise(pib = sum(x2021_p))

# Reemplazar nombre de columnas

# Actividades primarias
colnames_pib <- names(pibe_a1_clean)

colnames_pib <- sapply(colnames_pib, function(col_name){
  col_name <- ifelse(str_detect(col_name, "^x") == TRUE, str_replace(col_name, "^x", "pib_"), col_name)
  ifelse(str_detect(col_name, "_r|_p$") == TRUE, str_replace(col_name, "_r|_p$", ""), col_name)
})

colnames(pibe_a1_clean) <- colnames_pib

pibe_a1_clean <- pibe_a1_clean %>%
  select(estado, ends_with("2018"), ends_with("2019"), ends_with("2021")) %>%
  pivot_longer(cols = -estado, names_to = "anio", values_to = "pib_a1_edo") %>%
  mutate(anio = str_remove(anio, "pib_"))

# Actividades secundarias
pibe_a2_clean <- pibe_a2_raw %>%
  mutate(indicador = str_extract(descriptores, "(?<=\\|)(.*?)(?=\\|)"),
         estado = str_extract(descriptores, "(?<=\\|)([A-Za-zá-ú\\s]+)(?=<C1>)")) %>%
  filter(indicador == "Millones de pesos" & estado != "Estados Unidos Mexicanos")

pibe_a2_clean %>% 
  summarise(pib = sum(x2021_p))

colnames_pib <- names(pibe_a2_clean)

colnames_pib <- sapply(colnames_pib, function(col_name){
  col_name <- ifelse(str_detect(col_name, "^x") == TRUE, str_replace(col_name, "^x", "pib_"), col_name)
  ifelse(str_detect(col_name, "_r|_p$") == TRUE, str_replace(col_name, "_r|_p$", ""), col_name)
})

colnames(pibe_a2_clean) <- colnames_pib

pibe_a2_clean <- pibe_a2_clean %>%
  select(estado, ends_with("2018"), ends_with("2019"), ends_with("2021")) %>%
  pivot_longer(cols = -estado, names_to = "anio", values_to = "pib_a2_edo") %>%
  mutate(anio = str_remove(anio, "pib_"))

# Actividades terciarias
pibe_a3_clean <- pibe_a3_raw %>%
  mutate(indicador = str_extract(descriptores, "(?<=\\|)(.*?)(?=\\|)"),
         estado = str_extract(descriptores, "(?<=\\|)([A-Za-zá-ú\\s]+)(?=<C1>)")) %>%
  filter(indicador == "Millones de pesos" & estado != "Estados Unidos Mexicanos")

pibe_a3_clean %>% 
  summarise(pib = sum(x2021_p))

colnames_pib <- names(pibe_a3_clean)

colnames_pib <- sapply(colnames_pib, function(col_name){
  col_name <- ifelse(str_detect(col_name, "^x") == TRUE, str_replace(col_name, "^x", "pib_"), col_name)
  ifelse(str_detect(col_name, "_r|_p$") == TRUE, str_replace(col_name, "_r|_p$", ""), col_name)
})

colnames(pibe_a3_clean) <- colnames_pib

pibe_a3_clean <- pibe_a3_clean %>%
  select(estado, ends_with("2018"), ends_with("2019"), ends_with("2021")) %>%
  pivot_longer(cols = -estado, names_to = "anio", values_to = "pib_a3_edo") %>%
  mutate(anio = str_remove(anio, "pib_"))

pibe <- pibe_a1_clean %>%
  left_join(pibe_a2_clean, by = c("estado", "anio")) %>%
  left_join(pibe_a3_clean, by = c("estado", "anio")) %>%
  mutate(pib_edo = pib_a1_edo + pib_a2_edo + pib_a3_edo,
         anio = as.numeric(anio),
         estado = ifelse(str_detect(estado, "Querétaro") == TRUE, str_replace(estado, "Querétaro", "Querétaro de Arteaga"), estado)) %>%
  select(nom_ent = estado, anio, pib_edo)

# Agregar variables ----

# PIBE
fs_clean <- fs_raw %>%
  left_join(pibe, by = c("nom_ent", "anio")) %>%
  rename(graproes_mun = graproes,
         pobtot_mun = pobtot) %>%
  group_by(nom_ent, anio) %>%
  mutate(graproes_edo = mean(graproes_mun),
         pobtot_edo = sum(pobtot_mun),
         pib_percapita_edo = pib_edo/pobtot_edo) %>% 
  ungroup() 

# Escritura ----
write_excel_csv(fs_clean, "./data/3_final/fs_multilevel.csv")
