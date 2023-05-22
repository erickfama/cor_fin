### Indicadores de solvencia financiera ###

# Librerias ----
library(tidyverse)

# Lectura ----

# La efipem solo capto 2088 municipios de los 2453 en 2021
efipem_clean <- read_csv("./data/2_interim/efipem_clean.csv")
censo_raw <- read_csv("./data/1_raw/conjunto_de_datos_iter_00CSV20.csv") %>%
  janitor::clean_names()
imm_raw <- readxl::read_xls("./data/1_raw/IMM_2020.xls", skip = 1)

# Indicadores ----

## Cash Solvency ----

### Revenue per capita (Ingresos totales / Poblacion) ----
revenue_perCapita <- efipem_clean %>%
  filter(descripcion_categoria == "total_de_ingresos") %>%
  mutate(cs_revenue_perCapita = valor/pobtot) %>%
  select(mun_inegi, anio, total_revs = valor, pobtot, cs_revenue_perCapita)

### Total general fund revenues from own sources / total general fund sources ----
total_general_fund_rev_own <- efipem_clean %>%
  filter(tema == "ingresos" & categoria == "capitulo" & descripcion_categoria %in% c("impuestos", "contribuciones_de_mejoras", "derechos", "productos", "aprovechamientos")) %>%
  group_by(mun_inegi, anio) %>%
  summarise(total_general_fund_rev_own = sum(valor, na.rm = TRUE))

total_general_fund_rev <- efipem_clean %>%
  filter(tema == "ingresos" & categoria == "capitulo" & descripcion_categoria %in% c("impuestos", "contribuciones_de_mejoras", "derechos", "productos", "aprovechamientos", "participaciones_federales", "aportaciones_federales_y_estatales")) %>%
  group_by(mun_inegi, anio) %>%
  summarise(total_general_fund_rev = sum(valor))

total_general_fund <- total_general_fund_rev_own %>%
  left_join(total_general_fund_rev, by = c("mun_inegi", "anio")) %>%
  mutate(cs_total_general_fund = total_general_fund_rev_own/total_general_fund_rev)

### Intergovernmental revenues / Total revenues ----
total_revs <- efipem_clean %>%
  filter(tema == "ingresos" & descripcion_categoria == "total_de_ingresos")

intergovernmental_revs <- efipem_clean %>% 
  filter(tema == "ingresos" & categoria == "capitulo" & descripcion_categoria %in% c("aportaciones_federales_y_estatales", "participaciones_federales")) %>% # OJO CON ESTO
  group_by(mun_inegi, anio) %>%
  summarise(intergovernmental_revs = sum(valor)) %>%
  ungroup() %>%
  mutate(total_revs = efipem_clean %>% filter(descripcion_categoria == "total_de_ingresos") %>% pull(valor),
         cs_intergov_revs_total_revs = intergovernmental_revs / total_revs)

### Property tax / Total revenue ----
property_tax <- efipem_clean %>%
  filter(tema == "ingresos" & categoria == "concepto" & descripcion_categoria %in% c("impuestos_sobre_el_patrimonio")) %>%
  group_by(mun_inegi, anio) %>%
  summarise(property_tax = valor) %>%
  ungroup() %>%
  left_join(total_revs %>% select(mun_inegi, anio, valor), by = c("mun_inegi", "anio")) %>%
  mutate(cs_propety_tax_total_revs = property_tax / valor) # Aqui resultan 1706 NAs porque hay 1706 municipios que no cuentan con datos de impuestos sobre el patrimonio en los 4 anios

### General fund balance / Total government expenditures ----

# Total Expenditures
total_exp <- efipem_clean %>%
  filter(tema == "egresos" & descripcion_categoria == "total_de_egresos") %>%
  select(mun_inegi, anio, total_exp = valor, pobtot)

general_fund <- efipem_clean %>%
  filter()

## Conjunto de indicadores Cash Solvency ----
cash_solvency <- revenue_perCapita %>% 
  left_join(total_general_fund, by = c("mun_inegi", "anio")) %>%
  left_join(intergovernmental_revs, by = c("mun_inegi", "anio")) %>%
  left_join(property_tax, by = c("mun_inegi", "anio"))

rm(total_general_fund, total_general_fund_rev, total_general_fund_rev_own, revenue_perCapita, intergovernmental_revs, property_tax)

## Budget Solvency ----

# Solo egresos
expenditure <- efipem_clean %>%
  filter(tema == "egresos")

### Operating expenditures / Total Expenditures ----
operating_exp <- expenditure %>%
  filter(categoria == "capitulo" & descripcion_categoria %in% c("servicios_personales", "materiales_y_suministros", "servicios_generales")) %>% # OJO con esto
  group_by(mun_inegi, anio) %>%
  summarise(operating_exp = sum(valor)) %>%
  ungroup() %>%
  mutate(total_exp = total_exp$total_exp, 
         bs_operating_exp = operating_exp / total_exp) 

### Operating balance: total revenues - finance / total expenditures ----
total_revs_noFinance <- efipem_clean %>%
  filter(tema == "ingresos" & categoria == "capitulo" & descripcion_categoria != "financiamiento") %>%
  group_by(mun_inegi, anio) %>% 
  summarise(total_revs_noFinance = sum(valor))

operating_balance <- data.frame(mun_inegi = total_exp$mun_inegi,
                                anio = total_exp$anio, 
                                total_exp = total_exp$total_exp) %>%
  left_join(total_revs_noFinance, by = c("mun_inegi", "anio")) %>%
  mutate(bs_operating_balance = total_revs_noFinance/total_exp) # Resulta interesante, estos registros demuestran que todos los ingresos son gastados
 
### Total expenditures per capita ----
expenditures_perCapita <- total_exp %>%
  mutate(bs_expenditure_perCapita = total_exp / pobtot)

## Conjunto de indicadores Budget Solvency ----
budget_solvency <- operating_exp %>% 
  left_join(operating_balance, by = c("mun_inegi", "anio")) %>%
  left_join(expenditures_perCapita, by = c("mun_inegi", "anio"))

## Long-run solvency ----

### Direct long-term debt / Population ----

direct_long_term_debt <- efipem_clean %>%
  filter(tema == "egresos" & descripcion_categoria == "deuda_publica") %>%
  mutate(lrs_direct_long_term_debt_pobtot = valor/pobtot) %>% 
  select(mun_inegi, anio, financiamiento = valor, pobtot, lrs_direct_long_term_debt_pobtot)

### Debt service / Total revenues ----
debt_service <- efipem_clean %>%
  filter(tema == "egresos" & descripcion_categoria == "deuda_publica") %>%
  left_join(total_revs %>% select(mun_inegi, anio, total_rev = valor), by = c("mun_inegi", "anio")) %>%
  mutate(lrs_debt_service_total_rev = valor / total_rev) %>%
  select(mun_inegi, anio, deuda_publica = valor, total_rev, lrs_debt_service_total_rev)

## Conjunto de indicadores Long-run Solvency ----

long_run_solvency <- direct_long_term_debt %>%
  left_join(debt_service, by = c("mun_inegi", "anio"))

## DF de indicadores ----

indicadores_fs <- cash_solvency %>%
  select(mun_inegi, anio, pobtot, starts_with("cs_")) %>%
  left_join(budget_solvency %>% select(mun_inegi, anio, starts_with("bs_")), by = c("mun_inegi", "anio")) %>%
  left_join(long_run_solvency %>% select(mun_inegi, anio, starts_with("lrs_")), by = c("mun_inegi", "anio"))

# Variables contextuales ----
# indicadores_fs <- read_csv("./data/3_final/indicadores_fs.csv")

## Censo 2020 ----
censo_clean <- censo_raw %>%
  mutate(mun_inegi = paste(entidad, mun, sep = "")) %>% 
  filter(mun_inegi != "00000" & mun != "000", loc == "0000") %>%
  # Tipología de municipios: https://archivos.juridicas.unam.mx/www/bjv/libros/10/4513/8.pdf
  group_by(mun_inegi, loc) %>%
  mutate(mun_tipo = case_when(pobtot >= 150000 ~ "metropolitano",
                                 pobtot >= 30000 & pobtot < 150000 ~ "urbano",
                                 pobtot >= 10000 & pobtot < 30000 ~ "transición_rural-urbano",
                                 pobtot >= 0 & pobtot < 10000 ~ "rural")
  ) %>%
  ungroup() %>%
  select(mun_inegi, mun_tipo, graproes, pea, pe_inac, pocupada, pdesocup, psinder, pder_ss)

indicadores_fs <- indicadores_fs %>%
  left_join(censo_clean, by = "mun_inegi")

## Índice de marginación ----

# Se elimina la columna de posiciones y las últimas dos filas porque no contienen info.
imm_clean <- imm_raw[7:nrow(imm_raw) - 2, 1:ncol(imm_raw) - 1]

# Se renombran las columnas
names(imm_clean) <- unlist(imm_raw[2, 1:ncol(imm_raw) - 1], use.names = FALSE) %>%
  str_to_lower() %>% str_replace("cve_mun", "mun_inegi")

indicadores_fs <- indicadores_fs %>%
  left_join(imm_clean %>% select(-c("cve_ent", "pob_tot")), by = "mun_inegi") %>%
  select(nom_ent, nom_mun, everything())

## Efipem ----
efipem_clean_cap <- efipem_clean %>%
  filter(categoria %in% c("tema", "capitulo")) %>%
  select(mun_inegi, anio, tema, descripcion_categoria, valor) %>%
  unite(descripcion_categoria, tema:descripcion_categoria, sep = "_") %>%
  pivot_wider(names_from = descripcion_categoria,
              values_from = valor) %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x)))

indicadores_fs <- indicadores_fs %>%
  left_join(efipem_clean_cap, by = c("mun_inegi", "anio"))

## Encig ----

# Percepción
encig17_per_clean <- read_csv("./data/2_interim/encig17_per_clean.csv")
encig19_per_clean <- read_csv("./data/2_interim/encig19_per_clean.csv")
encig21_per_clean <- read_csv("./data/2_interim/encig21_per_clean.csv")

# Incidencia
encig17_inc_clean <- read_csv("./data/2_interim/encig17_inc_clean.csv")
encig19_inc_clean <- read_csv("./data/2_interim/encig19_inc_clean.csv")
encig21_inc_clean <- read_csv("./data/2_interim/encig21_inc_clean.csv")

# Redondeo de valores muy grandes
indicadores_fs <- indicadores_fs %>%
  mutate(across(c(graproes:im_2020, imn_2020), ~ as.numeric(.x))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 6)))


## Encig 17-21 Corrupcion ----
encig17 <- read_csv("./data/2_interim/encig17_clean.csv") %>%
  select(-starts_with("nom"))
encig19 <- read_csv("./data/2_interim/encig19_clean.csv")%>%
  select(-starts_with("nom"))
encig21 <- read_csv("./data/2_interim/encig21_clean.csv")%>%
  select(-starts_with("nom"))

indicadores_fs <- indicadores_fs %>%
  left_join(encig17, by = c("mun_inegi", "anio")) %>%
  left_join(encig19, by = c("mun_inegi", "anio")) %>%
  left_join(encig21, by = c("mun_inegi", "anio"))

# Diccionarios ----

## Indicadores efipem ----
efipem_dicc <- data.frame(
  mnemonico = indicadores_fs %>%
  select(starts_with(c("cs_", "bs_", "lrs_"))) %>%
  names(.)
  ) %>%
    mutate(indicador = c("Cash solvency: revenue per capita", 
                         "Cash solvency: total general fund revenues from own sources",
                         "Cash solvency: intergovernmental revenues divided by total revenues",
                         "Cash solvency: property tax divided by total revenues",
                       "Budget solvency: operating expenditures divided by total expenditures",
                       "Budget solvency: operating balance = total revenues divided by total expenditures",
                       "Budget solvency: total expenditures divided by population",
                       "Long run solvency: direct long term debt divided by population",
                       "Long run solvency: debt service divided by total revenues"),
         fuente = "INEGI: estadística de finanzas públicas estatales y municipales 2018 - 2021") %>%
  select(fuente, mnemonico, indicador)

## Censo 2020 ----
censo_dicc <- read_csv("./data/1_raw/diccionario_datos_iter_00CSV20.csv", skip = 3) %>%
  janitor::clean_names() %>%
  mutate(mnemonico = str_to_lower(mnemonico),
         fuente = "INEGI: censo población y vivienda 2020") %>%
  filter(mnemonico %in% names(censo_clean)) %>%
  select(fuente, mnemonico, indicador) %>%
  bind_rows(c("fuente" = "INEGI: censo población y vivienda 2020", "mnemonico" = "mun_inegi", "indicador" = "Clave única del municipio"))

## Indice de marginacion ----
imm_dicc <- imm_raw %>%
  slice(2) %>%
  select(-18) %>%
  pivot_longer(everything(), names_to = "indicador", values_to = "mnemonico") %>%
  mutate(fuente = "CONAPO: índice de marginación 2020",
         mnemonico = str_to_lower(mnemonico)) %>%
  select(fuente, mnemonico, indicador)

## Encig ----
encig_dicc <- data.frame(mnemonico = unlist(lapply(list(encig17 %>% select(starts_with("prop")), 
                                                 encig19 %>% select(starts_with("prop")), 
                                                 encig21 %>% select(starts_with("prop"))),
                                            names))) %>%
  mutate(fuente = c(rep("INEGI: ENCIG 2017", 3), rep("INEGI: ENCIG 2019", 3), rep("INEGI: ENCIG 2021", 3)), 
         indicador = case_when(mnemonico == "prop_corrup_per17" ~ "Proporción de la población del municipio que reporta percibir el fenómeno de la corrupción como 'muy frecuente' con base en datos de la ENCIG 2017",
                               mnemonico == "prop_corrup_inc17" ~ "Proporción de la población del municipio que reporta haber experimentado actos de corrupción durante el año 2017 con base en datos de la ENCIG 2017",
                               mnemonico == "prop_corrup5_inc17" ~"Proporción de la población del municipio que reporta haber experimentado actos de corrupción en los últimos 5 años con base en datos de la ENCIG 2017",
                               mnemonico == "prop_corrup_per19" ~ "Proporción de la población del municipio que reporta percibir el fenómeno de la corrupción como 'muy frecuente' con base en datos de la ENCIG 2019",
                               mnemonico == "prop_corrup_inc19" ~ "Proporción de la población del municipio que reporta haber experimentado actos de corrupción durante el año 2019 con base en datos de la ENCIG 2019",
                               mnemonico == "prop_corrup5_inc19" ~"Proporción de la población del municipio que reporta haber experimentado actos de corrupción en los últimos 5 años con base en datos de la ENCIG 2019",
                               mnemonico == "prop_corrup_per21" ~ "Proporción de la población del municipio que reporta percibir el fenómeno de la corrupción como 'muy frecuente' con base en datos de la ENCIG 2021",
                               mnemonico == "prop_corrup_inc21" ~ "Proporción de la población del municipio que reporta haber experimentado actos de corrupción durante el año 2021 con base en datos de la ENCIG 2021",
                               mnemonico == "prop_corrup5_inc21" ~"Proporción de la población del municipio que reporta haber experimentado actos de corrupción en los últimos 5 años con base en datos de la ENCIG 2021")) %>%
  select(fuente, mnemonico, indicador)

## Diccionario completo ----
dicc <- efipem_dicc %>%
  bind_rows(censo_dicc, imm_dicc, encig_dicc)

indicadores_dicc <- data.frame(mnemonico = names(indicadores_fs)) %>%
  left_join(dicc, by = "mnemonico") %>%
  select(fuente, mnemonico, indicador) %>%
  mutate(fuente = case_when(mnemonico == "anio" ~ "INEGI: estadística de finanzas públicas estatales y municipales 2018 - 2021",
                            mnemonico == "mun_tipo" ~ "Elaboración propia con base en: https://archivos.juridicas.unam.mx/www/bjv/libros/10/4513/8.pdf",
                            mnemonico == "pobtot" ~ "INEGI: censo población y vivienda 2020",
                            is.na(fuente) == TRUE ~ "INEGI: Finanzas Públicas Estatales y Municipales",
                            TRUE ~ fuente),
         indicador = case_when(mnemonico == "anio" ~ "Año de levantamiento de información sobre la estadística de las finanzas públicas estatales y municipales",
                               mnemonico == "mun_tipo" ~ "Tipo de municipio: Metropolitano > 150000 hab; Urbano >= 30000 y < 150000 hab; En transición de rural a urbano >= 10000 y < 30000 hab; Rural < 10000 hab",
                               mnemonico == "pobtot" ~ "Población total del municipio",
                               TRUE ~ indicador))

# Escritura ----
write_excel_csv(indicadores_fs, "./data/3_final/indicadores_fs.csv")
#write_excel_csv(indicadores_dicc, "./data/3_final/indicadores_dicc.csv")

NA_count <- function(df){
  df %>%
    summarise(across(everything(), ~ sum(is.na(.x)))) %>%
    pivot_longer(cols = everything(), names_to = "Variables", values_to = "NAs") %>%
    print(n = nrow(.))
}

NA_count(indicadores_fs)

rm(list = ls())