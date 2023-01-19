### Indicadores de solvencia financiera ###

# Librerias ----
library(tidyverse)

# Lectura ----
efipem_clean <- read_csv("./data/2_interim/efipem_clean.csv")

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
  summarise(total_general_fund_rev_own = sum(valor))

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
  filter(tema == "ingresos" & categoria %in% c("capitulo", "concepto") & descripcion_categoria %in% c("impuestos_sobre_el_patrimonio")) %>%
  group_by(mun_inegi, anio) %>%
  summarise(property_tax = valor) %>%
  ungroup() %>%
  left_join(total_revs %>% select(mun_inegi, anio, valor), by = c("mun_inegi", "anio")) %>%
  mutate(cs_propety_tax_total_revs = property_tax / valor)

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

### Operating balance: total revenues / total expenditures ----
operating_balance <- data.frame(mun_inegi = total_exp$mun_inegi,
                                anio = total_exp$anio, 
                                total_exp = total_exp$total_exp, 
                                total_revs = total_revs$valor) %>%
  mutate(bs_operating_balance = total_revs/total_exp) # Resulta interesante, estos registros demuestran que todos los ingresos son gastados
 
### Total expenditures per capita ----
expenditures_perCapita <- total_exp %>%
  mutate(bs_expenditire_perCapita = total_exp / pobtot)

## Conjunto de indicadores Budget Solvency ----
budget_solvency <- operating_exp %>% 
  left_join(operating_balance, by = c("mun_inegi", "anio")) %>%
  left_join(expenditures_perCapita, by = c("mun_inegi", "anio"))

## Long-run solvency ----

### Direct long-term debt / Population ----

direct_long_term_debt <- efipem_clean %>%
  filter(tema == "ingresos" & descripcion_categoria == "financiamiento") %>%
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
