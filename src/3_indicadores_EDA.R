### EDA Indicadores ###

# Librerias ----
library(tidyverse)
library(ggplot2)

# Lectura ----
indicadores_fs <- read_csv("./data/3_final/indicadores_fs.csv")

# EDA ----

# Cantidad de NAs por indicador
indicadores_fs %>%
  select(-mun_inegi) %>%
  group_by(anio) %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(names_to = "indicador", values_to = "NA_count", cols = -c(anio)) %>%
  ggplot(aes(NA_count, indicador)) +
  geom_col(width = 0.1) +
  geom_point() + 
  facet_wrap(~anio) +
  labs(title = "Cantidad de valores faltantes por indicador y año") +
  theme_bw()

# Estadística descriptiva de los indicadores
indicadores_fs %>%
  select(-mun_inegi) %>%
  pivot_longer(names_to = "indicador", values_to = "valor", cols = -anio) %>%
  group_by(anio, indicador) %>%
  summarise(min = min(valor, na.rm = TRUE),  
            media = mean(valor, na.rm = TRUE),
            max = max(valor, na.rm = TRUE),
            sd = sd(valor, na.rm = TRUE))

# PCA
lel <- prcomp(~ cs_revenue_perCapita + cs_total_general_fund + cs_intergov_revs_total_revs + cs_propety_tax_total_revs, data = indicadores_fs)
summary(lel)
