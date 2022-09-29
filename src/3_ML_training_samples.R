### Train and test sets ###

# Librerías 
library(tidyverse)
library(caret)
library(fst)

# Lectura ----
eg_inc <- read_fst("./data/3_final/eg_inc_all_clean.fst")

eg_inc <- eg_inc %>%
  mutate(across(starts_with("prop"), ~ ifelse(.x > 8, 1, 0), .names = "corrup"),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
  select(everything(), corrup, -c(mun_inegi, tema, nom_ent, nom_mun, frec_corrup, frec_no_corrup, prop_corrup, starts_with("concepto"), starts_with("capitulo"), starts_with("tema")))

# Train y test sets 
training_samples <- eg_inc$corrup %>% 
  createDataPartition(p = 0.7, list = FALSE)

# Se guardan los índices para el train y test set
saveRDS(training_samples, "./data/2_interim/training_samples.RDS")




