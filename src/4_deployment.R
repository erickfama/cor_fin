### Deployment ###

# Librerias ----
library(tidyverse)
library(fst)
library(caret)
source("./src/3_ML_modeling.R", encoding = "UTF-8")

# Lectura ----
eg_per <- read_fst("./data/3_final/egresos19_per_clean.fst")
eg_inc <- read_fst("./data/3_final/egresos19_inc_clean.fst")

ig_per <- read_fst("./data/3_final/ingresos19_per_clean.fst")
ig_inc <- read_fst("./data/3_final/ingresos19_inc_clean.fst")

# cutoff 
best_cutoff <- function(tipo_fin, method){
  if(tipo_fin == "eg"){
    cutoff <- read_csv("./data/2_interim/eval_eg.csv", locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
      select(method, best_cutoff) 
    if(method == "per"){
      value <- cutoff %>% filter(method == "Percepción") %>% select(best_cutoff) %>% pull() %>% unique() %>% ifelse(is.null(.), 0, .)
    } else {
      value <- cutoff %>% filter(method == "Incidencia") %>% select(best_cutoff) %>% pull() %>% unique() %>% ifelse(is.null(.), 0, .)
    }
  } 
  if(tipo_fin == "ig"){
    cutoff <- read_csv("./data/2_interim/eval_ig.csv", locale = locale(encoding = "latin1"), show_col_types = FALSE) %>%
      select(method, best_cutoff) 
    if(method == "per"){
      value <- cutoff %>% filter(method == "Percepción") %>% select(best_cutoff) %>% pull() %>% unique() %>% ifelse(is.null(.), 0, .)
    } else {
      value <- cutoff %>% filter(method == "Incidencia") %>% select(best_cutoff) %>% pull() %>% unique() %>% ifelse(is.null(.), 0, .)
    }
  }
  value <- ifelse(is.null(value), 0, value)
  return(value)
}
# Egresos ----

## Percepcion
eg_per_corrup_hat <- model_eg_per$model %>%
  predict(eg_per %>% select(everything(), -c(mun_inegi, tema, nom_ent, nom_mun, frec_corrup, frec_no_corrup, prop_corrup, starts_with("concepto"), starts_with("capitulo"), starts_with("tema"))))

eg_per_predicted <- eg_per %>%
  mutate(across(starts_with("prop"), ~ ifelse(.x > best_cutoff("eg", "per"), 1, 0), .names = "corrup"),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto")), 
         corrup_hat = eg_per_corrup_hat)

## Incidencia
eg_inc_corrup_hat <- model_eg_inc$model %>%
  predict(eg_inc %>% select(everything(), -c(mun_inegi, tema, nom_ent, nom_mun, frec_corrup, frec_no_corrup, prop_corrup, starts_with("concepto"), starts_with("capitulo"), starts_with("tema"))))

eg_inc_predicted <- eg_inc %>%
  mutate(across(starts_with("prop"), ~ ifelse(.x > best_cutoff("eg", "inc"), 1, 0), .names = "corrup"),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto")),
         corrup_hat = eg_inc_corrup_hat)

### Escritura
write_csv(eg_per_predicted, "./data/3_final/eg_per_predicted.csv")
write_csv(eg_inc_predicted, "./data/3_final/eg_inc_predicted.csv")

# Ingresos ----

## Percepcion
ig_per_corrup_hat <- model_ig_per$model %>%
  predict(ig_per %>% select(everything(), -c(mun_inegi, tema, nom_ent, nom_mun, frec_corrup, frec_no_corrup, prop_corrup, starts_with("concepto"), starts_with("capitulo"), starts_with("tema"))))

ig_per_predicted <- ig_per %>%
  mutate(across(starts_with("prop"), ~ ifelse(.x > best_cutoff("ig", "per"), 1, 0), .names = "corrup"),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto")),
         corrup_hat = ig_per_corrup_hat)

## Incidencia
ig_inc_corrup_hat <- model_ig_inc$model %>%
  predict(ig_inc %>% select(everything(), -c(mun_inegi, tema, nom_ent, nom_mun, frec_corrup, frec_no_corrup, prop_corrup, starts_with("concepto"), starts_with("capitulo"), starts_with("tema"))))

ig_inc_predicted <- ig_inc %>%
  mutate(across(starts_with("prop"), ~ ifelse(.x > best_cutoff("ig", "inc"), 1, 0), .names = "corrup"),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto")),
         corrup_hat = ig_inc_corrup_hat)

### Escritura
write_csv(ig_per_predicted, "./data/3_final/ig_per_predicted.csv")
write_csv(ig_inc_predicted, "./data/3_final/ig_inc_predicted.csv")