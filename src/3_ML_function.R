### ML function ####

# Librerías ----
library(tidyverse)
library(caret)
library(xgboost)

# Funcion ----

gb_model <- function(data, cutoff = 0, seed = TRUE, algorithm = "xgbTree"){
  if(seed == TRUE){
    set.seed(123)
  }
  
  # Lectura 
  if(is.character(data) == TRUE){
  df <- read_csv(data) %>%
    mutate(across(starts_with("prop"), ~ ifelse(.x > cutoff, 1, 0), .names = "corrup"), # RESOLVER PROBLEMA DE STARTS WITH
           corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
    select(everything(), corrup, -c(mun_inegi, tema, nom_ent, nom_mun, frec_corrup, frec_no_corrup, prop_corrup, starts_with("concepto"), starts_with("capitulo"), starts_with("tema")))
  } else {
    df <- data %>%
      mutate(across(starts_with("prop"), ~ ifelse(.x > cutoff, 1, 0), .names = "corrup"),
             corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
      select(everything(), corrup, -c(mun_inegi, tema, nom_ent, nom_mun, frec_corrup, frec_no_corrup, prop_corrup, starts_with("concepto"), starts_with("capitulo"), starts_with("tema")))
  }
  
  # Train y test sets 
  training_samples <- df$corrup %>% 
    createDataPartition(p = 0.7, list = FALSE)
  train_set  <- df[training_samples, ]
  test_set <- df[-training_samples, ]
  
  # Training
  model <- train(corrup ~ ., 
                 data = train_set, 
                 method = algorithm,
                 trControl = trainControl("cv", number = 5))
  
  # Ajuste con test set
  y_hat <- model %>% predict(test_set)
  
  # F1-score 
  F1 <- F_meas(y_hat, reference = test_set$corrup)
  
  # Variable importance 
  var_imp <- varImp(model)
  
  # Confusion Matrix
  cm <- confusionMatrix(y_hat, reference = test_set$corrup)
  print(var_imp)
  print(cm)
  
  beepr::beep(sound = 2)
  
  results <- list(var_imp = var_imp,
       cm = cm,
       model = model)
  return(results)
}

# Best cutoff
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

