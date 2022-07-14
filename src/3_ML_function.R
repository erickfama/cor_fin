### ML function ####

# Librerías ----
library(tidyverse)
library(caret)
library(xgboost)

# Funcion ----

gb_model <- function(data, cutoff = 0, seed = TRUE){
  if(seed == TRUE){
    set.seed(123)
  }
  
  # Lectura 
  if(is.character(data) == TRUE){
  df <- read_csv(data) %>%
    mutate(across(starts_with("prop"), ~ ifelse(.x > cutoff, 1, 0), .names = "corrup"), # RESOLVER PROBLEMA DE STARTS WITH
           corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
    select(pobtot, graproes, starts_with("partida"), corrup)
  } else {
    df <- data %>%
      mutate(across(starts_with("prop"), ~ ifelse(.x > cutoff, 1, 0), .names = "corrup"),
             corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
      select(pobtot, graproes, starts_with("partida"), corrup)
  }
  
  # Train y test sets 
  training_samples <- df$corrup %>% 
    createDataPartition(p = 0.7, list = FALSE)
  train_set  <- df[training_samples, ]
  test_set <- df[-training_samples, ]
  
  # Training
  model <- train(corrup ~ ., 
                 data = train_set, 
                 method = "xgbTree",
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
       cm = cm)
  return(results)
}

