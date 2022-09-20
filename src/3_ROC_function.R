### ROC function ### 

# Librerias ----
library(tidyverse)
library(caret)
library(xgboost)

# ROC ----

ROC <- function(data, method_name, algorithm = "xgbTree", props_values, seed = TRUE){
  
  # Lectura 
  if(is.character(data) == TRUE){
    data <- read_csv(data)
  } else {
    data <- data
  }
  
  props <- props_values
  
  roc <- map_df(props, function(p){
    
    # Data set
    df <- data %>%
      mutate(across(starts_with("prop"), ~ ifelse(.x > p, 1, 0), .names = "corrup"),
             corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
      select(everything(), corrup, -c(mun_inegi, tema, nom_ent, nom_mun, frec_corrup, frec_no_corrup, prop_corrup, starts_with("concepto"), starts_with("capitulo"), starts_with("tema")))
    
    # Train y test sets ----
    if(seed == TRUE){
      set.seed(123)
    }
    training_samples <- df$corrup %>% 
      createDataPartition(p = 0.7, list = FALSE)
    train_set  <- df[training_samples, ]
    test_set <- df[-training_samples, ]
    
    # xgbTree ----
    
    # Training
    model <- train(corrup ~ ., 
                   data = train_set, 
                   method = algorithm,
                   trControl = trainControl("cv", number = 5))
    
    # Ajuste con test set
    y_hat <- model %>% predict(test_set)
    
    list(method = method_name,
         prop = p,
         prevalence = round(mean(df$corrup == "corrupto"), 4),
         specifiticy = specificity(y_hat, test_set$corrup),
         FPR = 1 - specificity(y_hat, test_set$corrup),
         TPR = sensitivity(y_hat, test_set$corrup),
         precision = precision(y_hat, test_set$corrup),
         F1_score = F_meas(y_hat, test_set$corrup))
  })
  # Best cutoff
  # roc$best_cutoff <- roc$prop[which(roc$prevalence == max(roc$prevalence[which(roc$F1_score == max(roc$F1_score[which(roc$recall >= roc$specifiticy)]))]))]
  beepr::beep(sound = 2)
  return(roc)
}
