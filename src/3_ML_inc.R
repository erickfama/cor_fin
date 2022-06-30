### ML incidencia per capita ###

# Librerias ----
library(tidyverse)
library(caret)
library(xgboost)

# Lectura ----
egresos <- read_csv("./data/3_final/egresos19_inc_clean.csv")

# ROC ----
props <- seq(0, mean(egresos$prop_inc_corrup), length.out = 10)

roc_inc <- map_df(props, function(p){
    # Dummy Corrupcion ----
    df <- egresos %>%
    mutate(corrup = ifelse(prop_inc_corrup > p, 1, 0),
           corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
      select(pobtot, graproes, starts_with("partida"), corrup)
  
    # Train y test sets ----
    set.seed(123)
    training_samples <- df$corrup %>% 
      createDataPartition(p = 0.7, list = FALSE)
    train_set  <- df[training_samples, ]
    test_set <- df[-training_samples, ]
    
    # xgbTree ----
    
    # Train 
    model_xgbTree_inc <- train(
      corrup ~ ., data = train_set, method = "xgbTree",
      trControl = trainControl("cv", number = 5)
    )
    # Make predictions on the test data
    y_hat_xgbTree_inc <- model_xgbTree_inc %>% predict(test_set)
    
    list(method = "Incidencia",
         prop = p,
         prevalence = round(mean(df$corrup == "corrupto"), 4),
         precision = precision(y_hat_xgbTree_inc, test_set$corrup),
         recall = sensitivity(y_hat_xgbTree_inc, test_set$corrup))
          
})
write.csv(roc_inc, "./data/2_interim/roc_inc.csv", row.names = FALSE)
beepr::beep(sound = 2)

# Se elige la mejor proporcion como cutoff
best_cutoff_inc <- roc_inc$prop[which(roc_inc$recall == max(roc_inc$recall[which(roc_inc$recall > roc_inc$precision)]))]

# Dummy Corrupcion ----
df <- egresos %>%
  mutate(corrup = ifelse(prop_inc_corrup > best_cutoff_inc, 1, 0),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
  select(pobtot, graproes, starts_with("partida"), corrup)

# Train y test sets ----
set.seed(123)
training_samples <- df$corrup %>% 
  createDataPartition(p = 0.7, list = FALSE)
train_set  <- df[training_samples, ]
test_set <- df[-training_samples, ]

# xgbTree ----

# Train 
model_xgbTree_inc <- train(
  corrup ~ ., data = train_set, method = "xgbTree",
  trControl = trainControl("cv", number = 5)
)
# Make predictions on the test data
y_hat_xgbTree_inc <- model_xgbTree_inc %>% predict(test_set)

# Best tuning parameter
model_xgbTree_inc$bestTune

# Overall Accuracy
sum(y_hat_xgbTree_inc == test_set$corrup)

# RSME 
y_hat_xgbTree_bin <- ifelse(y_hat_xgbTree_inc == "corrupto", 1, 0)
y <- ifelse(test_set$corrup == "corrupto", 1, 0)
rmse_xgbTree <- sqrt(sum((y_hat_xgbTree_bin - y)^2)/nrow(test_set))

# F1-score
F1_xgbTree_inc <- F_meas(y_hat_xgbTree_inc, reference = test_set$corrup)

# Variable importance xgbTree
varImp(model_xgbTree)

# Confusion matrix
table(predicted = y_hat_xgbTree_inc, actual = test_set$corrup)

test_set %>%
  mutate(y_hat = y_hat_xgbTree_inc) %>%
  group_by(corrup) %>%
  summarize(accuracy = mean(y_hat == corrup))

cm_inc <- confusionMatrix(y_hat_xgbTree_inc, reference = test_set$corrup)
cm_inc