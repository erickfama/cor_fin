### Modelos ###

# Librerias ----
library(tidyverse)
library(caret)
library(xgboost)

# Lectura ----
egresos <- read_csv("./data/3_final/egresos19_per_clean.csv")

# ROC ----
props <- seq(0, mean(egresos$prop_corrup), length.out = 10)

roc_per <- map_df(props, function(p){
  # Dummy Corrupcion ----
  df <- egresos %>%
    mutate(corrup = ifelse(prop_corrup > p, 1, 0),
           corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
    select(starts_with("partida"), corrup)
  
  # Train y test sets ----
  set.seed(123)
  training_samples <- df$corrup %>% 
    createDataPartition(p = 0.7, list = FALSE)
  train_set  <- df[training_samples, ]
  test_set <- df[-training_samples, ]
  
  # xgbTree ----
  
  # Train 
  model_xgbTree_per <- train(
    corrup ~ ., data = train_set, method = "xgbTree",
    trControl = trainControl("cv", number = 5)
  )
  # Make predictions on the test data
  y_hat_xgbTree_per <- model_xgbTree_per %>% predict(test_set)
  
  list(method = "Percepción",
       prop = p,
       prevalence = round(mean(df$corrup == "corrupto"), 4),
       precision = precision(y_hat_xgbTree_per, test_set$corrup),
       recall = sensitivity(y_hat_xgbTree_per, test_set$corrup))
  
})
write.csv(roc_per, "./data/2_interim/roc_per.csv", row.names = FALSE)
beepr::beep(sound = 2)

# Cutoff ----
best_cutoff_per <- roc_per$prop[which(roc_per$recall == max(roc_per$recall[which(roc_per$recall > roc_per$precision)]))]

# Set perception ----
df <- egresos %>%
  mutate(corrup = ifelse(prop_corrup > best_cutoff_per, 1, 0),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
  select(starts_with("partida"), corrup)

# Train y test sets ----
set.seed(123)
training_samples <- df$corrup %>% 
  createDataPartition(p = 0.7, list = FALSE)
train_set  <- df[training_samples, ]
test_set <- df[-training_samples, ]

# Prevalencia de corruptos
print(paste("prev corruptos: ", round(mean(df$corrup == "corrupto"), 4)))

# xgbTree ----

# Train 
model_xgbTree_per <- train(
  corrup ~ ., data = train_set, method = "xgbTree",
  trControl = trainControl("cv", number = 5)
)
beepr::beep(sound = 2)

# Best tuning parameter
model_xgbTree_per$bestTune

# Make predictions on the test data
y_hat_xgbTree_per <- model_xgbTree_per %>% predict(test_set)

# Overall Accuracy
sum(y_hat_xgbTree == test_set$corrup)

# RSME 
y_hat_xgbTree_bin <- ifelse(y_hat_xgbTree_per == "corrupto", 1, 0)
y <- ifelse(test_set$corrup == "corrupto", 1, 0)
rmse_xgbTree_per <- sqrt(sum((y_hat_xgbTree_bin - y)^2)/nrow(test_set))

# F1-score
F1_xgbTree_per <- F_meas(y_hat_xgbTree_per, reference = test_set$corrup)

# Variable importance xgbTree
varImp(model_xgbTree_per)

# Confusion matrix
table(predicted = y_hat_xgbTree_per, actual = test_set$corrup)

test_set %>%
  mutate(y_hat = y_hat_xgbTree_per) %>%
  group_by(corrup) %>%
  summarize(accuracy = mean(y_hat == corrup))

cm_per <- confusionMatrix(y_hat_xgbTree_per, reference = test_set$corrup)
cm_per

