### ML original percepcion ###

# Librerias ----
library(tidyverse)
library(caret)
library(xgboost)

# Lectura ----
egresos <- read_csv("./data/3_final/egresos19_mun_clean.csv") %>%
  mutate(corrup = ifelse(prop_corrup > 0, 1, 0),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
  select(starts_with("partida"), corrup)

# Train y test sets ----
set.seed(13)
training_samples <- egresos$corrup %>% 
  createDataPartition(p = 0.7, list = FALSE)
train_set  <- egresos[training_samples, ]
test_set <- egresos[-training_samples, ]

# Prevalencia de corruptos
print(paste("prev corruptos: ", round(mean(egresos$corrup == "corrupto"), 4)))

# xgbTree ----

# Train 
model_xgbTree <- train(
  corrup ~ ., data = train_set, method = "xgbTree",
  trControl = trainControl("cv", number = 5)
)
beepr::beep(sound = 2)

# Best tuning parameter
model_xgbTree$bestTune

# Make predictions on the test data
y_hat_xgbTree <- model_xgbTree %>% predict(test_set)

# Overall Accuracy
sum(y_hat_xgbTree == test_set$corrup)

# RSME 
y_hat_xgbTree_bin <- ifelse(y_hat_xgbTree == "corrupto", 1, 0)
y <- ifelse(test_set$corrup == "corrupto", 1, 0)
rmse_xgbTree <- sqrt(sum((y_hat_xgbTree_bin - y)^2)/nrow(test_set))

# F1-score
F1_xgbTree <- F_meas(y_hat_xgbTree, reference = test_set$corrup)

# Variable importance xgbTree
varImp(model_xgbTree)

# Confusion matrix
table(predicted = y_hat_xgbTree, actual = test_set$corrup)

test_set %>%
  mutate(y_hat = y_hat_xgbTree) %>%
  group_by(corrup) %>%
  summarize(accuracy = mean(y_hat == corrup))

cm <- confusionMatrix(y_hat_xgbTree, reference = test_set$corrup)
cm 

# Logit ----

# Train 
model_Logit <- glm(corrup ~ ., data = train_set, family = "binomial")

# Make predictions on the test data
y_hat_logit <- model_Logit %>% predict(test_set, type = "response") # type = "response" es muy importante 

y_hat_logit <- ifelse(y_hat_logit > 0.5, "corrupto", "no_corrupto") %>%
  factor()

# Binary predictions
y_hat_logit_bin <- ifelse(y_hat_logit == "corrupto", 1, 0)

# Overall Acurracy
mean(y_hat_logit == test_set$corrup)

# RMSE
rmse_logit <- sqrt(sum((y_hat_logit_bin - ifelse(test_set$corrup == "corrupto", 1, 0))^2)/nrow(test_set))

# F1-score
F1_logit <- F_meas(y_hat_logit, reference = test_set$corrup)

# Variable importance
varImp(model_Logit)
summary(model_Logit)

# Confusion Matrix
table(predicted = y_hat_logit, actual = test_set$corrup)
confusionMatrix(y_hat_logit, reference = test_set$corrup)


# ROC ----
egresos <- read_csv("./data/3_final/egresos19_mun_clean.csv")

props <- seq(0, mean(egresos$prop_corrup), length.out = 10)

roc_og <- map_df(props, function(p){
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
  
  list(method = "Percepción sin normalizar",
       prop = p,
       prevalence = round(mean(df$corrup == "corrupto"), 4),
       precision = precision(y_hat_xgbTree_per, test_set$corrup),
       recall = sensitivity(y_hat_xgbTree_per, test_set$corrup))
  
})
write.csv(roc_og, "./data/2_interim/roc_og.csv", row.names = FALSE)
beepr::beep(sound = 2)

best_cutoff_og <- roc_og$prop[which(roc_og$recall == max(roc_og$recall[which(roc_og$recall > roc_og$precision)]))]
