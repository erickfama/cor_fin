library(matrixStats)
library(tidyverse)
library(caret)
library(xgboost)
library(randomForest)


# Lectura
eg_inc <- fst::read_fst("./data/3_final/eg_inc_all_clean.fst") %>%
  filter(!is.na(prop_corrup))
training_samples <- readRDS("./data/2_interim/training_samples.RDS")
data <- eg_inc

# Limpieza
df <- data %>%
  mutate(across(starts_with("prop"), ~ ifelse(.x > 3.3, 1, 0), .names = "corrup"),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
  select(everything(), corrup, -c(mun_inegi, tema, nom_ent, nom_mun, frec_corrup, frec_no_corrup, prop_corrup, starts_with("concepto"), starts_with("capitulo"), starts_with("tema")))
table(df$corrup)
train_set  <- df[training_samples, ]
test_set <- df[-training_samples, ]
table(train_set$corrup)
table(test_set$corrup)

# Preprocessing
sds <- colSds(df %>% select(-corrup) %>% as.matrix())
sds <- sds[-which.max(sds)]
qplot(sds, bins = ncol(df))


df <- df[-nearZeroVar(df)]
sds <- colSds(df %>% as.matrix())
sds <- sds[-which.max(sds)]
qplot(sds, bins = ncol(df))

# xgbTree ----

# Training
model <- train(corrup ~ ., 
               data = train_set, 
               method = "rf",
               trControl = trainControl("cv", number = 5, repeats = 5),
               ntree = 150,
               nSamp = 5000)
beepr::beep(sound = 13)
y_hat <- model %>% predict(test_set)
ggplot(model, highlight = TRUE)
cm <- confusionMatrix(y_hat, reference = test_set$corrup)
cm
model$bestTune

fit_rf <- randomForest(corrup ~ .,
                       data = train_set[-327],
                       minNode = model$bestTune$mtry,
                       importance=TRUE,
                       proximity=TRUE)
plot(fit_rf)
y_hat <- fit_rf %>% predict(test_set)
cm_tree <- confusionMatrix(y_hat, reference = test_set$corrup)
cm_tree

data(iris)
set.seed(71)
iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        minnode = 35,
                        proximity=TRUE)
