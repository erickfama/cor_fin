library(matrixStats)
library(tidyverse)
library(caret)
library(xgboost)
library(randomForest)


# Lectura
eg_inc <- fst::read_fst("./data/3_final/eg_inc_all_clean.fst") %>%
  filter(!is.na(prop_corrup)) %>%
  rename("partida_generica_herramientas_y_maquinas_herramienta" = "partida_generica_herramientas_y_maquinas-herramienta")
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
new_sds <- colSds(df %>% select(-corrup) %>% as.matrix())
new_sds <- sds[-which.max(sds)]
qplot(new_sds, bins = ncol(df))

# xgbTree ----

train_set  <- df[training_samples, ] 
test_set <- df[-training_samples, ]

# Training
model <- train(corrup ~ ., 
               data = train_set, 
               method = "rf",
               trControl = trainControl("repeatedcv", number = 10, repeats = 5),
               tuneGrid = expand.grid(mtry = c(1, seq(5, 100, 5))))

model$bestTune

# Entrenamiento final
final_model <- randomForest(corrup ~ .,
                            data = train_set,
                            minNode = model$bestTune$mtry)

plot(final_model)
y_hat <- predict(final_model, test_set)
final_cm <- confusionMatrix(y_hat, reference = test_set$corrup)
final_cm
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


