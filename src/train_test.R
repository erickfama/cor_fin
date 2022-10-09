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
data <- eg_inc %>%
  mutate(across(starts_with("prop"), ~ ifelse(.x > 0, 1, 0), .names = "corrup"),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) 

# EDA 
data %>%
  ggplot(aes(prop_corrup)) +
  geom_histogram(bins = 233)
sum(data$prop_corrup == 0)

# Prevalencia
table(data$corrup)

# Limpieza
df <- data %>%
  mutate(across(starts_with("prop"), ~ ifelse(.x > 0, 1, 0), .names = "corrup"),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
  select(everything(), corrup, -c(mun_inegi, tema, nom_ent, nom_mun, frec_corrup, frec_no_corrup, prop_corrup, starts_with("capitulo"), starts_with("concepto"), starts_with("tema"))) 
table(df$corrup)


# Training and test sets
train_set  <- df[training_samples, ] 
test_set <- df[-training_samples, ]

# Se eliminan las variables con varianza nula
df <- df[-nearZeroVar(df)]

# Tuning ----
nodesize <- seq(1, 15, 1)

acc <- sapply(nodesize, function(ns){
  train(corrup ~ ., method = "rf",
        data = train_set,
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy
})
beepr::beep()
qplot(nodesize, acc)

mtrysize <- seq(1, )
acc <- sapply()

# Training ----

model <- train(corrup ~ ., 
               data = train_set, 
               method = "rf",
               trControl = trainControl("repeatedcv", number = 10, repeats = 5),
               tuneGrid = expand.grid(mtry = c(1, 2, 3, 4, 5, 10, 15, 20, 30, 40)),
               nodesize = nodesize[which.max(acc)])

model$bestTune
beepr::beep(sound = 2)

# Entrenamiento final
final_model <- randomForest(corrup ~ .,
                            data = train_set,
                            minNode = model$bestTune$mtry,
                            nodesize = nodesize[which.max(acc)],
                            ntree = 10000)

plot(final_model)
y_hat <- predict(final_model, test_set)
final_cm <- confusionMatrix(y_hat, reference = test_set$corrup)
final_cm
final_cm$byClass["F1"]
beepr::beep(sound = 13)

# escritura modelo
saveRDS(final_model, "./models/model_eg_rf.rds")

# Matriz de confusion
saveRDS(final_cm, "./models/model_eg_rf_cm.rds")

varImp(model_eg_inc) %>%
  add_rownames(var = "Variable") %>%
  arrange(desc(Overall)) %>%
  rename("Importance" = "Overall") %>%
  filter(!str_detect(Variable, "^[a-z]\\d+[a-z]"))
  