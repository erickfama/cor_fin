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
  mutate(across(starts_with("prop"), ~ ifelse(.x > 10, 1, 0), .names = "corrup"),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) 

# Limpieza
df <- data %>%
  mutate(across(starts_with("prop"), ~ ifelse(.x > mean(eg_inc$prop_corrup), 1, 0), .names = "corrup"),
         corrup = factor(corrup, levels = c(1, 0), labels = c("corrupto", "no_corrupto"))) %>%
  select(everything(), corrup, -c(mun_inegi, tema, nom_ent, nom_mun, frec_corrup, frec_no_corrup, prop_corrup, starts_with("partida"), starts_with("capitulo"), starts_with("tema"))) %>%
  rename("concepto_adeudos_de_ejercicios_fiscales_anteriores" = "concepto_adeudos_de_ejercicios_fiscales_anteriores_(adefas)")
mean(eg_inc$prop_corrup)
table(df$corrup)

# Preprocessing
sds <- colSds(df %>% select(-corrup) %>% as.matrix())
sds <- sds[-which.max(sds)]
qplot(sds, bins = ncol(df))

# Training and test sets
train_set  <- df[training_samples, ] 
test_set <- df[-training_samples, ]

vars_partidad <- data %>% select(matches("^partida_generica")) %>% var()

vars_partidad %>%
  melt() %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red")

partidas_names <- data %>% select(matches("^partida_generica")) %>% names()

vars_df <- data.frame(partida = partidas_names, var_p = vars_partidad) %>%
  filter(var_p > 5)

data %>%
  filter(partida_generica_primas_de_vacaciones_dominical_y_gratificacion_de_fin_de_ano < 2000 & partida_generica_dependencias_diversas < 5000) %>%
  ggplot(aes(partida_generica_primas_de_vacaciones_dominical_y_gratificacion_de_fin_de_ano, partida_generica_dependencias_diversas, color = corrup)) +
  geom_point()
  
df <- df[-nearZeroVar(df)]
# new_sds <- colSds(df %>% select(-corrup) %>% as.matrix())
# new_sds <- sds[-which.max(sds)]
# qplot(new_sds, bins = ncol(df))

# Tuning ----
nodesize <- seq(1, 15, 1)

acc <- sapply(nodesize, function(ns){
  train(corrup ~ ., method = "rf",
        data = train_set,
        tuneGrid = data.frame(mtry = 10),
        nodesize = ns)$results$Accuracy
})
beepr::beep()
qplot(nodesize, acc)

# Training ----

model <- train(corrup ~ ., 
               data = train_set, 
               method = "rf",
               trControl = trainControl("repeatedcv", number = 10, repeats = 5),
               tuneGrid = expand.grid(mtry = c(1, seq(5, 100, 5))),
               nodesize = nodesize[which.max(acc)])

model$bestTune
beepr::beep(sound = 2)

# Entrenamiento final
final_model <- randomForest(corrup ~ .,
                            data = train_set,
                            minNode = model$bestTune$mtry,
                            nodesize = nodesize[which.max(acc)],
                            ntree = 1000)

plot(final_model)
y_hat <- predict(final_model, test_set)
final_cm <- confusionMatrix(y_hat, reference = test_set$corrup)
final_cm
beepr::beep(sound = 13)

eg_inc_predicted <- read_csv("./data/3_final/eg_inc_predicted.csv")
table(eg_inc_predicted$corrup)
