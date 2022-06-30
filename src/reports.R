### Reports ###

# Librerias ----
library(tidyverse)

# Reporte 2 ----

# Seccion de comparacion de modelos 
comp_models <- data.frame(Modelo = c("Percepción sin normalizar", "Percepción", "Incidencia"),
                          Umbral = round(c(best_cutoff_og, best_cutoff_per, best_cutoff_inc), 2),
                          Sensibilidad = c(round(c(cm$byClass[c("Sensitivity")], cm_per$byClass[c("Sensitivity")], cm_inc$byClass[c("Sensitivity")]), 2)),
                          Especificidad = c(round(c(cm$byClass[c("Specificity")], cm_per$byClass[c("Specificity")], cm_inc$byClass[c("Specificity")]), 2)),
                          F1_score = round(c(F1_xgbTree, F1_xgbTree_per, F1_xgbTree_inc), 2),
                          Precisión_balanceada = round(c(cm$byClass[c("Balanced Accuracy")], cm_per$byClass[c("Balanced Accuracy")], cm_inc$byClass[c("Balanced Accuracy")]), 2))
write.csv(comp_models, "./data/reports/2_comp_models.csv", row.names = FALSE)
