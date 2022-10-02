### Optimization ###

# Librerias ----
library(fst)
source("./src/3_Optim_function.R")

# Egresos ----

## Lectura ----
eg_inc <- read_fst("./data/3_final/eg_inc_all_clean.fst") %>% 
  filter(!is.na(prop_corrup))
training_samples <- readRDS("./data/2_interim/training_samples.RDS")

## Metricas ----
eval_eg_inc <- Optim(eg_inc, method_name = "Incidencia", 
                     props_values = seq(0, 11, length.out = 11), 
                     training_samples = training_samples, 
                     seed = FALSE,
                    algorithm = "xgbTree")
eval_eg <- rbind(eval_eg_inc)

## Escritura ----
write_csv(eval_eg, "./data/2_interim/eval_eg.csv")

## F1 score optim ----
eval_eg <- read_csv("./data/2_interim/eval_eg.csv", locale = locale(encoding = "UTF-8"))

eval_eg %>%
  ggplot(aes(prop, balanced_accuracy, color = method, label = prop)) +
  geom_point() +
  geom_line() +
  ggrepel::geom_text_repel(size = 4) +
  scale_x_continuous(limits = c(0, 12)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~ method) +
  labs(title = "Egresos",
       x = "Umbral de proporción") +
  theme_bw() + 
  theme(legend.position = "none")

# Ingresos ----

## Lectura ----
ig_inc <- read_fst("./data/3_final/ig_inc_all_clean.fst") %>%
  filter(!is.na(prop_corrup))

## Metricas ----
eval_ig_inc <- Optim(ig_inc, method_name = "Incidencia", props_values = seq(0, 11, length.out = 21), training_samples = training_samples)
eval_ig <- rbind(eval_ig_inc)

## Escritura ----
write.csv(eval_ig, "./data/2_interim/eval_ig.csv", row.names = FALSE)

## F1 score optim ----
eval_ig <- read_csv("./data/2_interim/eval_ig.csv", locale = locale(encoding = "latin1"))

eval_ig %>%
  ggplot(aes(prop, balanced_accuracy, color = method, label = prop)) +
  geom_point() +
  geom_line() +
  ggrepel::geom_text_repel(size = 4) +
  scale_x_continuous(limits = c(0, 12)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~ method) +
  labs(title = "Ingresos",
       x = "Umbral de proporción") +
  theme_bw() + 
  theme(legend.position = "none")

