### EDA ###

# librerias ----
library(tidyverse)

# Egresos ----

# lectura 
egresos_per <- read_csv("./data/3_final/egresos19_per_clean.csv") %>%
  mutate(medida = "percepción")
egresos_inc <- read_csv("./data/3_final/egresos19_inc_clean.csv") %>%
  mutate(medida = "incidencia")
egresos <- rbind(egresos_per, egresos_inc)

# Histogramas de frecuencias de proporciones
egresos %>%
  ggplot(aes(x = prop_corrup, fill = medida)) +
  geom_histogram(color = "black", bins = 20) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  facet_wrap(~medida) + 
  labs(title = "Frecuencia de las proporciones calculadas de cada municipio",
       x = "Proporción de encuestados",
       y = "Frecuencia") +
  theme_bw()
