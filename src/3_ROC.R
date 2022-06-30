### ROC curves ###

# Librerias ----
library(tidyverse)

# Lectura ----
roc_og <- read_csv("./data/2_interim/roc_og.csv", locale = locale(encoding = "latin1"))
roc_per <- read_csv("./data/2_interim/roc_per.csv", locale = locale(encoding = "latin1"))
roc_inc <- read_csv("./data/2_interim/roc_inc.csv", locale = locale(encoding = "latin1"))

# Plots ----

# Merge data frames 
roc <- rbind(roc_og, roc_per, roc_inc)

# plot 
roc %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_point() +
  geom_line() +
  # ggrepel::geom_text_repel(aes(label = round(prop, 2))) +
  scale_x_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 5)) +
  scale_color_discrete(name = "Modelo") +
  theme_bw() +
  labs(title = "Curvas ROC",
       x = "Recall (sensitivity)",
       y = "Precision",
       caption = "Elaboración propia.")

