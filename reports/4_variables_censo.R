vars_censo <- eg_inc_predicted %>%
  select(starts_with(c("a", "h", "i", "j", "k"))) %>% 
  names() %>%
  str_extract("^[^_]+(?=_)")

dicc_ce <- read_csv("./data/1_raw/diccionario_de_datos_ce2019.csv") %>%
  janitor::clean_names() %>%
  mutate(columna = str_to_lower(columna)) %>%
  filter(columna %in% vars_censo) %>%
  mutate(descripcion = str_extract(descripcion, "^[^:]+")) %>%
  select(`Variables censo económico 2019` = descripcion)

write_csv(dicc_ce, "./reports/variables_censo.csv")

