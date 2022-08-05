### Limpieza Censo Economico 2019 ###

# Librerias -----
library(tidyverse)
library(rvest)

# Abreviaturas de los estados ----
url_abrevs <- "https://es.wikipedia.org/wiki/Plantilla:Abreviaciones_de_los_estados_de_M%C3%A9xico"
abreviaturas <- url_abrevs %>%
  read_html() %>%
  html_table() %>%
  magrittr::extract(1) %>%
  .[[1]] %>%
  pull(Variable) %>%
  str_to_lower() %>%
  str_remove_all("\\.") %>%
  stringi::stri_replace_all_regex(c("edo méx\\[d\\]", "q roo\\[f\\]"), c("mex", "qroo"), vectorize = FALSE) %>%
  .[-33] %>%
  replace(list = c(15, 23), values = c("mex", "qroo"))

# Descarga ----

## Censo economico ----

# Urls 
url_base <- "https://www.inegi.org.mx/contenidos/programas/ce/2019/Datosabiertos/ce2019_ags_csv.zip"
url_estados <- sapply(abreviaturas, function(i){
  str_replace(url_base, "ags", i)
})

# Nombre de archivos csv
csv_files <- str_extract(url_estados, "ce2019_[a-z]+_csv\\.zip$") %>%
  str_replace("_csv", ".csv") %>%
  str_replace(".zip", "")

# Descarga todos csv ----
censo_estados <- purrr::map2(url_estados, csv_files, function(url, csv){
  temp <- tempfile()
  download.file(url, temp)
  df <- read_csv(unz(temp, paste("conjunto_de_datos/", csv, sep = "")), col_types = "c")
})

# Merge en un gran DF
censo_econ_estados <- map_dfr(censo_estados, ~ .x %>%
                                mutate(across(everything(), as.character)))

## Descriptores identificadores actividades economicas ----

id_act <- readxl::read_xlsx("./data/1_raw/scian_2018_categorias_y_productos.xlsx", skip = 1) %>%
  janitor::clean_names() %>%
  filter(!is.na(codigo) & codigo < 100) %>%
  mutate(codigo = as.character(codigo))

# Escritura ----
write.csv(censo_econ_estados, "./data/1_raw/censo_econ2019_raw.csv", row.names = FALSE)
fst::write.fst(censo_econ_estados, "./data/1_raw/censo_econ2019_raw.fst")

# Lectura ----
censo_econ2019_raw <- fst::read_fst("./data/1_raw/censo_econ2019_raw.fst")

# Limpieza ----
censo_econ2019_clean <- censo_econ2019_raw %>%
  select(ENTIDAD, MUNICIPIO, CODIGO, ID_ESTRATO, UE, A111A, A211A, A700A,	A800A,	H000D,	H001A,	H010D,	J000A,	J300A,	J500A,	K311A) %>%
  janitor::clean_names() %>%
  filter(!is.na(municipio) & codigo %in% id_act$codigo & !is.na(a111a)) %>%
  left_join(select(id_act, codigo, titulo), by = "codigo") %>%
  mutate(mun_inegi = paste(entidad, municipio, sep = ""),
         across(c(a111a:k311a), as.numeric)) %>%
  select(mun_inegi, codigo, titulo, a111a:k311a) %>%
  group_by(mun_inegi, codigo, titulo) %>%
  summarise(across(c(a111a:k311a), sum)) %>%
  pivot_wider(id_cols = mun_inegi, names_from = codigo, values_from = a111a:k311a) %>%
  mutate(across(1:last_col(), ~ ifelse(is.na(.x) == TRUE, 0, .x)))

rm(censo_econ2019_raw, id_act)

# Escritura ----
fst::write_fst(censo_econ2019_clean, "./data/2_interim/censo_econ2019_clean.fst")
  
