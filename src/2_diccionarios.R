### Diccionarios de variables ###

# Librerías ----
library(tidyverse)
library(fst)

# Lectura ----
eg_per_clean_all <- read_fst("./data/3_final/eg_per_all_clean.fst")
ig_per_clean_all <- read_fst("./data/3_final/ig_per_all_clean.fst")
# eg_inc_clean_all <- read_fst("./data/3_final/eg_inc_all_clean.fst")
# ig_inc_clean_all <- read_fst("./data/3_final/ig_inc_all_clean.fst")

# Nombres de variables 
var_names_eg <- names(eg_per_clean_all) 
  
var_names_ig <- names(ig_per_clean_all)
rm(eg_per_clean_all, ig_per_clean_all)

# Diccionario SCIAN 2018 ----
scian_dicc <- readxl::read_xlsx("./data/1_raw/scian_2018_categorias_y_productos.xlsx", skip = 1) %>%
  janitor::clean_names() %>%
  filter(!is.na(codigo) & codigo < 100) %>%
  mutate(codigo = as.character(codigo))

# Diccionario Censo económico ----
ce_dicc <- read_csv("./data/1_raw/diccionario_de_datos_ce2019.csv") %>%
  janitor::clean_names() %>%
  mutate(columna = str_to_lower(columna))

# Diccionario efipem ----
efipem_dicc <- read_csv("./data/2_interim/efipem19_clean.csv")

# Diccionario Egresos ----

# Variables genéricas
vars_genericas_per <- c("Clave de municipio (últimos 4 dígitos de la clave INEGI)", "Distinción entre egresos e ingresos", "Nombre del estado al que pertenece el municipio", "Nombre del municipio", "Población total calculada con el factor de expansión que reportó percibir que las prácticas de corrupción son muy frecuentes en su municipio de residencia", "Resto de la población calculada con el factor de expansión que respondió percibir que las prácticas de corrupción son frecuentes hasta poco frecuentes en su municipio de residencia", "Proporción de la población calculada con el factor de expansión que percibe las prácticas de corrupción como muy frecuentes en su municipio de residencia", "Población del municipio con base en el censo de población más cercano (2020 en este caso)", "Grado promedio de escolaridad de la población del municipio con base en el censo de población más cercano (2020 en este caso)")
vars_genericas_inc <- c("Clave de municipio (últimos 4 dígitos de la clave INEGI)", "Distinción entre egresos e ingresos", "Nombre del estado al que pertenece el municipio", "Nombre del municipio", "Población total calculada con el factor de expansión que reportó haber participado en algún acto de corrupción en los últimos 5 años", "Resto de la población calculada con el factor de expansión que respondió no haber participado en algún acto de corrupción en los últimos 5 años", "Proporción de la población calculada con el factor de expansión que participó en algún acto de corrupción en los últimos 5 años", "Población del municipio con base en el censo de población más cercano (2020 en este caso)", "Grado promedio de escolaridad de la población del municipio con base en el censo de población más cercano (2020 en este caso)")

## Percepción ----

eg_dicc_per <- data.frame(variables = var_names_eg)

eg_dicc_per$descripcion <- c()
eg_dicc_per[1:9, "descripcion"] <- vars_genericas_per

# Censo
for(i in ce_dicc$columna){
  if(i %in% str_extract(eg_dicc_per$variable, "^[a-z][0-9]+[a-z]")){
    descrip <- ce_dicc[which(ce_dicc$columna == i), "descripcion"]
    eg_dicc_per[which(str_extract(eg_dicc_per$variable, "^[a-z][0-9]+[a-z]") == i), "descripcion"] <- descrip
  }
}

# SCIAN
for(i in scian_dicc$codigo){
  if(i %in% str_extract(eg_dicc_per$variable, "\\d*$")){
    descrip <- scian_dicc[which(scian_dicc$codigo == i), "titulo"]
    eg_dicc_per[which(str_extract(eg_dicc_per$variable, "\\d*$") == i), "descripcion"] <- paste(unique(eg_dicc_per[which(str_extract(eg_dicc_per$variable, "\\d*$") == i), "descripcion"]), descrip, sep = ";")
  }
}

# Efipem 
# efipem_dicc$descrip <- 1
# idx_caps <- which(efipem_dicc$categoria == "capitulo")
# for(i in seq(1, length(idx_caps))){
#   descrip <- efipem_dicc$descripcion_categoria[idx_caps[i]]
#   if(i == 1){
#     interval <- seq(idx_caps[i], idx_caps[i + 1])
#     sapply(interval, function(x){efipem_dicc$descrip[x] <- descrip})
#   }
#   if(i == length(idx_caps)){
#     interval <- seq(idx_caps[i], nrow(efipem_dicc))
#     sapply(interval, function(x){efipem_dicc$descrip[x] <- descrip}) 
#   }
#   else{
#     interval <- seq(idx_caps[i], idx_caps[i + 1])
#     sapply(interval, function(x){efipem_dicc$descrip[x] <- descrip}) 
#   }
# }



## Incidencia ----

eg_dicc_inc <- data.frame(variables = var_names_eg)

eg_dicc_inc$descripcion <- c()
eg_dicc_inc[1:9, "descripcion"] <- vars_genericas_inc

for(i in ce_dicc$columna){
  if(i %in% str_extract(eg_dicc_inc$variable, "^[a-z][0-9]+[a-z]")){
    descrip <- ce_dicc[which(ce_dicc$columna == i), "descripcion"]
    eg_dicc_inc[which(str_extract(eg_dicc_inc$variable, "^[a-z][0-9]+[a-z]") == i), "descripcion"] <- descrip
  }
}

for(i in scian_dicc$codigo){
  if(i %in% str_extract(eg_dicc_inc$variable, "\\d*$")){
    descrip <- scian_dicc[which(scian_dicc$codigo == i), "titulo"]
    eg_dicc_inc[which(str_extract(eg_dicc_inc$variable, "\\d*$") == i), "descripcion"] <- paste(unique(eg_dicc_inc[which(str_extract(eg_dicc_inc$variable, "\\d*$") == i), "descripcion"]), descrip, sep = ";")
  }
}

# Diccionario Ingresos ----

## Percepción ----

ig_dicc_per <- data.frame(variables = var_names_ig)

ig_dicc_per$descripcion <- c()

ig_dicc_per[1:9, "descripcion"] <- vars_genericas_per
for(i in ce_dicc$columna){
  if(i %in% str_extract(ig_dicc_per$variable, "^[a-z][0-9]+[a-z]")){
    descrip <- ce_dicc[which(ce_dicc$columna == i), "descripcion"]
    ig_dicc_per[which(str_extract(ig_dicc_per$variable, "^[a-z][0-9]+[a-z]") == i), "descripcion"] <- descrip
  }
}

for(i in scian_dicc$codigo){
  if(i %in% str_extract(ig_dicc_per$variable, "\\d*$")){
    descrip <- scian_dicc[which(scian_dicc$codigo == i), "titulo"]
    ig_dicc_per[which(str_extract(ig_dicc_per$variable, "\\d*$") == i), "descripcion"] <- paste(unique(ig_dicc_per[which(str_extract(ig_dicc_per$variable, "\\d*$") == i), "descripcion"]), descrip, sep = ";")
  }
}

## Incidencia ----

ig_dicc_inc <- data.frame(variables = var_names_ig)

ig_dicc_inc$descripcion <- c()

ig_dicc_inc[1:9, "descripcion"] <- vars_genericas_inc
for(i in ce_dicc$columna){
  if(i %in% str_extract(ig_dicc_inc$variable, "^[a-z][0-9]+[a-z]")){
    descrip <- ce_dicc[which(ce_dicc$columna == i), "descripcion"]
    ig_dicc_inc[which(str_extract(ig_dicc_inc$variable, "^[a-z][0-9]+[a-z]") == i), "descripcion"] <- descrip
  }
}

for(i in scian_dicc$codigo){
  if(i %in% str_extract(ig_dicc_inc$variable, "\\d*$")){
    descrip <- scian_dicc[which(scian_dicc$codigo == i), "titulo"]
    ig_dicc_inc[which(str_extract(ig_dicc_inc$variable, "\\d*$") == i), "descripcion"] <- paste(unique(ig_dicc_inc[which(str_extract(ig_dicc_inc$variable, "\\d*$") == i), "descripcion"]), descrip, sep = ";")
  }
}

# Escritura ----
write.csv(eg_dicc_per, "./data/3_final/eg_per_dicc.csv", row.names = FALSE, fileEncoding = "latin1")
write.csv(eg_dicc_inc, "./data/3_final/eg_inc_dicc.csv", row.names = FALSE, fileEncoding = "latin1")

write.csv(ig_dicc_per, "./data/3_final/ig_per_dicc.csv", row.names = FALSE, fileEncoding = "latin1")
write.csv(ig_dicc_inc, "./data/3_final/ig_inc_dicc.csv", row.names = FALSE, fileEncoding = "latin1")
