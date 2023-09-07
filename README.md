# Clasificación de municipios en susceptibles a actos de corrupción con base en sus finanzas públicas

## Conjuntos de datos

Corresponden a los scripts "2_limpieza"

CAMBIOS en todos los scripts de limpieza:
	- Estandarizar todos los scripts de bases de limpieza de bases (archivos fst para conjuntos grandes)
	- Reemplazar función write.csv por write_csv y write_fst

### Censo población 2020
Se integró la clave completa de municipio inegi 
Se guarda el conjunto de datos "./data/2_interim/censo2020_mun_clean.csv"

### Censo económico 2019
1) Se descargaron todos los datos del censo económico de cada estado (no estaban agregados en un solo archivo como en el caso del censo de población).
2) Se acotaron los datos a los grandes sectores de actividades económicas (aquellos que su código fuera menor a 100) 
3) Conjunto de datos fue transformado a formato wide, cada rubro se distingue por su actividad. Ejemplo: rubro -> a111a_21 <- actividad econ
Se guarda el conjunto de datos "./data/2_interim/censo_econ2019_clean.fst"

CAMBIOS: 
	- Se descartó escribir el raw csv

### Finanzas públicas 2019
Se agrega clave completa de municipio inegi
Se acota a variables relevantes: tema, categoria, descripcion_categoria y valor 
Se guarda el conjunto de datos "./data/2_interim/efipem19_clean.csv"

### Encig 2019
Se agrega clave completa de municipio inegi
Se crean dos conjuntos de datos: uno de percepción utilizando la pregunta 3.2 de la sección 3 y otro de incidencia utilizando la pregunta 9.1 de la sección 9, aquí hay un posible sesgo porque la pregunta se refiere a los últimos 5 años (Nota: Es posible construir una variable similar con las preguntas de la sección 8).
Ambos conjuntos se generan; 1) con la creación de una dummy dependiendo de la respuesta a la pregunta (caso de percepción si respondieron "muy frecuente", caso de incidencia si respondieron "sí"), 2) se multiplicó la dummy por el factor de expansión (fac_p18) de esta manera se contabiliza la supuesta cantidad de gente que reportó dichas experiencias en cada estrato del área encuestada
Después de la contabilización de las respuesta se calculó la proporción con respecto al total de encuestados que sí respondieron, es decir que no respondieron "no sé".
Se guardan los conjuntos de datos:
	conjunto completo "./data/2_interim/encig19_clean.csv"
	conjunto percepción "./data/2_interim/encig19_per_clean.csv"
	conjunto incidencia "./data/2_interim/encig19_inc_clean.csv"


CAMBIOS:
	- Ahora las proporciones se calculan con respecto al total de encuestados y no con respecto a los encuestados que reportaron no percibir o haber practicado actos de corrupción respectivamente para cada conjunto de datos.
	- El conjunto de datos para percepción se renombró de encig19_mun_clean.csv a encig19_per_clean.csv

### Base final
	Se crean cuatro conjuntos de datos: ingresos y egresos para percepción e incidencia de corrupción
	Estos conjuntos son el resultado de un join entre los conjuntos del censo de población 2020, censo económico 2019, finanzas públicas 2019 y encig 2019. Están a nivel municipio
	Algunos municipios no cuentan con información del censo económico, por lo tanto, se sustituyeron sus NAs por la media del estado en cada variable (NOTA: esto no es bueno porque los municipios pueden ser muy chicos; es mejor reemplazar estos valores por municipios que sí tengan datos y sean similares a los que cuentan con NAs).	
	Los conjuntos finales cuentan con 255 observaciones que corresponden a la cantidad de municipios encuestados por la encig (NOTA: esto es una limitación porque significa que son los únicos municipios de los cuales se cuenta información sobre corrupción)
	Se guardan los cuatro conjuntos con el prefijo "ingresos19|egresos19" en "./data/3_final/"

CAMBIOS:
	- Se elimina el apartado de la base completa
	- Se modifica la ruta del conjunto de percepción
	- Se agregan las variables económicas al conjunto de percepción
	- Se crearon conjuntos de datos que contienen observaciones de todos los municipios para predecir clasificarlos a todos como corruptos o no corruptos con ayuda de los modelos entrenados

## Modelado

Corresponden a los scripts "3_ML"

### 3_Optim_function
Esta funcion sirve para optimizar el valor de la proporción de corrupción que permita obtener un modelo más preciso en términos de prevalencia, recall, especificidad, precisión y F1-score.
Se optimiza la proporción porque es la variable continua que se está utilizando para catalogar a los municipios como corruptos o no corruptos con base en una dummy, la cual toma el valor de 1 si el municipio tiene un valor mayor al cutoff de proporción de corrupción.
El output es el valor de proporción de corrupción que maximiza el F1-score condicionado a que exista mayor recall sobre especificidad.
IMPORTANTE: Las métricas de evaluación (precisión, recall, etc.) fueron evaluadas con en train_set, esto para evitar "Cherry picking"

CAMBIOS: 
	- Se actualizó la seleccion de variables para incluir las variables del censo economico

### 3_Optimizacion
Aplica la función a los conjuntos de ingresos y egresos para optimizar el valor de la proporción de corrupción.
Se grafican los resultados de la optimización.
Se guardan los conjuntos de datos "./data/2_interim/eval_eg.csv" y "./data/2_interim/eval_ig.csv" con los resultados de la optimización.

CAMBIOS: 
	- Se actualizacon los paths y funciones para archivos .fst

### 3_ROC_function
Básicamente igual a la función de optimización, pero en este caso las métricas de evaluación se realizaron con el test set.

CAMBIOS: 
	- Se actualizó la seleccion de variables para incluir las variables del censo economico

### 3_ROC
Se aplica la función ROC para crear gráficos comparativos entre los modelos de percepción e incidencia.
Se guardan los conjuntos de datos "./data/2_interim/roc_eg.csv" y "./data/2_interim/roc_ig.csv" con los resultados de la función ROC.

### 3_ML_function
Sirve para aplicar el algoritmo Gradient Boostinga a través de la función gb_model()
El cutoff es elegido con los resultados del script 3_Optimizacion.
Se crea la función best_cutoff() para extraer el valor del conjunto de datos de evaluación

CAMBIOS:
	- Se actualizó la selección de variables para incluir las variables del censo económico

### 3_ML_modeling
Aplica la función gb_model() utilizando el mejor cutoff de la etapa de optimización
Se entrenan cuatro modelos: Dos de ingresos y dos de egresos; percepción e incidencia.

CAMBIOS:
	- Reemplazo read_csv por read_fst

### 4_deployment
Se utilizan los modelos del script 3_ML para predecir los valores de corrupción de cada municipio, utilizando los conjuntos de datos que contienen observaciones de todos los municipios
Se guardan los resultados, es decir, los valores predichos por los modelos en "./data/3_final/" con la terminación "predicted"

### 5_geo
Se utilizan las clasificaciones predichas por los modelos para graficar mapas. Output final



