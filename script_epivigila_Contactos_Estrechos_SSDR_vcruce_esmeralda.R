library(tidyverse)
library(openxlsx)
library(readxl)
library (lubridate)
library(plyr)

#Cargar BD Epivigila y Esmeralda descargada
epivigila <- read_csv(file.choose())

# Al archivo descargado de esmeralda se le debe cambiar el "#"
# de la primera celda por "folio"
esmeralda <- `2020.08.20_8.40_lista.examenes`

#Extraer fecha de creación y obtener la última
#para poder nombrar los archivos posteriormente.
#Se interpretará como la fecha de última actualización
fecha_creacion <- ymd_hms(epivigila$fecha_creacion)  
f_act <- max(fecha_creacion)
f <- date(f_act)
h <- hour(f_act)
m <- minute(f_act)
s <- second(f_act)

# Filtrar comunas de nuestro Servicio de Salud.
# En nuestra región hay 3 Servicios, así que debemos
# filtrar sólo las comunas que nos interesan.
epivigila_ssdr <- filter(epivigila, epivigila$cont_comuna %in% c("Calbuco",
                                                                 "Chaitén",
                                                                 "Cochamó",
                                                                 "Fresia",
                                                                 "Frutillar",
                                                                 "Futaleufú",
                                                                 "Hualaihué",
                                                                 "Llanquihue",
                                                                 "Los Muermos",
                                                                 "Maullín",
                                                                 "Palena",
                                                                 "Puerto Montt",
                                                                 "Puerto Varas"))

#Filtrar sólo contactos
#Los pacientes se clasifican como caso o contacto.
ev_ssdr_contactos <- filter(epivigila_ssdr, epivigila_ssdr$tipo_seguimiento == "contacto")

# Filtrar sólo contactos del día 1
# Los pacientes se repiten 14 veces en la BD, cambiando principalmente la
# información de seguimiento.
ev_ssdr_contactos_dia1 <- filter(ev_ssdr_contactos , ev_ssdr_contactos$dia_contacto == 1)

# Cambiar inicio cuarentena a formato fecha, para poder
# realizar operaciones con ella.
ev_ssdr_contactos_dia1 <- mutate(ev_ssdr_contactos_dia1,
                                 cont_inicio_cuarentena2 = ymd(ev_ssdr_contactos_dia1$cont_inicio_cuarentena))
ev_ssdr_contactos_dia1 <- select(ev_ssdr_contactos_dia1, -cont_inicio_cuarentena)
names (ev_ssdr_contactos_dia1)[names(ev_ssdr_contactos_dia1) == 'cont_inicio_cuarentena2'] <- "cont_inicio_cuarentena"

#filtrar por fecha de inicio de cuarentena
#9 días previos

ev_ssdr_CE <- filter(ev_ssdr_contactos_dia1, cont_inicio_cuarentena >= today()-9)

# Eliminar repetidos de cont_n_identificacion
ev_ssdr_CE <- ev_ssdr_CE %>% 
  distinct(cont_n_identificacion, .keep_all = TRUE)

# Seleccionar columnas útiles para contactabilidad de CE y reordenarlas
ev_ssdr_CE <- ev_ssdr_CE[c("n_folio",
                           "tipo_seguimiento",
                           "cont_inicio_cuarentena",
                           "cont_tipo_identificacion",
                           "cont_n_identificacion",
                           "cont_nombres",
                           "cont_primer_apellido",
                           "cont_segundo_apellido",
                           "cont_sexo",
                           "cont_fecha_nacimiento",
                           "cont_edad",
                           "cont_tipo_direccion",
                           "cont_tipo_institucion",
                           "cont_via_residencia",
                           "cont_direccions",
                           "cont_n_residencia",
                           "cont_dpto",
                           "cont_poblacion",
                           "cont_region",
                           "cont_comuna",
                           "cont_n_telefono",
                           "cont_n_celular",
                           "cont_correo_electronico",
                           "cont_fecha_seguimiento",
                           "nombre_institucion_indice",
                           "institucion_contacto",
                           "nombre_institucion_seguimiento")]

# Tabla de total de pacientes según fecha de inicio de cuarentena
table(ev_ssdr_CE$cont_inicio_cuarentena)

#Eliminar datos intermedios 
rm(epivigila_ssdr,ev_ssdr_contactos, ev_ssdr_contactos_dia1)


#### Esmeralda

# Seleccionar columnas útiles de archivo esmeralda
esmeralda2 <- esmeralda[c("run",
                          "fecha_recepcion_muestra",
                          "fecha_resultado",
                          "resultado",
                          "laboratorio",
                          "origen")]

#Separar Rut
esmeralda2 <- separate(esmeralda2, run, c("run","dv"), sep = "-")

# Cambiar a formato numerico y borrar run NA
esmeralda2$run <- as.numeric(esmeralda2$run)
esmeralda2 <- esmeralda2[!is.na(esmeralda2$run),]

# Cambiar nombre de columna de Epivigila a run
names(ev_ssdr_CE)[names(ev_ssdr_CE) == 'cont_n_identificacion'] <- "run"

# Cruzar ambas tablas conservando filas de CE de Epivigila
consolidado_CE <- left_join(ev_ssdr_CE, esmeralda2)

## Eliminar run repetidos
consolidado_CE <- consolidado_CE %>% 
  distinct(run, .keep_all = TRUE)

#Separar fecha y hora de recepción
consolidado_CE <- separate(consolidado_CE_ordenado, fecha_recepcion_muestra,
                           c("fecha_recepcion_muestra","hora_recepcion_muestra"), sep = " ")
#Separar fecha y hora de resultado
consolidado_CE <- separate(consolidado_CE_ordenado, fecha_resultado,
                           c("fecha_resultado","hora_resultado"), sep = " ")

#Ordenar consolidado por fecha de inicio de cuarentena y fecha de toma de muestra
consolidado_CE_ordenado <- consolidado_CE[order(
  consolidado_CE$cont_inicio_cuarentena,
  consolidado_CE$fecha_recepcion_muestra),]

# Seleccionar columnas útiles para contactabilidad de CE y reordenarlas
CE_ordenado_tm <- consolidado_CE_ordenado[c("n_folio",
                           "tipo_seguimiento",
                           "cont_inicio_cuarentena",
                           "fecha_recepcion_muestra",
                           "fecha_resultado",
                           "resultado",
                           "cont_tipo_identificacion",
                           "run",
                           "cont_nombres",
                           "cont_primer_apellido",
                           "cont_segundo_apellido",
                           "cont_sexo",
                           "cont_fecha_nacimiento",
                           "cont_edad",
                           "cont_tipo_direccion",
                           "cont_tipo_institucion",
                           "cont_via_residencia",
                           "cont_direccions",
                           "cont_n_residencia",
                           "cont_dpto",
                           "cont_poblacion",
                           "cont_region",
                           "cont_comuna",
                           "cont_n_telefono",
                           "cont_n_celular",
                           "cont_correo_electronico",
                           "cont_fecha_seguimiento",
                           "nombre_institucion_indice",
                           "institucion_contacto",
                           "nombre_institucion_seguimiento")]

rm(consolidado_CE_ordenado,
   consolidado_CE,
   esmeralda,
   esmeralda2,
   ev_ssdr_CE)

# Ordenar y filtrar CE sin info de toma de muestras
CE_ordenado_sin_tm <- filter(CE_ordenado_tm,  is.na(fecha_recepcion_muestra))

CE_ordenado_con_tm <- filter(CE_ordenado_tm,  !is.na(fecha_recepcion_muestra))

#Eliminar columnas de toma de muestras, que están vacías
CE_ordenado_sin_tm <- select(CE_ordenado_sin_tm,
                             -fecha_recepcion_muestra,
                             -fecha_resultado,
                             -resultado)

#Crear hoja resumen
resumen <- table(CE_ordenado_sin_tm$cont_inicio_cuarentena)
resumen <- as.data.frame(resumen)
names(resumen)[names(resumen) == 'Var1'] <- "Fecha de inicio de cuarentena"
names(resumen)[names(resumen) == 'Freq'] <- "Número de pacientes"

## Guardar archivo excel de CE usando openxlsx

CE <- createWorkbook("Contactos Estrechos")
addWorksheet(CE, sheetName = "Consolidado Contactos Estrechos")
addWorksheet(CE, sheetName = "Contactos Estrechos sin TM")
addWorksheet(CE, sheetName = "Contactos Estrechos con TM")

writeData(CE, sheet = "Consolidado Contactos Estrechos", x = CE_ordenado_tm)
writeData(CE, sheet = "Contactos Estrechos sin TM", x = CE_ordenado_sin_tm)
writeData(CE, sheet = "Contactos Estrechos con TM", x = CE_ordenado_con_tm)

saveWorkbook(CE, file = paste0("epivigila_esmeralda_CE_ult9dias_al_",
                               f,
                               "_",
                               h,
                               "h",
                               m,
                               "m",
                               ".xlsx"), overwrite = TRUE)







