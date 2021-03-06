library(tidyverse)
library(openxlsx)
library(readxl)
library (lubridate)
library(plyr)

#Cargar BD Epivigila descargada
epivigila<- read_csv(file.choose())

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
#10 días previos

ev_ssdr_CE <- filter(ev_ssdr_contactos_dia1, cont_inicio_cuarentena >= today()-10)

# Eliminar repetidos de cont_n_identificacion
ev_ssdr_CE <- ev_ssdr_CE %>% 
  distinct(cont_n_identificacion, .keep_all = TRUE)

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


#Guardar excel de CE epivigila con fecha y hora de última actualización
write.xlsx(ev_ssdr_CE, paste0("epivigila_CE_SSDR_ult10dias_al_",
                              f,
                              "_",
                              h,
                              "h",
                              m,
                              "m",
                              ".xlsx"))

# Tabla de total de pacientes según fecha de inicio de cuarentena
table(ev_ssdr_CE$cont_inicio_cuarentena)

#Guardar excel de BD epivigila con fecha y hora de última actualización
#Esto puede ser útil para compartir la BD en un formato más común
write.xlsx(epivigila, paste0("epivigila_bd_actualizada_al_",
                             f,
                             "_",
                             h,
                             "h",
                             m,
                             "m",
                             ".xlsx"))

