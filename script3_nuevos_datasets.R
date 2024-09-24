library(dplyr)
library(lubridate)
#importar el dataset
{
wd<-getwd()
  
sampleo<-60
  
horas_anteriores<-6
  
obs_anteriores<-horas_anteriores*60/sampleo
  
 rm(horas_anteriores)
df<-readRDS(paste(wd, 
                                 "/rds_files/dataframe_", 
                                 sampleo, 
                                 ".rds", 
                                 sep=""))
}

# Se va a crear un dataframe con solo las horas nocturnas
# en las que no hay ni radiación solar ni sistema de climatización

# Convertir la columna marca_tiempo a formato POSIXct
df$marca_tiempo <- as.POSIXct(df$marca_tiempo, format = "%d/%m/%Y %H:%M")

# Extraer la hora de la columna marca_tiempo
df$hora <- hour(df$marca_tiempo)

# Filtrar las filas que representan las horas entre las 21:00 y las 7:00
nuevo_df <- df %>%
  filter(hora >= 21 | hora < 8)

# Ahora nuevo_df contendrá solo las filas que representan las horas entre las 21:00 y las 7:00

nuevo_df

#Guardar dataset
saveRDS(nuevo_df, paste(wd, "/rds_files/", "dataframe_noche.rds", 
                                 sep=""))
write.csv2(nuevo_df, 
           paste(wd, "/datasets/", "dataframe_noche.csv", sep=""), 
           row.names=FALSE) 

rm(nuevo_df_2)

# Se va a crear otro dataset en el que solo estén las horas donde si que hay
# tanto sistema de climatización como radiación solar

# Filtrar las filas que representan las horas entre las 8:00 y las 20:00
nuevo_df_2 <- df %>%
  filter(df$hora >= 8 & df$hora <= 20)

# Ahora nuevo_df contendrá solo las filas que representan las horas entre las 8:00 y las 20:00

nuevo_df_2

#Guardar dataset
saveRDS(nuevo_df_2, paste(wd, "/rds_files/", "dataframe_dia.rds", 
                        sep=""))
write.csv2(nuevo_df_2, 
           paste(wd, "/datasets/", "dataframe_dia.csv", sep=""), 
           row.names=FALSE) 
