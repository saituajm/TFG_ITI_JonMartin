# Este script prepara el dataset añadiendo la variable 'setpoint_temperatura' 
# al dataset modificado por Iñigo García de Eulate en su trabajo.
# Se crean dataframes con diferentes tiempos de muestreo (15, 30 y 60 minutos),
# se generan medias móviles de las variables y se añaden observaciones anteriores 
# (hasta 6 horas) para cada variable. Los datasets resultantes se guardan para 
# análisis posteriores.

# Importar la librería necesaria para las medias móviles
library(zoo)

# Obtener el directorio de trabajo y cargar el dataset
{
  wd <- getwd()
  dataset <- readRDS(paste(wd, "/rds_files/dataset5.rds", sep=""))
}

# Creación de un dataframe con las variables que se emplearán para las regresiones,
# incluyendo la variable 'setpoint_temperatura'.
{
  variables <- c("marca_tiempo",
                 "hora_solar",
                 "ocupantes_conteo_robus3",
                 "temperatura_exterior",
                 "temperatura_interior",
                 "energia_agua_refrigerada",
                 "radiacion_global_fachada",
                 "setpoint_temperatura")
  
  dataframe <- data.frame(matrix(ncol=length(variables), nrow=nrow(dataset)))
  names(dataframe) <- variables
  
  for(i in 1:length(dataframe)){
    for(j in 1:length(dataset)){
      if (names(dataset[j]) == names(dataframe[i])){
        dataframe[,i] <- dataset[,j]
      }
    }
  }
}

# Creación de los diferentes dataframes en función del tiempo de sampleo deseado
# Se calculan medias móviles para cada variable, excluyendo marca_tiempo y hora_solar
{
  sampleo <- c(15, 30, 60) # Tiempos de sampleo deseados en minutos
  
  for (i in 1:length(sampleo)){
    n_elementos <- sampleo[i] / 5
    
    dataframe_trabajo <- dataframe
    
    for (j in 1:length(dataframe_trabajo)){
      if (names(dataframe[j]) != "marca_tiempo" & 
          names(dataframe[j]) != "hora_solar"){
        dataframe_trabajo[, j] <- rollmean(dataframe_trabajo[, j], k=n_elementos, 
                                           fill=NA, align="center") 
      }
    }
    
    # Eliminar las filas con NA generadas por la media móvil
    dataframe_trabajo <- na.omit(dataframe_trabajo) 
    
    # Filtrar para que los datos correspondan a los tiempos de muestreo exactos
    dataframe_trabajo <- subset(dataframe_trabajo,
                                as.numeric(format(dataframe_trabajo$marca_tiempo,
                                                  "%M")) %% sampleo[i] == 0)
    
    nombre_dataframe <- paste("dataframe", sampleo[i], sep="_")
    assign(nombre_dataframe, dataframe_trabajo)
  }
}

# Limpieza de variables intermedias
rm(dataframe_trabajo, dataset, i, j, n_elementos, nombre_dataframe, variables)

# Añadir observaciones anteriores (hasta 6 horas) para cada variable
# Se añaden columnas para cada observación anterior a las variables originales
{
  for (i in 1:length(sampleo)){
    nombre_dataframe <- paste("dataframe", sampleo[i], sep="_")
    dataframe_trabajo <- get(nombre_dataframe)
    
    horas_anteriores <- 6
    obs_anteriores <- horas_anteriores * 60 / sampleo[i]
    
    for (k in 1:length(dataframe_trabajo)){
      for (j in 1:obs_anteriores){
        dataframe_trabajo[[paste(names(dataframe_trabajo[k]), j, sep="_")]] <-
          filter(dataframe_trabajo[, k], c(rep(0, j), 1), sides=1)
      }
    }
    
    assign(nombre_dataframe, dataframe_trabajo) 
  }
}

# Guardar los datasets generados en archivos RDS y CSV
{
  for (i in 1:length(sampleo)){
    nombre_dataframe <- paste("dataframe", sampleo[i], sep="_")
    dataframe_trabajo <- get(nombre_dataframe)
    
    saveRDS(dataframe_trabajo, paste(wd, "/rds_files/", nombre_dataframe, ".3.rds", 
                                     sep=""))
    
    write.csv2(dataframe_trabajo, 
               paste(wd, "/datasets/", nombre_dataframe, ".3.csv", sep=""), 
               row.names=FALSE) 
  }
}

# Limpieza final de variables
rm(i, j, k, nombre_dataframe, obs_anteriores, dataframe_trabajo, dataframe_15,
   dataframe_30, dataframe_60, sampleo, dataframe, horas_anteriores)
