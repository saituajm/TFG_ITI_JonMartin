library(MASS)
library(Metrics)
library(openxlsx)
library(caret)
library(writexl)
library(ggplot2)
library(lubridate)
library(dbplyr)
library(dplyr)

{
  wd<-getwd()
  
  sampleo<-60
  
  horas_anteriores<-6
  
  obs_anteriores<-horas_anteriores*60/sampleo
  
  rm(horas_anteriores)
  
  dataframe_trabajo_2<-readRDS("C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/rds_files/dataframe_dia.rds")
}

ggplot(dataframe_trabajo_2, aes(x = temperatura_exterior, y = energia_agua_refrigerada)) +
  geom_point(color = "blue", alpha = 0.5) +  # Puntos semi-transparentes para visualizar la densidad y superposición
  geom_smooth(method = "lm", color = "red") +  # Línea de regresión lineal para mostrar la tendencia
  labs(x = "Temperatura Exterior (°C)", y = "Energía del Agua Refrigerada (kWh)",
       title = "Relación entre la Energía del Agua Refrigerada y la Temperatura Exterior") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Alinea el título al centro
        axis.title.x = element_text(face = "bold"),  # Negrita para el título del eje X
        axis.title.y = element_text(face = "bold"))  # Negrita para el título del eje Y

