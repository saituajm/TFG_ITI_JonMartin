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
  
  dataframe_trabajo<-readRDS(paste(wd, 
                                   "/rds_files/dataframe_", 
                                   sampleo, 
                                   ".rds", 
                                   sep=""))
}  

############ Relación tempin tempex ###############

ggplot(dataframe_trabajo, aes(x = temperatura_exterior, y = temperatura_interior)) +
  geom_point(alpha = 0.5) +  # Añade puntos al gráfico, ajusta la transparencia con alpha
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Añade una línea de regresión lineal con intervalos de confianza
  labs(x = "Temperatura Exterior (°C)", y = "Temperatura Interior (°C)", title = "Relación entre la Temperatura Exterior e Interior") +
  theme_minimal()  # Utiliza un tema minimalista para el gráfico

##########################################################################
##########
########## Tempex tempin una semana
##########
#########################################################################
##########
# Convertir la columna 'marca_tiempo' a tipo POSIXct para manejar fecha y hora
# Asumiendo que 'marca_tiempo' está en el formato 'dd/mm/yyyy h:mm'
dataframe_trabajo$marca_tiempo <- as.POSIXct(dataframe_trabajo$marca_tiempo, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")

# Verificar los primeros valores convertidos para asegurarse de que ahora están correctos
head(dataframe_trabajo$marca_tiempo)
# Filtrar los datos para la semana del 20 al 24 de septiembre 2021
data_semana <- dataframe_trabajo %>% 
  filter(marca_tiempo >= as.POSIXct("2021-09-20", tz="UTC") & 
           marca_tiempo <= as.POSIXct("2021-09-24 23:59:59", tz="UTC"))

# Crear el gráfico
ggplot(data_semana, aes(x = marca_tiempo)) +
  geom_line(aes(y = temperatura_exterior, color = "Temperatura Exterior"), size = 1) +
  geom_line(aes(y = temperatura_interior, color = "Temperatura Interior"), size = 1) +
  labs(x = "Fecha y Hora", y = "Temperatura (°C)", title = "Evolución de la Temperatura Exterior e Interior durante una Semana") +
  scale_color_manual(values = c("Temperatura Exterior" = "blue", "Temperatura Interior" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Imprimir el gráfico
print(ggplot(data_semana, aes(x = marca_tiempo)) +
        geom_line(aes(y = temp_exterior, color = "Temperatura Exterior"), size = 1) +
        geom_line(aes(y = temp_interior, color = "Temperatura Interior"), size = 1) +
        labs(x = "Fecha y Hora", y = "Temperatura (°C)", title = "Evolución de la Temperatura Exterior e Interior durante una Semana") +
        scale_color_manual(values = c("Temperatura Exterior" = "blue", "Temperatura Interior" = "red")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)))

############ Tempin a lo largo de todo el dataframe #####################

# Calcular el máximo y el mínimo de la temperatura interior
max_temp <- max(dataframe_trabajo$temperatura_interior, na.rm = TRUE)
min_temp <- min(dataframe_trabajo$temperatura_interior, na.rm = TRUE)
mean_temp <- mean(dataframe_trabajo$temperatura_interior, na.rm = TRUE)
sd_temp <- sd(dataframe_trabajo$temperatura_interior, na.rm = TRUE)

# Calcular la media + 1 SD y la media - 1 SD
mean_plus_sd <- mean_temp + sd_temp
mean_minus_sd <- mean_temp - sd_temp
ggplot(dataframe_trabajo, aes(x = marca_tiempo)) +
  geom_line(aes(y = temperatura_interior, color = "Temperatura Interior"), size = 1) +
  geom_point(aes(y = temperatura_interior), color = "blue", size = 2, show.legend = FALSE) +
  geom_hline(yintercept = max_temp, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = min_temp, color = "green", linetype = "dashed", size = 1) +
  geom_hline(yintercept = mean_temp, color = "darkblue", linetype = "solid", size = 1) +
  geom_hline(yintercept = mean_plus_sd, color = "purple", linetype = "dotdash", size = 1) +
  geom_hline(yintercept = mean_minus_sd, color = "purple", linetype = "dotdash", size = 1) +
  scale_color_manual(values = c("Temperatura Interior" = "blue")) +
  labs(x = "Tiempo", y = "Temperatura Interior (°C)", title = "Evolución de la Temperatura Interior con Estadísticas Adicionales",
       color = "Leyenda") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

######################## Carga térmica con temperatura exterior ###########################

ggplot(dataframe_trabajo, aes(x = temperatura_exterior, y = energia_agua_refrigerada)) +
  geom_point(color = "blue", alpha = 0.5) +  # Puntos semi-transparentes para visualizar la densidad y superposición
  geom_smooth(method = "lm", color = "red") +  # Línea de regresión lineal para mostrar la tendencia
  labs(x = "Temperatura Exterior (°C)", y = "Energía del Agua Refrigerada (kWh)",
       title = "Relación entre la Energía del Agua Refrigerada y la Temperatura Exterior") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Alinea el título al centro
        axis.title.x = element_text(face = "bold"),  # Negrita para el título del eje X
        axis.title.y = element_text(face = "bold"))  # Negrita para el título del eje Y
