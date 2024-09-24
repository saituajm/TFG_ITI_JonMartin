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
  
  dataframe_trabajo_1<-readRDS("C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/rds_files/dataset5.rds")
}
# Asumiendo que dataframe_trabajo_1 ya está ordenado por fecha
dataframe_trabajo_1 <- dataframe_trabajo_1 %>%
  mutate(Index = row_number())  # Crear un índice de fila consecutivo

# Crear el gráfico usando el índice en lugar de la marca de tiempo
ggplot(dataframe_trabajo_1, aes(x = Index)) +
  geom_line(aes(y = temperatura_interior, color = "Temperatura Interior"), size = 1) +
  geom_line(aes(y = setpoint_temperatura, color = "Setpoint Temperatura"), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("Temperatura Interior" = "blue", "Setpoint Temperatura" = "red")) +
  labs(x = "Observación", y = "Temperatura (°C)", 
       title = "Comparación de la Temperatura Interior y Setpoint de Temperatura",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")         


################## Solo setpoint ####################

media_setpoint <- mean(dataframe_trabajo_1$setpoint_temperatura, na.rm = TRUE)

ggplot(dataframe_trabajo_1, aes(x = marca_tiempo, y = setpoint_temperatura)) +
  geom_line(color = "green", size = 1) +  # Dibuja la línea de setpoint_temperatura
  geom_hline(yintercept = media_setpoint, color = "red", linetype = "dashed", size = 1, 
             show.legend = TRUE, linetype="longdash") +  # Dibuja una línea horizontal para la media
  scale_color_manual(values = c("Setpoint Temperatura" = "green", "Media" = "red")) +
  labs(x = "Tiempo", y = "Setpoint de Temperatura (°C)", 
       title = "Evolución del Setpoint de Temperatura y su Media") +
  theme_minimal() +  # Aplica un tema minimalista
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Mejora la legibilidad de las fechas en el eje X
        plot.title = element_text(hjust = 0.5))  # Centra el título del gráfico

############### Suavizada ###############################

ggplot(dataframe_trabajo_1, aes(x = marca_tiempo, y = setpoint_temperatura)) +
  geom_smooth(color = "green", size = 1, method = "loess", span = 0.2) +  # 'loess' es bueno para suavizado no paramétrico, 'span' controla la suavidad
  geom_hline(yintercept = media_setpoint, color = "red", linetype = "dashed", size = 1) +
  labs(x = "Tiempo", y = "Setpoint de Temperatura (°C)", 
       title = "Evolución del Setpoint de Temperatura con Suavizado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


############### COmparación setpoint y tempin ###############################

ggplot(dataframe_trabajo_1, aes(x = setpoint_temperatura, y = temperatura_interior)) +
  geom_point(color = "blue", alpha = 0.5) +  # Puntos semi-transparentes para visualizar la densidad y superposición
  geom_smooth(method = "lm", color = "red") +  # Línea de regresión lineal para mostrar la tendencia
  labs(x = "Setpoint de Temperatura (°C)", y = "Temperatura Interior (°C)",
       title = "Relación entre la Temperatura Interior y el Setpoint") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Alinea el título al centro
        axis.title.x = element_text(face = "bold"),  # Negrita para el título del eje X
        axis.title.y = element_text(face = "bold")) +  # Negrita para el título del eje Y
  ylim(20, 35)  # Ajustar los límites del eje Y para alejar la escala
