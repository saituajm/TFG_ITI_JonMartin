# ------------------------------------ LIBRERÍAS NECESARIAS -----------------------------------------
# Cargar todas las librerías que serán utilizadas a lo largo del análisis

library(MASS)       # Para stepAIC y selección de modelos
library(Metrics)    # Para métricas de evaluación como MAE y RMSE
library(openxlsx)   # Para exportar archivos Excel
library(caret)      # Para manejo de dataframes y modelado
library(writexl)    # Para escribir archivos Excel
library(dplyr)      # Para manipulación de datos
library(ggplot2)    # Para visualización de datos
library(tidyverse)  # Para manejo de dataframes y manipulación
library(GGally)     # Para graficar matrices de correlación
library(broom)      # Para manejo de resultados de modelos
library(scales)     # Para manejo de escalas en gráficos
library(tidyr)      # Para reorganización de dataframes
library(lmtest)     # Para pruebas estadísticas en modelos de regresión
library(nortest)    # Para pruebas de normalidad


# ------------------------------------ CARGA Y PREPARACIÓN DE DATOS ---------------------------------
# Cargar los datos y crear nuevas variables que representen las diferencias de setpoint de temperatura
# El dataframe `dataframe_60.3.rds` incluye la nueva variable `dif_sp_tin`, que representa 
# la diferencia entre el setpoint de temperatura y la temperatura interior.
dataframe_trabajo <- readRDS("C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/rds_files/dataframe_60.3.rds")

# Crear nuevas variables con la diferencia entre setpoint de temperatura y temperatura interior
dataframe_trabajo$dif_sp_tin <- dataframe_trabajo$setpoint_temperatura - dataframe_trabajo$temperatura_interior
dataframe_trabajo$dif_sp_tin_1 <- dataframe_trabajo$setpoint_temperatura_1 - dataframe_trabajo$temperatura_interior_1
dataframe_trabajo$dif_sp_tin_2 <- dataframe_trabajo$setpoint_temperatura_2 - dataframe_trabajo$temperatura_interior_2

# --------------------------------- PROCESO DE CONSTRUCCIÓN DEL MODELO --------------------

# Paso 1: Modelo inicial
# Comenzamos con un modelo simple con dif_sp_tin y temperatura_exterior.
mod_inicial <- lm(energia_agua_refrigerada ~ dif_sp_tin + temperatura_exterior, data = dataframe_trabajo)
summary(mod_inicial)

# Paso 2: Añadir variables de forma incremental
# Añadir radiacion_global_fachada y optimizar el modelo
mod_paso_2 <- lm(energia_agua_refrigerada ~ dif_sp_tin + temperatura_exterior + radiacion_global_fachada, data = dataframe_trabajo)
mod_paso_2 <- stepAIC(mod_paso_2, direction = "both")
summary(mod_paso_2)

# Paso 3: Añadir energia_agua_refrigerada_1 (valor de una hora antes)
mod_paso_3 <- lm(energia_agua_refrigerada ~ dif_sp_tin + temperatura_exterior + 
                   radiacion_global_fachada + energia_agua_refrigerada_1, data = dataframe_trabajo)
mod_paso_3 <- stepAIC(mod_paso_3, direction = "both")
summary(mod_paso_3)

# Aquí se repite el proceso para todas las variables.


# ---------------------------------- MODELO Optimizado (mod_def) ----------------------------------
# Construir un modelo de regresión simple con algunas variables clave

# Selección de variables para el modelo optimizado
df_simple <- dataframe_trabajo %>%
  select(energia_agua_refrigerada, temperatura_exterior, radiacion_global_fachada, 
         ocupantes_conteo_robus3, energia_agua_refrigerada_1, dif_sp_tin)

# Ajustar el modelo de regresión lineal
mod_def <- lm(energia_agua_refrigerada ~ dif_sp_tin + radiacion_global_fachada + energia_agua_refrigerada_1)
summary(mod_def)

# ------------------------------------ MÉTRICAS DE ERROR (mod_def) ----------------------------------
# Calcular predicciones y métricas de evaluación del modelo simple

predictions_0 <- predict(mod_def)
residd <- predictions_0 - mod_def$model$energia_agua_refrigerada
rediddd <- c(0, residd)  # Añadir un cero inicial para mantener la longitud correcta
MAE_0 <- mean(abs(rediddd))
RMSE_0 <- sqrt(mean(rediddd^2))
print(MAE_0)
print(RMSE_0)

# Añadir las predicciones al dataframe para visualización
dataframe_trabajo$predicciones <- c(0, predictions_0)  # Ajustar la longitud de las predicciones

# ---------------------------------- VISUALIZACIÓN DEL MODELO SIMPLE ----------------------------------
# Comparar gráficamente la energía real vs. predicha por el modelo con temperatura exterior como color

ggplot(dataframe_trabajo, aes(x = energia_agua_refrigerada, y = predicciones)) +
  geom_point(aes(color = temperatura_exterior), alpha = 0.6) +  # Transparencia para ver mejor los puntos
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Línea de referencia y=x
  labs(x = "Energía Consumida Real (kWh)", y = "Energía Consumida Predicha (kWh)",
       title = "Comparación de Energía Consumida Real vs. Predicha") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")


# -------------------------------------- ANÁLISIS DE RESIDUOS ----------------------------------------
# Análisis de residuos y cálculo de error estándar y margen de error

residuoss <- residuals(mod_def)
dataframe_trabajo$residuos <- c(0, residuoss)

# Calcular el error estándar de los residuos
std_error <- sd(dataframe_trabajo$residuos)

# Calcular el margen de error para un intervalo de confianza del 95%
margin_error <- 1.96 * std_error

# Visualizar residuos vs. predicciones con intervalo de confianza
ggplot(dataframe_trabajo, aes(x = energia_agua_refrigerada, y = predicciones)) +
  geom_point(aes(color = temperatura_exterior), alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_abline(intercept = margin_error, slope = 1, linetype = "dotted", color = "blue") +
  geom_abline(intercept = -margin_error, slope = 1, linetype = "dotted", color = "blue") +
  labs(x = "Energía Consumida Real (kWh)", y = "Energía Consumida Predicha (kWh)",
       title = "Comparación Real vs. Predicha con Intervalo de Confianza") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")


# ---------------------------- ANÁLISIS EXPLORATORIO DE VARIABLES INDEPENDIENTES ---------------------
# Analizar residuos en función de variables clave como ocupación, radiación solar, temperatura, etc.

# Residuos vs. Ocupación
ggplot(dataframe_trabajo, aes(x = ocupantes_conteo_robus3, y = residuos)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Ocupación por conteo", y = "Residuos", title = "Residuos vs. Ocupación por Conteo")

# Residuos vs. Radiación Solar
ggplot(dataframe_trabajo, aes(x = radiacion_global_fachada, y = residuos)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Radiación solar", y = "Residuos", title = "Residuos vs. Radiación Solar")

# Residuos vs. Temperatura Exterior
ggplot(dataframe_trabajo, aes(x = temperatura_exterior, y = residuos)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Temperatura exterior", y = "Residuos", title = "Residuos vs. Temperatura Exterior")


# ---------------------------------- ANÁLISIS DE OUTLIERS Y GRÁFICOS EXTRAS ----------------------------
# Visualizar el comportamiento temporal de las variables clave como energía consumida y radiación

# Graficar evolución de la radiación solar durante una semana específica
datos_filtrados <- dataframe_trabajo %>%
  filter(marca_tiempo >= as.POSIXct("2021-09-20") & marca_tiempo <= as.POSIXct("2021-09-24 23:59:59"))

ggplot(datos_filtrados, aes(x = marca_tiempo, y = radiacion_global_fachada)) +
  geom_line() +
  labs(title = "Evolución de la Radiación Global en Fachada (20 al 24 de Septiembre de 2021)",
       x = "Fecha", y = "Radiación Global Fachada")

