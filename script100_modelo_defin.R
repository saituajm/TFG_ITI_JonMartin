library(MASS)
library(Metrics)
library(openxlsx)
library(caret)
library(writexl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(GGally)
library(broom)
library(scales)
library(tidyr)
library(lmtest)
library(nortest)

dataframe_trabajo<-readRDS("C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/rds_files/dataframe_60.3.rds")
dataframe_trabajo$dif_sp_tin <- dataframe_trabajo$setpoint_temperatura - dataframe_trabajo$temperatura_interior
dataframe_trabajo$dif_sp_tin_1 <- dataframe_trabajo$setpoint_temperatura_1 - dataframe_trabajo$temperatura_interior_1
dataframe_trabajo$dif_sp_tin_2 <- dataframe_trabajo$setpoint_temperatura_2 - dataframe_trabajo$temperatura_interior_2

###------------- Montaje modelo------------------#####

attach(dataframe_trabajo)

df_simple <- dataframe_trabajo %>%
  select( energia_agua_refrigerada, temperatura_exterior, radiacion_global_fachada, 
          ocupantes_conteo_robus3,energia_agua_refrigerada_1,dif_sp_tin)

mod_def <- lm(energia_agua_refrigerada ~ dif_sp_tin + radiacion_global_fachada + 
               energia_agua_refrigerada_1)

summary(mod_def)

predictions_0 <- predict(mod_def)
residd <- predictions_0-mod_def$model$energia_agua_refrigerada
rediddd <- c(0, residd)
MAE_0 <- mean(abs(rediddd))
RMSE_0 <- sqrt(mean(rediddd^2))
print(MAE_0)
print(RMSE_0)
prediction_correct <- c(0,predictions_0)



dataframe_trabajo$predicciones <- prediction_correct

ggplot(dataframe_trabajo, aes(x = energia_agua_refrigerada, y = predicciones)) +
  geom_point(aes(color = temperatura_exterior), alpha = 0.6) +  # Usar transparencia para ver mejor los puntos superpuestos
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Línea y=x para guiar el ojo
  labs(x = "Energía Consumida Real (kWh)",
       y = "Energía Consumida Predicha (kWh)",
       title = "Comparación de Energía Consumida Real vs. Predicha") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")  # Gradiente de color para visualizar la temperatura exterior



seleccion = stepAIC(mod_def, direction = c("both"))
summary(seleccion)

predictions <- predict(seleccion)
MAE <- mean(abs(predictions - seleccion$model$energia_agua_refrigerada))


rm(mod_def, seleccion, MAE_0, predictions_0, prediction_correct, MAE, predictions, nuevo_df, nuevo_df_2, df,
   margin_error, residuoss, std_error)


###################################################################################################################


mod_def_1 <- lm(energia_agua_refrigerada ~ temperatura_exterior + temperatura_exterior_1 + temperatura_exterior_2 + 
                radiacion_global_fachada + radiacion_global_fachada_1 + radiacion_global_fachada_2  +
                ocupantes_conteo_robus3 + ocupantes_conteo_robus3_1 + ocupantes_conteo_robus3_2 +
                energia_agua_refrigerada_1 + energia_agua_refrigerada_2 +
                dif_sp_tin + dif_sp_tin_1 + dif_sp_tin_2)

mod_prueba <- stepAIC(mod_def_1, directon = c("both"))
pred_prueba <- predict(mod_prueba)
pred_prueba_cor <- c(0,0, pred_prueba)

#############################################################################################################
residuoss <- residuals(mod_def)
dataframe_trabajo$residuos <- c(0, residuoss)

# Calcula el error estándar de los residuos
std_error <- sd(dataframe_trabajo$residuos)

# Calcula el margen de error para el 95% de confianza
# Aproximadamente 1.96 * std_error para un intervalo de confianza del 95%
margin_error <- 1.96 * std_error

ggplot(dataframe_trabajo, aes(x = energia_agua_refrigerada, y = predicciones)) +
  geom_point(aes(color = temperatura_exterior), alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_abline(intercept = margin_error, slope = 1, linetype = "dotted", color = "blue") +  # Línea superior del intervalo
  geom_abline(intercept = -margin_error, slope = 1, linetype = "dotted", color = "blue") +  # Línea inferior del intervalo
  labs(x = "Energía Consumida Real (kWh)",
       y = "Energía Consumida Predicha (kWh)",
       title = "Comparación de Energía Consumida Real vs. Predicha con Intervalo de Confianza") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")

###################### Analisis exploratorio ocupantes ##########################
ggplot(dataframe_trabajo, aes(x = ocupantes_conteo_robus3, y = residuos)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +  # Añade una línea de tendencia
  labs(x = "Ocupación por conteo", y = "Residuos", title = "Residuos vs. Ocupación por conteo")

###################### Analisis exploratorio radiación ##########################
ggplot(dataframe_trabajo, aes(x = radiacion_global_fachada, y = residuos)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +  # Añade una línea de tendencia
  labs(x = "Radiación solar", y = "Residuos", title = "Residuos vs. Radiación solar")

###################### Analisis exploratorio temp ex ##########################
ggplot(dataframe_trabajo, aes(x = temperatura_exterior, y = residuos)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +  # Añade una línea de tendencia
  labs(x = "Temperatura exterior", y = "Residuos", title = "Residuos vs. Temperatura exterior")


###################### Analisis exploratorio difsptin ##########################
ggplot(dataframe_trabajo, aes(x = dif_sp_tin, y = residuos)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +  # Añade una línea de tendencia
  labs(x = "SP-Tin", y = "Residuos", title = "Residuos vs. SP-Tin")


#------------------ Radiación respecto a la hora---------------------#

dataframe_trabajo$marca_tiempo <- as.POSIXct(dataframe_trabajo$marca_tiempo, format="%Y-%m-%d %H:%M:%S")

fechas_interes <- seq(as.Date("2021-09-20"), as.Date("2021-09-24"), by = "day")
horas_interes <- c("07:30:00", "20:30:00")
momentos_interes <- as.POSIXct(paste(outer(fechas_interes, horas_interes, paste), " ", sep=""))



datos_filtrados <- dataframe_trabajo %>%
  filter(marca_tiempo >= as.POSIXct("2021-09-20") & marca_tiempo <= as.POSIXct("2021-09-24 23:59:59"))

p <- ggplot(datos_filtrados, aes(x=marca_tiempo, y=radiacion_global_fachada)) +
  geom_line() +
  labs(title="Evolución de la Radiación Global en la Fachada durante la Semana del 20 de Septiembre de 2021",
       x="Fecha y hora", 
       y="Radiación Global (W/m2)") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%d %b %H:%M", date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Agregar líneas verticales rojas en los momentos de interés
for (momento in momentos_interes) {
  p <- p + geom_vline(xintercept = momento, color = "red", linetype = "dashed")
}

# Mostrar el gráfico
print(p)


#--------------------- Lo mismo pero para la carga térmica ------------------#

datos_filtrados <- dataframe_trabajo %>%
  filter(marca_tiempo >= as.POSIXct("2021-09-20") & marca_tiempo <= as.POSIXct("2021-09-24 23:59:59"))

f <- ggplot(datos_filtrados, aes(x=marca_tiempo, y=energia_agua_refrigerada)) +
  geom_line() +
  labs(title="Evolución de la carga térmica durante la Semana del 20 de Septiembre de 2021",
       x="Fecha y hora", 
       y="Carga térmica (kWh)") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%d %b %H:%M", date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Agregar líneas verticales rojas en los momentos de interés
for (momento in momentos_interes) {
  f <- f + geom_vline(xintercept = momento, color = "red", linetype = "dashed")
}
print(f)


###################### Graficas de interes##########################
View(df_simple)
ggpairs(df_simple)

############################### Otro analisis residual###################################

perf_aug <- augment(mod_def)
view(perf_aug)
ruta <-"C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/datasets/pref_aug.xlsx"
write_xlsx(perf_aug, ruta)
ggplot(perf_aug, aes(x = .fitted,
                     y = .resid)) +
  geom_point() + 
  geom_hline(yintercept = 0)

rmse(energia_agua_refrigerada, prediction_correct)
rmse(energia_agua_refrigerada, pred_prueba_cor)

summary(mod_prueba)

perf_aug_prueba <- augment(mod_prueba)

ggplot(perf_aug_prueba, aes(x = .fitted,
                     y = .resid)) +
  geom_point() + 
  geom_hline(yintercept = 0)

######################### VIendo outliers #################
dataframe_trabajo %>%
ggplot(aes(x=marca_tiempo, y=energia_agua_refrigerada)) +
  geom_line()


# Suponiendo que marca_tiempo es de tipo POSIXct, si no, necesitas convertirlo
dataframe_trabajo_p <- dataframe_trabajo %>%
  mutate(marca_tiempo = as.POSIXct(marca_tiempo, format = "%Y-%m-%d %H:%M:%S")) # Ajusta el formato según tus datos

# Filtra los datos para una semana específica
# Aquí estamos suponiendo que quieres la semana del 1 de enero de 2023
semana_inicio <- as.POSIXct("2021-09-20")
semana_fin <- semana_inicio + days(2)

dataframe_filtrado <- dataframe_trabajo_p %>%
  filter(marca_tiempo >= semana_inicio & marca_tiempo < semana_fin)

# Ahora crea el gráfico con ggplot
dataframe_filtrado_long <- dataframe_filtrado %>%
  pivot_longer(cols = c(energia_agua_refrigerada, radiacion_global_fachada),
               names_to = "variable",
               values_to = "valor")

ggplot(dataframe_filtrado, aes(x=marca_tiempo, y=energia_agua_refrigerada)) +
  geom_line() +
  labs(title = "Consumo de Energía de Agua Refrigerada por Semana",
       x = "Marca de Tiempo",
       y = "Energía (Unidad)") +
  theme_minimal()

ggplot(dataframe_filtrado_long, aes(x=marca_tiempo, y=valor)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title="Energía de Agua Refrigerada y Radiación Solar",
       x="Marca de Tiempo",
       y="Valor") +
  theme_minimal()
####################### Pruebas de residuos ##############################

dwtest(mod_def)
residuos <- c(0, perf_aug$.resid)
hist(residuos)
normalidad <- lillie.test(residuos)
print(normalidad)
attach(dataframe_trabajo)
varianza_constante <- bartlett.test(list(energia_agua_refrigerada, dif_sp_tin,
                                         radiacion_global_fachada, energia_agua_refrigerada_1), data=dataframe_trabajo)
print(varianza_constante)
detach(dataframe_trabajo)
#---------------- Analisis por tramos de los residuos ---------------------#

# Supongamos que tu dataframe se llama df, y que tienes las columnas 'radiacion_solar' y 'residuos'
# Primero, definimos los tramos de la radiación solar, por ejemplo:
breaks = quantile(dataframe_trabajo$radiacion_global_fachada, probs = seq(0, 1, length.out = 5), na.rm = TRUE)
labels = c("Muy baja", "Baja", "Media", "Alta")

# Creamos una nueva columna que categoriza la radiación solar en tramos
dataframe_trabajo$tramo_radiacion = cut(dataframe_trabajo$radiacion_global_fachada, breaks = breaks, labels = labels, include.lowest = TRUE)

# Ahora agrupamos por tramo de radiación y calculamos estadísticas de los residuos
resumen_residuos = aggregate(residuos ~ tramo_radiacion, data = dataframe_trabajo, function(x) {
  c(media = mean(x, na.rm = TRUE), desviacion_std = sd(x, na.rm = TRUE))
})

# Desempaquetamos el resultado para tenerlo en columnas separadas
resumen_residuos$media_residuos = resumen_residuos$residuos[, "media"]
resumen_residuos$std_residuos = resumen_residuos$residuos[, "desviacion_std"]
resumen_residuos$residuos = NULL  # eliminamos la columna original de lista

# Opcional: puedes visualizar los resultados
print(resumen_residuos)

# También podrías hacer un boxplot de los residuos por tramo de radiación solar para visualizar la distribución
boxplot(residuos ~ tramo_radiacion, data = dataframe_trabajo, main = "Distribución de Residuos por Tramo de Radiación Solar",
        ylab = "Residuos", xlab = "Tramo de Radiación Solar")

ggplot(resumen_residuos, aes(x = tramo_radiacion, y = media_residuos, fill = tramo_radiacion)) +
  geom_col(color = "black", linewidth = 0.7) +
  geom_errorbar(aes(ymin = media_residuos - std_residuos, ymax = media_residuos + std_residuos), width = 0.2, color = "black") +
  scale_fill_manual(values = c("Muy baja" = "#fbb4ae", "Baja" = "#b3cde3", "Media" = "#ccebc5", "Alta" = "#decbe4")) +
  labs(title = "Media y Desviación Estándar de Residuos por Tramo de Radiación Solar",
       subtitle = "Análisis de los residuos del modelo en función de la radiación solar",
       x = "Tramo de Radiación Solar",
       y = "Media de Residuos",
       fill = "Tramo de Radiación") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(color = "gray20"),
        axis.title = element_text(face = "bold"))

##-------------------------------- Visualización de tramos ---------------------#

breaks_0 = quantile(dataframe_trabajo$radiacion_global_fachada, probs = seq(0, 1, length.out = 5), na.rm = TRUE)

# Crear un data frame para mostrar los tramos y sus límites
tramos_radiacion_0 = data.frame(
  Tramo = c("Muy baja", "Baja", "Media", "Alta"),
  Desde = breaks[-length(breaks)],
  Hasta = breaks[-1]
)

print(tramos_radiacion_0)

######---------- Tramos especificos radiación --------------------#####

dataframe_trabajo$radiacion_tramo_m <- cut(dataframe_trabajo$radiacion_global_fachada,
                          breaks = c(-Inf, 15, 70, 110, Inf),
                          labels = c("Muy baja", "Baja", "Media", "Alta"),
                          right = TRUE)

radiacion_resumen <- aggregate(dataframe_trabajo$radiacion_global_fachada,
                               by = list(dataframe_trabajo$radiacion_tramo_m),
                               FUN = function(x) c(min = min(x), max = max(x)))

names(radiacion_resumen) <- c("Tramo", "Valores")
radiacion_resumen$Min <- sapply(radiacion_resumen$Valores, function(x) x[1])
radiacion_resumen$Max <- sapply(radiacion_resumen$Valores, function(x) x[2])
radiacion_resumen <- radiacion_resumen[, -2]

print(radiacion_resumen)

detach(dataframe_trabajo)
dataframe_trabajo$residuos <- residuos

attach(dataframe_trabajo)
resumen_errores <- dataframe_trabajo %>%
  group_by(radiacion_tramo_m) %>%
  summarise(
    Media_Error = mean(residuos, na.rm = TRUE),
    SD_Error = sd(residuos, na.rm = TRUE)
  )
print(resumen_errores)


library(ggplot2)

ggplot(resumen_errores, aes(x = radiacion_tramo_m, y = Media_Error, fill = radiacion_tramo_m)) +
  geom_col(color = "black", linewidth = 0.7) +  # Corregido 'linewidth' en lugar de 'size'
  geom_errorbar(aes(ymin = Media_Error - SD_Error, ymax = Media_Error + SD_Error), 
                width = 0.2, color = "black") +  # Corregido el nombre de la variable
  scale_fill_brewer(palette = "Pastel1") +  # Usar una paleta de colores más vistosa
  labs(title = "Media y Desviación Estándar del Error por Tramo de Radiación",
       subtitle = "Análisis de los residuos del modelo en función de la radiación solar",
       x = "Tramo de Radiación Solar",
       y = "Media del Error",
       fill = "Tramo") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),  # Dar estilo a los títulos
        plot.subtitle = element_text(color = "gray40"),
        legend.title = element_text(face = "bold"),  # Negrita en los títulos de leyenda
        legend.position = "bottom",  # Mover la leyenda abajo
        axis.text = element_text(color = "gray20"),  # Texto de los ejes en gris oscuro
        axis.title = element_text(face = "bold"))  # Títulos de ejes en negrita


##------------------- Separación en cuartiles del resto de variables------------#####

breaks_1 = quantile(dataframe_trabajo$temperatura_exterior, probs = seq(0, 1, length.out = 5),
                  na.rm = TRUE)
labels_1 = c("Muy baja", "Baja", "Media", "Alta")

# Creamos una nueva columna que categoriza la radiación solar en tramos
dataframe_trabajo$tramo_tempex = cut(dataframe_trabajo$temperatura_exterior, 
                                        breaks = breaks_1, labels = labels_1, include.lowest = TRUE)


breaks_2 = quantile(dataframe_trabajo$dif_sp_tin, probs = seq(0, 1, length.out = 5),
                  na.rm = TRUE)
labels_2 = c("Muy baja", "Baja", "Media", "Alta")

# Creamos una nueva columna que categoriza la radiación solar en tramos
dataframe_trabajo$tramo_dspt = cut(dataframe_trabajo$dif_sp_tin, 
                                     breaks = breaks_2, labels = labels_2, include.lowest = TRUE)


breaks_3 = quantile(dataframe_trabajo$ocupantes_conteo_robus3, probs = seq(0, 1, length.out = 5),
                  na.rm = TRUE)
labels_3 = c("Muy baja", "Baja", "Media", "Alta")

# Creamos una nueva columna que categoriza la radiación solar en tramos
dataframe_trabajo$tramo_ocup = cut(dataframe_trabajo$ocupantes_conteo_robus3, 
                                     breaks = breaks_3, labels = labels_3, include.lowest = TRUE)


breaks_4 = quantile(dataframe_trabajo$energia_agua_refrigerada_1, probs = seq(0, 1, length.out = 5),
                  na.rm = TRUE)
labels_4 = c("Muy baja", "Baja", "Media", "Alta")

# Creamos una nueva columna que categoriza la radiación solar en tramos
dataframe_trabajo$ene_1 = cut(dataframe_trabajo$energia_agua_refrigerada_1, 
                                     breaks = breaks_4, labels = labels_4, include.lowest = TRUE)



#------------------ Creación del dataframes con residuos ------------------------##########
attach(dataframe_trabajo)

resumen_residuos_1 = aggregate(residuos ~ tramo_tempex, data = dataframe_trabajo, function(x) {
  c(media = mean(x, na.rm = TRUE), desviacion_std = sd(x, na.rm = TRUE))
})

resumen_residuos_1$media_residuos = resumen_residuos_1$residuos[, "media"]
resumen_residuos_1$std_residuos = resumen_residuos_1$residuos[, "desviacion_std"]
resumen_residuos_1$residuos = NULL  # eliminamos la columna original de lista

print(resumen_residuos_1)

resumen_residuos_2 = aggregate(residuos ~ tramo_dspt, data = dataframe_trabajo, function(x) {
  c(media = mean(x, na.rm = TRUE), desviacion_std = sd(x, na.rm = TRUE))
})

resumen_residuos_2$media_residuos = resumen_residuos_2$residuos[, "media"]
resumen_residuos_2$std_residuos = resumen_residuos_2$residuos[, "desviacion_std"]
resumen_residuos_2$residuos = NULL  # eliminamos la columna original de lista

resumen_residuos_3 = aggregate(residuos ~ tramo_ocup, data = dataframe_trabajo, function(x) {
  c(media = mean(x, na.rm = TRUE), desviacion_std = sd(x, na.rm = TRUE))
})

resumen_residuos_3$media_residuos = resumen_residuos_3$residuos[, "media"]
resumen_residuos_3$std_residuos = resumen_residuos_3$residuos[, "desviacion_std"]
resumen_residuos_3$residuos = NULL  # eliminamos la columna original de lista

resumen_residuos_4 = aggregate(residuos ~ ene_1, data = dataframe_trabajo, function(x) {
  c(media = mean(x, na.rm = TRUE), desviacion_std = sd(x, na.rm = TRUE))
})

resumen_residuos_4$media_residuos = resumen_residuos_4$residuos[, "media"]
resumen_residuos_4$std_residuos = resumen_residuos_4$residuos[, "desviacion_std"]
resumen_residuos_4$residuos = NULL  # eliminamos la columna original de lista

#-------------------- Grafiación tempex -----------------#########

boxplot(residuos ~ tramo_tempex, data = dataframe_trabajo, main = "Distribución de Residuos por Tramo de Temperatura Exterior",
        ylab = "Residuos", xlab = "Tramo de temperatura exterior")

ggplot(resumen_residuos_1 , aes(x = tramo_tempex, y = media_residuos, fill = tramo_tempex)) +
  geom_col(color = "black", linewidth = 0.7) +
  geom_errorbar(aes(ymin = media_residuos - std_residuos, ymax = media_residuos + std_residuos), width = 0.2, color = "black") +
  scale_fill_manual(values = c("Muy baja" = "#fbb4ae", "Baja" = "#b3cde3", "Media" = "#ccebc5", "Alta" = "#decbe4")) +
  labs(title = "Media y Desviación Estándar de Residuos por Tramo de Temperatura Exterior",
       subtitle = "Análisis de los residuos del modelo en función de la temperatura exterior",
       x = "Tramo de Temperatura Exterior",
       y = "Media de Residuos",
       fill = "Tramo de Temperatura Exterior") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(color = "gray20"),
        axis.title = element_text(face = "bold"))


#-------------------- Grafiación dstp -----------------#########
boxplot(residuos ~ tramo_dspt, data = dataframe_trabajo, main = "Distribución de Residuos por Diferencia entre Consigna y Temperatura Interior",
        ylab = "Residuos", xlab = "Tramo de diferencia consigna y temperatura interior")

ggplot(resumen_residuos_2 , aes(x = tramo_dspt, y = media_residuos, fill = tramo_dspt)) +
  geom_col(color = "black", linewidth = 0.7) +
  geom_errorbar(aes(ymin = media_residuos - std_residuos, ymax = media_residuos + std_residuos), width = 0.2, color = "black") +
  scale_fill_manual(values = c("Muy baja" = "#fbb4ae", "Baja" = "#b3cde3", "Media" = "#ccebc5", "Alta" = "#decbe4")) +
  labs(title = "Media y Desviación Estándar de Residuos por Tramo de Diferencia entre Consigna y Temperatura Interior",
       subtitle = "Análisis de los residuos del modelo en función de la Diferencia entre Consigna y Temperatura Interior",
       x = "Tramo de Diferencia entre Consigna y Temperatura Interior",
       y = "Media de Residuos",
       fill = "Tramo de Diferencia entre Consigna y Temperatura Interior") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(color = "gray20"),
        axis.title = element_text(face = "bold"))


#-------------------- Grafiación ocup -----------------#########
boxplot(residuos ~ tramo_ocup, data = dataframe_trabajo, main = "Distribución de Residuos por Ocupación",
        ylab = "Residuos", xlab = "Tramo de Ocupación")

ggplot(resumen_residuos_3, aes(x = tramo_ocup, y = media_residuos, fill = tramo_ocup)) +
  geom_col(color = "black", linewidth = 0.7) +
  geom_errorbar(aes(ymin = media_residuos - std_residuos, ymax = media_residuos + std_residuos), width = 0.2, color = "black") +
  scale_fill_manual(values = c("Muy baja" = "#fbb4ae", "Baja" = "#b3cde3", "Media" = "#ccebc5", "Alta" = "#decbe4")) +
  labs(title = "Media y Desviación Estándar de Residuos por Tramo de Ocupación",
       subtitle = "Análisis de los residuos del modelo en función de la Ocupación",
       x = "Tramo de Ocupación",
       y = "Media de Residuos",
       fill = "Tramo de Ocupación") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(color = "gray20"),
        axis.title = element_text(face = "bold"))

#-------------------- Grafiación ene_1 -----------------#########
boxplot(residuos ~ ene_1, data = dataframe_trabajo, main = "Distribución de Residuos por Carga Térmica t-1",
        ylab = "Residuos", xlab = "Tramo de Carga Térmica t-1")

ggplot(resumen_residuos_4, aes(x = ene_1, y = media_residuos, fill = ene_1)) +
  geom_col(color = "black", linewidth = 0.7) +
  geom_errorbar(aes(ymin = media_residuos - std_residuos, ymax = media_residuos + std_residuos), width = 0.2, color = "black") +
  scale_fill_manual(values = c("Muy baja" = "#fbb4ae", "Baja" = "#b3cde3", "Media" = "#ccebc5", "Alta" = "#decbe4")) +
  labs(title = "Media y Desviación Estándar de Residuos por Tramo de Carga Térmica t-1",
       subtitle = "Análisis de los residuos del modelo en función de la Carga Térmica t-1",
       x = "Tramo de Carga Térmica t-1r",
       y = "Media de Residuos",
       fill = "Tramo de Carga Térmica t-1") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(color = "gray20"),
        axis.title = element_text(face = "bold"))

#---------------------- Rangos de cada cuartil -------------------------------#

tramos_tempex_0 = data.frame(
  Tramo = c("Muy baja", "Baja", "Media", "Alta"),
  Desde = breaks_1[-length(breaks_1)],
  Hasta = breaks_1[-1]
)
print(tramos_tempex_0)

tramos_dspt_0 = data.frame(
  Tramo = c("Muy baja", "Baja", "Media", "Alta"),
  Desde = breaks_2[-length(breaks_2)],
  Hasta = breaks_2[-1]
)
print(tramos_dspt_0)

tramos_ocup_0 = data.frame(
  Tramo = c("Muy baja", "Baja", "Media", "Alta"),
  Desde = breaks_3[-length(breaks_3)],
  Hasta = breaks_3[-1]
)
print(tramos_ocup_0)

tramos_ene1_0 = data.frame(
  Tramo = c("Muy baja", "Baja", "Media", "Alta"),
  Desde = breaks_4[-length(breaks_4)],
  Hasta = breaks_4[-1]
)
print(tramos_ene1_0)
