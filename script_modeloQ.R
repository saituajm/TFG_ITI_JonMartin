library(MASS)
library(Metrics)
library(openxlsx)
library(caret)
library(writexl)
library(dplyr)


################# Para todo el dataset #########################

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

attach(dataframe_trabajo)

modelo=lm(energia_agua_refrigerada ~ temperatura_exterior + temperatura_exterior_1 + temperatura_exterior_2 + temperatura_exterior_3 +
             radiacion_global_fachada + radiacion_global_fachada_1 + radiacion_global_fachada_2 + radiacion_global_fachada_3 +
             ocupantes_conteo_robus3 + ocupantes_conteo_robus3_1 + ocupantes_conteo_robus3_2 + ocupantes_conteo_robus3_3 +
             energia_agua_refrigerada_1 + energia_agua_refrigerada_2 + energia_agua_refrigerada_3 +
             temperatura_interior + temperatura_interior_1 + temperatura_interior_2 + temperatura_interior_3)

summary(modelo)

seleccion = stepAIC(modelo, direction = c("both"))
summary(seleccion)

summary_seleccion <- summary(seleccion)



R2 <- summary_seleccion$r.squared
predictions_0 <- predict(modelo)
predictions <- predict(seleccion)
MAE <- mean(abs(predictions - seleccion$model$energia_agua_refrigerada))
MAE_0 <- mean(abs(predictions_0 - seleccion$model$energia_agua_refrigerada))


residuos_modelo_original <- resid(modelo)
residuos_modelo_seleccionado <- resid(seleccion)


data_residuos <- data.frame(Residuos_Original = residuos_modelo_original,
                            Residuos_Seleccionado = residuos_modelo_seleccionado,
                            Fitted_Values = fitted(seleccion))

ggplot(data_residuos, aes(x = Fitted_Values, y = Residuos_Seleccionado)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Gráfico de Residuos del Modelo Seleccionado",
       x = "Valores Ajustados",
       y = "Residuos") +
  theme_minimal()

residuos <- abs(predictions - seleccion$model$energia_agua_refrigerada)
residuos_ajustados <- c(0, 0, 0, residuos)
dataframe_trabajo$Error = residuos_ajustados

analisis <- dataframe_trabajo %>%
  group_by(ocupantes_conteo_robus3) %>%
  summarise(Media_Error = mean(Error), Mediana_Error = median(Error), Max_Error = max(Error), Min_Error = min(Error))


ggplot(dataframe_trabajo, aes(x = ocupantes_conteo_robus3, y = Error)) +
  geom_boxplot() +
  labs(title = "Error de Predicción por Número de Ocupantes",
       x = "Número de Ocupantes",
       y = "Error de Predicción") +
  theme_minimal()

############## Solo para el día #####################


dataframe_trabajo2<-readRDS("C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/rds_files/dataframe_dia.rds")

attach(dataframe_trabajo2)

modelo2=lm(energia_agua_refrigerada ~ temperatura_exterior + temperatura_exterior_1 + temperatura_exterior_2 + temperatura_exterior_3 +
            radiacion_global_fachada + radiacion_global_fachada_1 + radiacion_global_fachada_2 + radiacion_global_fachada_3 +
            ocupantes_conteo_robus3 + ocupantes_conteo_robus3_1 + ocupantes_conteo_robus3_2 + ocupantes_conteo_robus3_3 +
            energia_agua_refrigerada_1 + energia_agua_refrigerada_2 + energia_agua_refrigerada_3 +
            temperatura_interior + temperatura_interior_1 + temperatura_interior_2 + temperatura_interior_3)

summary(modelo2)

seleccion2 = stepAIC(modelo2, direction = c("both"))
summary(seleccion2)

summary_seleccion2 <- summary(seleccion2)

R22 <- summary_seleccion2$r.squared
predictions2 <- predict(seleccion2)
MAE2 <- mean(abs(predictions2 - seleccion2$model$energia_agua_refrigerada))

######################### Borrar todo #######################

rm(dataframe_trabajo, dataframe_trabajo2, modelo, seleccion, summary_seleccion, MAE, obs_anteriores, predictions, R2, sampleo,
   modelo2, seleccion2, summary_seleccion2, MAE2, predictions2, R22)



############## Con 30 mins ####################################

dataframe_trabajo3<-readRDS("C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/rds_files/dataframe_30.rds")

attach(dataframe_trabajo3)

modelo3=lm(energia_agua_refrigerada ~ temperatura_exterior + temperatura_exterior_1 + temperatura_exterior_2 + temperatura_exterior_3 +
             radiacion_global_fachada + radiacion_global_fachada_1 + radiacion_global_fachada_2 + radiacion_global_fachada_3 +
             ocupantes_conteo_robus3 + ocupantes_conteo_robus3_1 + ocupantes_conteo_robus3_2 + ocupantes_conteo_robus3_3 +
             energia_agua_refrigerada_1 + energia_agua_refrigerada_2 + energia_agua_refrigerada_3 +
             temperatura_interior + temperatura_interior_1 + temperatura_interior_2 + temperatura_interior_3)

summary(modelo3)

seleccion3 = stepAIC(modelo3, direction = c("both"))
summary(seleccion3)

summary_seleccion3 <- summary(seleccion3)


R23 <- summary_seleccion3$r.squared
predictions3 <- predict(seleccion3)
MAE3 <- mean(abs(predictions3 - seleccion3$model$energia_agua_refrigerada))

residuos <- predictions3 - seleccion3$model$energia_agua_refrigerada

df_residuos <- data.frame(residuos)

ruta_archivo <- "C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/output/Residuos/residuos.xlsx"
write.xlsx(df_residuos, file = ruta_archivo, sheetName = "Residuos", rowNames = FALSE)

difmax <- max(abs(predictions3 - seleccion3$model$energia_agua_refrigerada))

############# Solo añadiendo ###############
