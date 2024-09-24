library(MASS)
library(Metrics)
library(openxlsx)
library(caret)
library(writexl)
library(dplyr)
library(ggplot2)

rm(dataframe_trabajo)

dataframe_trabajo<-readRDS("C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/rds_files/dataframe_dia_bueno.rds")
dataframe_trabajo$dif_sp_tin <- dataframe_trabajo$setpoint_temperatura - dataframe_trabajo$temperatura_interior
dataframe_trabajo$dif_sp_tin_1 <- dataframe_trabajo$setpoint_temperatura_1 - dataframe_trabajo$temperatura_interior_1
dataframe_trabajo$dif_sp_tin_2 <- dataframe_trabajo$setpoint_temperatura_2 - dataframe_trabajo$temperatura_interior_2

attach(dataframe_trabajo)

mod_def <- lm(energia_agua_refrigerada ~ dif_sp_tin + radiacion_global_fachada + 
                energia_agua_refrigerada_1)

summary(mod_def)

predictions_0 <- predict(mod_def)
MAE_0 <- mean(abs(predictions_0 - mod_def$model$energia_agua_refrigerada))
prediction_correct <- c(0,predictions_0)
dataframe_trabajo$predicciones <- prediction_correct


seleccion = stepAIC(mod_def, direction = c("both"))
summary(seleccion)

predictions <- predict(seleccion)
MAE <- mean(abs(predictions - seleccion$model$energia_agua_refrigerada))


rm(mod_def, seleccion, MAE_0, predictions_0, prediction_correct, MAE, predictions)
