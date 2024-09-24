library(MASS)
library(Metrics)
library(openxlsx)
install.packages("caret")
library(caret)
library(writexl)

install.packages("Metrics")
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
modelo=lm(temperatura_interior ~ 0 + temperatura_exterior + radiacion_global_fachada + ocupantes_conteo_robus3 + 
            ocupantes_conteo_robus3_1 + ocupantes_conteo_robus3_2 + ocupantes_conteo_robus3_3 +
            temperatura_interior_1 + temperatura_interior_2 + temperatura_interior_3)
summary(modelo)

seleccion=stepAIC(modelo, direction = c("both"))
summary(seleccion)

######Modelo 1 con menos varibles#################################################################################

modelo2=lm(temperatura_interior ~ 0 + temperatura_exterior + temperatura_exterior_1 + temperatura_exterior_2 +
             radiacion_global_fachada + radiacion_global_fachada_1 + radiacion_global_fachada_2 + 
             ocupantes_conteo_robus3 + ocupantes_conteo_robus3_1 + ocupantes_conteo_robus3_2 +
             energia_agua_refrigerada + energia_agua_refrigerada_1 + energia_agua_refrigerada_2 +
             temperatura_interior_1 + temperatura_interior_2)
summary(modelo2)

seleccion2 = stepAIC(modelo2, direction = c("both"))
summary(seleccion2)

summary_seleccion2 <- summary(seleccion2)
# Calcular el R2
R2 <- summary_seleccion2$r.squared

# Calcular el MAE
predictions <- predict(seleccion2)
MAE <- mean(abs(predictions - seleccion2$model$temperatura_interior))

# Obtener el número de variables (sin contar el intercepto)
num_variables <- length(seleccion2$coefficients)

# Crear un data frame con los resultados
resultados <- data.frame(
  R2 = R2,
  MAE = MAE,
  num_variables = num_variables
)

# Crear un archivo Excel y escribir los resultados
wb <- createWorkbook()
addWorksheet(wb, "Resultados")
writeData(wb, "Resultados", resultados)

# Especificar la ruta donde se guardará el archivo Excel
ruta_excel <- "C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/output/Modelos con reducción de varibles/resultados_modelo1.xlsx"


saveWorkbook(wb, ruta_excel, overwrite = TRUE)

summary(seleccion2)
summary(modelo2)
################################## Forzando el intercept a 0 #####################################################################

stepAIC_custom <- function(model) {
  current_model <- model
  best_model <- model
  best_aic <- AIC(model)
  
  while(TRUE) {
    candidate_models <- list()
    terms <- attr(terms(current_model), "term.labels")
    
    for (term in terms) {
      formula <- as.formula(paste("temperatura_interior ~ 0 +", paste(setdiff(terms, term), collapse = " + ")))
      candidate_model <- lm(formula, data = dataframe_trabajo)
      candidate_models[[term]] <- candidate_model
    }
    
    aic_values <- sapply(candidate_models, AIC)
    min_aic <- min(aic_values)
    
    if (min_aic < best_aic) {
      best_aic <- min_aic
      best_model <- candidate_models[[which.min(aic_values)]]
      current_model <- best_model
    } else {
      break
    }
  }
  
  return(best_model)
}

seleccion2_0 <- stepAIC_custom(modelo2)
summary_seleccion2_0 <- summary(seleccion2_0)

# Calcular el R2
R2_0 <- summary_seleccion2_0$r.squared

# Calcular el MAE
predictions0 <- predict(seleccion2_0)
MAE_0 <- mean(abs(predictions0 - dataframe_trabajo$temperatura_interior))

# Obtener el número de variables
num_variables0 <- length(seleccion2_0$coefficients)

# Crear un data frame con los resultados
resultados0 <- data.frame(
  R2 = R2_0,
  MAE = MAE_0,
  num_variables = num_variables0
)

# Crear un archivo Excel y escribir los resultados
wb <- createWorkbook()
addWorksheet(wb, "Resultados0")
writeData(wb, "Resultados0", resultados0)

# Especificar la ruta donde se guardará el archivo Excel
ruta_excel0 <- "C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/output/Modelos con reducción de varibles/resultados_modelo_itcp0.xlsx"

# Guardar el archivo Excel
saveWorkbook(wb, ruta_excel0, overwrite = TRUE)
summary(seleccion2_0)

################################# Modelo 2 mejorado####################################################
modelo3=lm(temperatura_interior ~ 0 + temperatura_exterior + radiacion_global_fachada + ocupantes_conteo_robus3 + 
             ocupantes_conteo_robus3_1 + ocupantes_conteo_robus3_2 + ocupantes_conteo_robus3_3 +
             temperatura_interior_1 + temperatura_interior_2 + temperatura_interior_3)

summary(modelo3)

seleccion3 = stepAIC(modelo3, direction = c("both"))
summary(seleccion3)

summary_seleccion3 <- summary(seleccion3)
# Calcular el R2
R2 <- summary_seleccion3$r.squared

# Calcular el MAE
predictions <- predict(seleccion3)
MAE <- mean(abs(predictions - seleccion3$model$temperatura_interior))

# Obtener el número de variables (sin contar el intercepto)
num_variables <- length(seleccion3$coefficients)

# Crear un data frame con los resultados
resultados <- data.frame(
  R2 = R2,
  MAE = MAE,
  num_variables = num_variables
)

# Crear un archivo Excel y escribir los resultados
wb <- createWorkbook()
addWorksheet(wb, "Resultados")
writeData(wb, "Resultados", resultados)

# Especificar la ruta donde se guardará el archivo Excel
ruta_excel <- "C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/output/Modelos con reducción de varibles/resultados_modelo2.xlsx"


saveWorkbook(wb, ruta_excel, overwrite = TRUE)

################################################  Modelo 4   ####################################################
modelo4=lm(temperatura_interior ~ 0 + energia_agua_refrigerada + energia_agua_refrigerada_1 + energia_agua_refrigerada_2 + energia_agua_refrigerada_3 + 
             energia_agua_refrigerada_4 + energia_agua_refrigerada_5 + energia_agua_refrigerada_6 + 
             radiacion_global_fachada + radiacion_global_fachada_1 + radiacion_global_fachada_2 + radiacion_global_fachada_3 + radiacion_global_fachada_4 + 
             ocupantes_conteo_robus3 + ocupantes_conteo_robus3_1 + ocupantes_conteo_robus3_2 + ocupantes_conteo_robus3_3 + ocupantes_conteo_robus3_4 + 
             ocupantes_conteo_robus3_5 + ocupantes_conteo_robus3_6 +
             temperatura_interior_1 + temperatura_interior_2 + temperatura_interior_3 + temperatura_interior_4 + 
             temperatura_interior_5 + temperatura_interior_6)

summary(modelo4)

seleccion4 = stepAIC(modelo4, direction = c("both"))
summary(seleccion4)

summary_seleccion4 <- summary(seleccion4)
# Calcular el R2
R2 <- summary_seleccion4$r.squared

# Calcular el MAE
predictions <- predict(seleccion4)
MAE <- mean(abs(predictions - seleccion4$model$temperatura_interior))

# Obtener el número de variables (sin contar el intercepto)
num_variables <- length(seleccion4$coefficients)

# Crear un data frame con los resultados
resultados <- data.frame(
  R2 = R2,
  MAE = MAE,
  num_variables = num_variables
)

# Crear un archivo Excel y escribir los resultados
wb <- createWorkbook()
addWorksheet(wb, "Resultados")
writeData(wb, "Resultados", resultados)

# Especificar la ruta donde se guardará el archivo Excel
ruta_excel <- "C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/output/Modelos con reducción de varibles/resultados_modelo4.xlsx"


saveWorkbook(wb, ruta_excel, overwrite = TRUE)
########################### Modelo 5, 6, 7, 8 #######################################################################################
modelo5=lm(temperatura_interior ~ energia_agua_refrigerada + energia_agua_refrigerada_1 + energia_agua_refrigerada_2 + energia_agua_refrigerada_3 + 
             energia_agua_refrigerada_4 + energia_agua_refrigerada_5 + energia_agua_refrigerada_6 + 
             radiacion_global_fachada + radiacion_global_fachada_1 + radiacion_global_fachada_2 + radiacion_global_fachada_3 + radiacion_global_fachada_4 +
             radiacion_global_fachada_5 + radiacion_global_fachada_6 +
             ocupantes_conteo_robus3 + ocupantes_conteo_robus3_1 + ocupantes_conteo_robus3_2 + ocupantes_conteo_robus3_3 + ocupantes_conteo_robus3_4 + 
             ocupantes_conteo_robus3_5 + ocupantes_conteo_robus3_6 +
             temperatura_interior_1 + temperatura_interior_2 + temperatura_interior_3 + temperatura_interior_4 + 
             temperatura_interior_5 + temperatura_interior_6 +
             temperatura_exterior + temperatura_exterior_1 + temperatura_exterior_2 + temperatura_exterior_3 + 
             temperatura_exterior_4 + temperatura_exterior_5 + temperatura_exterior_6)

summary(modelo5)

seleccion5 = stepAIC(modelo5, direction = c("both"))
summary(seleccion5)

summary_seleccion5 <- summary(seleccion5)
# Calcular el R2
R2 <- summary_seleccion5$r.squared

# Calcular el MAE
predictions <- predict(seleccion5)
MAE <- mean(abs(predictions - seleccion5$model$temperatura_interior))

# Obtener el número de variables (sin contar el intercepto)
num_variables <- length(seleccion5$coefficients)

# Crear un data frame con los resultados
resultados <- data.frame(
  R2 = R2,
  MAE = MAE,
  num_variables = num_variables
)

# Crear un archivo Excel y escribir los resultados
wb <- createWorkbook()
addWorksheet(wb, "Resultados")
writeData(wb, "Resultados", resultados)

# Especificar la ruta donde se guardará el archivo Excel
ruta_excel <- "C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/output/Modelos con reducción de varibles/resultados_modelo5.xlsx"


saveWorkbook(wb, ruta_excel, overwrite = TRUE)

preProcValues <- preProcess(dataframe_trabajo, method = c("center", "scale"))
trans <- predict(preProcValues, dataframe_trabajo)
sd(trans$temperatura_exterior)

modelo6 <- lm(temperatura_interior ~ energia_agua_refrigerada + energia_agua_refrigerada_1 + energia_agua_refrigerada_2 + energia_agua_refrigerada_3 + 
                energia_agua_refrigerada_4 + energia_agua_refrigerada_5 + energia_agua_refrigerada_6 + 
                radiacion_global_fachada + radiacion_global_fachada_1 + radiacion_global_fachada_2 + radiacion_global_fachada_3 + radiacion_global_fachada_4 +
                radiacion_global_fachada_5 + radiacion_global_fachada_6 +
                ocupantes_conteo_robus3 + ocupantes_conteo_robus3_1 + ocupantes_conteo_robus3_2 + ocupantes_conteo_robus3_3 + ocupantes_conteo_robus3_4 + 
                ocupantes_conteo_robus3_5 + ocupantes_conteo_robus3_6 +
                temperatura_interior_1 + temperatura_interior_2 + temperatura_interior_3 + temperatura_interior_4 + 
                temperatura_interior_5 + temperatura_interior_6 +
                temperatura_exterior + temperatura_exterior_1 + temperatura_exterior_2 + temperatura_exterior_3 + 
                temperatura_exterior_4 + temperatura_exterior_5 + temperatura_exterior_6, data = trans)


summary(modelo6)
summary (modelo5)

coef_estand <- lm(data.frame(scale(modelo5$model)))
coef_estand

coef_df <- data.frame(Estimate = coef(coef_estand))
ruta_excel <- "C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/output/Comparativas/coef_estand1.xlsx"

# Escribir el dataframe a un archivo Excel en la ruta especificada
write_xlsx(coef_df, ruta_excel)
######################################################################################################################################
anova = aov(seleccion2)
summary (anova)

residual=shapiro.test(resid(seleccion2))
print(residual)

rm(MAE)
maee = mae(temreal, tempmed)
maee
MAE2=mean(modelo2$residuals)


tempmed=predict(seleccion2)
temreal=dataframe_trabajo$temperatura_interior[3:nrow(dataframe_trabajo)]