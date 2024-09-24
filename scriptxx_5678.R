library(MASS)
library(Metrics)
library(openxlsx)
library(caret)
library(writexl)

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

modelo2=lm(temperatura_interior ~ temperatura_exterior + temperatura_exterior_1 + temperatura_exterior_2 + temperatura_exterior_3 +
             radiacion_global_fachada + radiacion_global_fachada_1 + radiacion_global_fachada_2 + radiacion_global_fachada_3 +
             ocupantes_conteo_robus3 + ocupantes_conteo_robus3_1 + ocupantes_conteo_robus3_2 + ocupantes_conteo_robus3_3 +
             energia_agua_refrigerada + energia_agua_refrigerada_1 + energia_agua_refrigerada_2 + energia_agua_refrigerada_3 +
             temperatura_interior_1 + temperatura_interior_2 + temperatura_interior_3)
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
ruta_excel <- "C:/Users/Admin/Desktop/TFG ITI/COD_INIGO/output/Modelos con reducción de varibles/resultados_modelo5678.xlsx"


saveWorkbook(wb, ruta_excel, overwrite = TRUE)

dataframe_trabajo2 <- dataframe_trabajo[-c(1:3), ]
difmax <- max(abs(predictions - dataframe_trabajo2$temperatura_interior))



################# Estandarizar coeficientes #############################