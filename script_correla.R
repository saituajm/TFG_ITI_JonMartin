install.packages(foreign)
install.packages("PerformanceAnalytics")
install.packages("apaTables")
install.packages("psych")
install.packages("corrr")
install.packages("writexl")
install.packages("tidyverse")
install.packages("rstatix")
install.packages("ggpubr")
install.packages("nortest")

library(openxlsx)
library(dplyr)
library(tidyverse)
library(writexl)
library(rstatix)
library(ggpubr)
library(nortest)
library(ggplot2)

wd <- getwd()
df <- readRDS(paste(wd, "/rds_files/dataset4.rds", sep = ""))
View(df)  # Corregido: V mayúscula

# Seleccionar solo las columnas numéricas
numeric_columns <- sapply(df, is.numeric)
df_numeric <- df[, numeric_columns]

# Calcular la matriz de correlación
correlacion_spearman <- cor(df_numeric, method = "spearman", use = "complete.obs") # Manejar valores faltantes
install.packages("openxlsx")
library(openxlsx)
# Crear un data frame con la matriz de correlación para una mejor visualización en Excel
correlacion_df <- as.data.frame(correlacion_spearman)
correlaciones_dir <- paste(wd, "/correlaciones_2", sep = "")
dir.create(correlaciones_dir, showWarnings = FALSE)

# Definir la ruta completa del archivo
file_path <- paste(correlaciones_dir, "/correlacion.xlsx", sep = "")

# Guardar el data frame en un archivo Excel
write_xlsx(correlacion_df, path = file_path)


#########################################################################################################################

#Ver si las variables siguen una distribución normal
attach(df)

#temperatura interior#
lillie.test(temperatura_interior)

ggdensity(temperatura_interior, main= "Temperatura Interior", xlab = "Grados Celsius (ºC)", ylab = "Densidad")

ggqqplot(temperatura_interior,  main= "Temperatura Interior", xlab = "Valor teórico)", ylab = "Muestra (ºC)")

#temperatura exterior#
lillie.test(temperatura_exterior)

ggdensity(temperatura_exterior, main= "Temperatura Exterior", xlab = "Grados Celsius (ºC)", ylab = "Densidad")

ggqqplot(temperatura_exterior,  main= "Temperatura Exterior", xlab = "Valor teórico", ylab = "Muestra (ºC)")

#radiación#
lillie.test(radiacion_solar_global_horizontal)

ggdensity(radiacion_solar_global_horizontal, main= "Radiación Global Horizontal", xlab = "W/m2", ylab = "Densidad")

ggqqplot(radiacion_solar_global_horizontal,  main= "Temperatura Exterior", xlab = "Valor teórico", ylab = "Muestra (W/m2)")

#ocupantes#
lillie.test(ocupantes_conteo_robus3)

ggdensity(ocupantes_conteo_robus3, main= "Ocupantes por conteo", xlab = "Numero de personas", ylab = "Densidad")

ggqqplot(ocupantes_conteo_robus3,  main= "Ocupantes por conteo", xlab = "Valor teórico", ylab = "Muestra (Nº personas)")

#energia por agua refrigerada#
lillie.test(energia_agua_refrigerada)

ggdensity(energia_agua_refrigerada, main= "Energía consumida por agua refrigerada", xlab = "kWh", ylab = "Densidad") + xlim(0,2.5)

ggqqplot(energia_agua_refrigerada,  main= "Energía consumida por agua refrigerada", xlab = "Valor teórico", ylab = "Muestra (kWh)")

###########Para el dataframe_60########################
df_60 <- readRDS(paste(wd, "/rds_files/dataframe_60.rds", sep = ""))

# Seleccionar solo las columnas numéricas
numeric_columns <- sapply(df_60, is.numeric)
df_numeric_60 <- df_60[, numeric_columns]

# Calcular la matriz de correlación
correlacion_spearman_60 <- cor(df_numeric_60, method = "spearman", use = "complete.obs") # Manejar valores faltantes


# Crear un data frame con la matriz de correlación para una mejor visualización en Excel
correlacion_df_60 <- as.data.frame(correlacion_spearman_60)
correlaciones_dir_60 <- paste(wd, "/correlaciones_2", sep = "")
dir.create(correlaciones_dir, showWarnings = FALSE)

# Definir la ruta completa del archivo
file_path <- paste(correlaciones_dir_60, "/correlacion_60.xlsx", sep = "")

# Guardar el data frame en un archivo Excel
write_xlsx(correlacion_df_60, path = file_path)

