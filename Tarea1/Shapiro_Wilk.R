# Librerias ---------------------------------------------------------------
library(dplyr)
library(janitor)
library(pastecs)
library(mvnormtest)
library(caret)
# Rentabilidades de los Fondos de Inversión Colectiva (FIC) ---------------
#https://www.datos.gov.co/Hacienda-y-Cr-dito-P-blico/FIC2023-21-/uqpf-7qqy


# Limpieza de datos -------------------------------------------------------

#datos <- read.csv("C:/Users/Stefanny/OneDrive - Universidad EAFIT/Eafit/Quinto semestre/estadística/proyecto/FIC2023_21__.csv")
#datos <- read.csv("C:/Users/ASUS/OneDrive - Universidad EAFIT/2023-1/Estadística_II/data(1).csv")
datos <-read.csv("C:/Users/Stefanny/OneDrive - Universidad EAFIT/Eafit/Quinto semestre/estadística/proyecto/data.csv")
datos

# Filtrar datos por fecha -------------------------------------------------

# Por Sara Gallego @SaraGallego22


datos$fecha_corte <- as.Date(datos$fecha_corte, format="%m/%d/%Y")
datos_filtrados = filter(datos,fecha_corte == "2023-01-01")
datos_filtrados

# Guardar datos -----------------------------------------------------------

write.csv(datos_filtrados, "C:/Users/Stefanny/OneDrive - Universidad EAFIT/Eafit/Quinto semestre/estadística/proyecto/data2.csv", row.names=FALSE)


# Exploración datos -------------------------------------------------------

summary(datos_filtrados)
sapply(datos_filtrados, mean, na.rm=TRUE)
stat.desc(datos_filtrados)

# Shapiro-Wilk test -------------------------------------------------------

#Multivariado

num_filas <- nrow(datos_filtrados)
indices_filas <- sample(1:num_filas, 475)
datos_aleatorios <- datos %>% slice(indices_filas)

datos_numeric <- datos_aleatorios[,14:16]
matriz_datos <- as.matrix(datos_numeric)
shapito_multi <- mshapiro.test(t(matriz_datos))

#Univariado

shapiro_univ <- shapiro.test(datos_aleatorios[,14])






