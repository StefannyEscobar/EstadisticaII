library(dplyr)
library(janitor)
library(pastecs)
library(mvnormtest)
library(caret)
library(aplpack)

datos <- read.csv("C:/Users/ASUS/OneDrive - Universidad EAFIT/2023-1/Estadística_II/data(1).csv")
datos


# Filtrar datos por fecha -------------------------------------------------

datos$fecha_corte <- as.Date(datos$fecha_corte, format="%m/%d/%Y")
datos_filtrados = filter(datos,fecha_corte == "2023-01-01")


# Boxplot -----------------------------------------------------------------

boxplot(datos_filtrados$rentabilidad_semestral)


# Bag-plot ----------------------------------------------------------------


bagplot(datos_filtrados$rentabilidad_semestral, datos_filtrados$rentabilidad_anual, xlim=c(0, max(datos_filtrados$rentabilidad_semestral)*1.1), 
        ylim=c(0, max(datos_filtrados$rentabilidad_anual)*1.1))
