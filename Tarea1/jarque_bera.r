# Librerias ---------------------------------------------------------------
library(dplyr)
library(stargazer)
library(readr)
library(tseries)
library(fitdistrplus)
library(janitor)
library(pastecs)
library(semTools)
# Data --------------------------------------------------------------------

datos <- read.csv("C:/Users/Stefanny/OneDrive - Universidad EAFIT/Eafit/Quinto semestre/estadística/proyecto/data2.csv")
datos

# Test for one column

jarque.bera.test(datos$rendimientos_abonados)

# Test in normal data

data <- rnorm(70)
jarque.bera.test(data)

#QQPLOT

fit_normal <- fitdist(data =datos$rendimientos_abonados, distr = "norm")
descdist(datos$rendimientos_abonados, discrete=FALSE)
plot(fit_normal)
jarque.bera.test(datos$rendimientos_abonados)


# Multivariada ------------------------------------------------------------

# Tomemos los datos de 
media  <- colMeans(datos)
S  <- cov(datos)
corr  <- cor(datos)

# Implementación ----------------------------------------------------------

#Dado a la singularidad de la matriz  implementamos 2 varibles
N  <- 475 #Cantidad individuos
p <- 2
y  <- replicate(p, 0) #Vector medias


# Matriz ------------------------------------------------------------------

data=matrix(c(datos$numero_inversionistas, datos$rentabilidad_diaria), nrow=475,ncol=2)
data
# Estadístico ------------------------------------------------------------------

#Asimetria y Curtosis
asimtetria  <- mardiaSkew(data)[[1]]
curtosis <- mardiaSkew(data)[[2]]

JB <- N*(asimtetria%*% (1/6)+ ((1/8)*p*(p+2))%*%curtosis-(p*(p+2)^2))
vc=qchisq(0.05,(p*(p+1)*(p+2)/6)+1,lower.tail = FALSE)
vp=pchisq(JB,(p*(p+1)*(p+2)/6)+1,lower.tail = FALSE)

fit_normal <- fitdist(data =datos$numero_inversionistas, distr = "norm")
descdist(datos$numero_inversionistas, discrete=FALSE)
plot(fit_normal)
