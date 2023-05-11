library(DetMCD)
library(MASS)

# 99 observaciones de los datos -------------------------------------------


datos <- read.csv("C:/Users/Stefanny/OneDrive - Universidad EAFIT/Eafit/Quinto semestre/estadística/proyecto/data2.csv")
datos
data=matrix(c(datos$numero_inversionistas, datos$rentabilidad_diaria), nrow=99,ncol=2)
data

detmcd <- DetMCD(log(data))
dev.new()
plot(detmcd, which = "distance", classic = TRUE)# 2 plots
dev.new()
plot(detmcd, which = "dd")
dev.new()
plot(detmcd, which = "tolEllipsePlot", classic = TRUE)
op <- par(mfrow = c(2,3))
plot(detmcd)## -> which = "all" (5 plots)
par(op)


# P>2 ---------------------------------------------------------------------


data=matrix(c(datos$numero_inversionistas, datos$rentabilidad_diaria,datos$aportes_recibidos,
              datos$valor_unidad_operaciones_dia_t), nrow=200,ncol=4)
detmc= cov.mcd(data, cor = FALSE, quan = floor((200 + 4 + 1)/4))



mahaMV <- mahalanobis(data, center = detmc$center, cov = detmc$cov)
mahaMV <- as.data.frame(mahaMV)

out<-cbind(rownames(mahaMV), mahaMV, NA)
colnames(out) <- c("Obsevación", "DCM", "Outlier")

n <- dim(data)[1]
p<-dim(data)[2]
chisq<- qchisq(0.975,p)

for(i in 1:n){
  if(out$DCM[i]>chisq){
    out$Outlier[i]<- "VERDADERO"
  } else{
    out$Outlier[i]<- "FALSO"
  }
}

print(atipicos<-rownames(out)[out$Outlier=="VERDADERO"])

dev.new()

plot(out$DCM,type = "p", col = 2)
abline(h = chisq, lwd = 2, lty = 3, col =4)


# Lo comparo con la Mahalanobis -------------------------------------------
media <- colMeans(data)
cov <- cov(data)

maha <- mahalanobis(data, center = media, cov = cov)
maha <- as.data.frame(maha)

outl<-cbind(rownames(maha), maha, NA)
colnames(outl) <- c("Obsevación", "Mahalanobis", "Outlier")

n <- dim(data)[1]
p<-dim(data)[2]
chisqM<- qchisq(0.975,p)

for(i in 1:n){
  if(outl$Mahalanobis[i]>chisqM){
    outl$Outlier[i]<- "VERDADERO"
  } else{
    outl$Outlier[i]<- "FALSO"
  }
}

print(atipicos<-rownames(outl)[outl$Outlier=="VERDADERO"])

dev.new()

plot(outl$Mahalanobis,type = "p", col = 2)
abline(h = chisqM, lwd = 2, lty = 3, col =4)





