library(DetMCD)
library(MASS)
library(dplyr)
library(caret)
library(aplpack)
creditcard <- read.csv("C:/Users/Stefanny/OneDrive - Universidad EAFIT/Eafit/Quinto semestre/estadística/proyecto/Entrega2/creditcard.csv")


# Boxplot -----------------------------------------------------------------
dev.new()
V4 = creditcard$V4[1:2800]
boxplot(creditcard$V4, main ="Feature V4")

# Bag-plot ----------------------------------------------------------------

#datos ____________________________________________________________________
dev.new()
V6 = creditcard$V6[1:2800]
V27 =creditcard$V27[1:2800]

#Incluyendo solo algunos outliers _________________________________________
bagplot(V6,V27, xlab="Feature V6",
        ylab="Feature V27", main="Bagplot",xlim=c(-10, 10), ylim=c(-1, 1))


# Determinante de covarianza minima ---------------------------------------


data=matrix(c(creditcard$V1,creditcard$V2, creditcard$V10, creditcard$V26)
            , nrow=2800,ncol=4)

n <- dim(data)[1]
p<-dim(data)[2]
data = scale(data)
detmc= cov.mcd(data, cor = FALSE, quan = floor((n + p + 1)/p))
detmcd <- DetMCD(data)

mahaMV <- mahalanobis(data, center = detmcd$center, cov = detmcd$cov)
mahaMV <- as.data.frame(mahaMV)

out<-cbind(rownames(mahaMV), mahaMV, NA)
colnames(out) <- c("Obsevación", "DCM", "Outlier")

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





