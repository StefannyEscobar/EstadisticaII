marketing_campaign <- read.delim("C:/Users/Stefanny/Downloads/marketing_campaign.csv")
library(MASS)
library(tseries)
library(tidyverse)
mydatamatrix=matrix(c(marketing_campaign$MntFruits,marketing_campaign$MntMeatProducts, 
                marketing_campaign$MntFishProducts, marketing_campaign$MntGoldProds,
                  marketing_campaign$Income), nrow=2240,ncol=5)

mydata=as.data.frame(mydatamatrix)

# Multiple Linear Regression Example
fit <- lm(V5 ~ V1+V2+V3+V4,  data = mydata[200:500,])
summary(fit) # show results
# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals

# Significancia conjunta --------------------------------------------------


anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics


# R^2 ---------------------------------------------------------------------

summary(fit)$r.squared
summary(fit)$adj.r.squared

jarque.bera.test(fit$residual)
residuos =fit$residual
qqnorm(residuos)
qqline(residuos)


