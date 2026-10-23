#Arreglamos los datos:
library(dplyr)
library(readxl)
library(psych)
library(GGally)
library(ggplot2)
library(gridExtra)
install.packages("dlm", repos = "https://cran.rstudio.com/", dependencies = TRUE)
library(forecast)
library(tsm)
library(dlm)
library(readxl)
library(lmtest)
library(corrplot)
library(car)

datos <- read_excel("datos/BD_Indice.xlsx", sheet = "BD")
View(datos)

# Mostramos las series de tiempo:

tiempo<-(as.Date(datos$year))
indice<-datos$indice
TEM_PNN<-datos$TEM_PNN
datos$year<-NULL
datos$TEM_BOS<-NULL
datos$TEM_CAL<-NULL
datos$HUM_BOS<-NULL
datos$PRE_SECA<-NULL

TEM_PNN_TS <- ts(datos$TEM_PNN, start = c(2009, 1), end = c(2019, 12), frequency = 12)
indice_TS <- ts(datos$indice , c(2009, 1), end = c(2019, 12), frequency = 12)
autoplot(diff(indice_TS))

ts_unida<-ts.union(TEM_PNN_TS, diff(indice_TS))
autoplot(ts_unida)

pred_1 <- forecast(ts_unida, h=12)
plot(pred_1)

#Regresion dinamica
#Observamos el comportamiento
autoplot(ts_unida, facets = TRUE)+ xlab("year") + ylab("")+ ggtitle("Cambios en el indice")
ggplot(aes(y=indice, x= TEM_PNN), data=datos)+geom_point()+ggtitle("Cambios en el indice con respento a TEM_PNN")


#Modelo de regresion Dinamica como un modelo ARIMA

fit<-auto.arima(datos[,"indice"], xreg = as.matrix(datos[,"TEM_PNN"]))
fit
checkresiduals(fit)

fcast<-forecast(fit, xreg = rep(0.1, 12))
autoplot(fcast)+ xlab("Year")+ylab("Cambio en el indice")

#Con el dlm package

library(PerformanceAnalytics, quietly = TRUE,  warn.conflicts = FALSE)

# Especificación de parámetros de un modelo determinado
s2_obs = 1     # Variance of observations
s2_alpha = 0.01 # Variance of the alpha regression parameter
s2_beta = 0.01  # Variance of the beta regression parameter

# Construyendo un modelo de regresión
reg.dlm = dlmModReg(X=indice, addInt=TRUE, dV=s2_obs, dW=c(s2_alpha, s2_beta))
reg.dlm[c("FF","V","GG","W","m0","C0")]
reg.dlm[c("JFF","JV","JGG","JW")]

#función para construir el modelo de espacio de estado de parámetros que varían en el tiempo
buildTVP <- function(parm, x.mat){
  parm <- exp(parm)
  return(dlmModReg(X=x.mat, dV=parm[1], dW=c(parm[2], parm[3])) )
}

# Estimar el modelo
start.vals = c(0,0,0)
names(start.vals) = c("lns2_obs", "lns2_alpha", "lns2_beta")
reg.mle = dlmMLE(y=indice, parm=start.vals, x.mat=TEM_PNN, build=buildTVP, hessian=T)

# obtener estimaciones de sd
se2 <- sqrt(exp(reg.mle$par))
names(se2) = c("s_obs", "s_alpha", "s_beta")
sqrt(se2)

# modelo ss ajustado, pasándole TEMM_PNN como la matriz X en el modelo
REG.dlm <- buildTVP(reg.mle$par, TEM_PNN)

#Filtrado
REG.f <- dlmFilter(y = indice, mod = REG.dlm)
class(REG.f)
