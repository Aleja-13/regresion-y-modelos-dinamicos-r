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
#Creamos las series de tiempo para observar su comportamiento
TEM_PNN_TS <- ts(datos$TEM_PNN, start = c(2009, 1), end = c(2019, 12), frequency = 12)
indice_TS <- ts(datos$indice , c(2009, 1), end = c(2019, 12), frequency = 12)
ts_unida<-ts.union(TEM_PNN_TS, indice_TS)
indice_TS_D<-diff(indice_TS)
ts_unida1<-ts.union(TEM_PNN_TS, indice_TS_D)
autoplot(ts_unida1)

autoplot(ts_unida, facets = TRUE)+ xlab("year") + ylab("")+ ggtitle("Cambios en el indice")

ggplot(aes(y=indice, x= TEM_PNN), data=datos)+geom_point()+ggtitle("Cambios en el indice con respecto a TEM_PNN")

#Regresion dinamica

#Modelo de regresion Dinamica como un modelo ARIMA

fit<-auto.arima(datos[,"indice"], xreg = as.matrix(datos[,"TEM_PNN"]))
fit
checkresiduals(fit)

fcast<-forecast(fit, xreg = rep(0.8, 12))

autoplot(fcast)+ xlab("Year")+ylab("Cambio en el indice para el proximo año")


class(fit)

#Con el dlm package

library(PerformanceAnalytics, quietly = TRUE,  warn.conflicts = FALSE)

indice<-datos$indice
TEM_PNN<-datos$TEM_PNN

# Especificación de parámetros de un modelo determinado
s2_obs = 1     # Varianza de las observaciones
s2_alpha = 0.01 # Varianza del parámetro de regresión alfa
s2_beta = 0.01  # Varianza del parámetro de regresión beta

# Construyendo un modelo de regresión
reg.dlm = dlmModReg(X=indice, addInt=TRUE, dV=s2_obs, dW=c(s2_alpha, s2_beta))
#Ahora que hemos definido un modelo, podemos ver su componente diferente
reg.dlm[c("FF","V","GG","W","m0","C0")]
reg.dlm[c("JFF","JV","JGG","JW")]

start.vals = c(0,0,0)
# Nombres en varianza de: observación y, alfa y beta (intersección y pendiente correspondientes de y (Indice) con respecto a X (TEM_PNN))
names(start.vals) = c("Obs", "alfa", "beta")

#función para construir el modelo de espacio de estado de parámetros que varían en el tiempo

buildTVP <- function(parm, x.mat){
  parm <- exp(parm)
  return(dlmModReg(X=x.mat, dV=parm[1], dW=c(parm[2], parm[3])) )
}

# Estimar el modelo
reg.mle = dlmMLE(y=indice, parm=start.vals, x.mat=TEM_PNN, build=buildTVP, hessian=T)
reg.mle
# obtener estimaciones de sd
se2 <- sqrt(exp(reg.mle$par))
names(se2) = c("s_obs", "s_alpha", "s_beta")
sqrt(se2)

# modelo ss ajustado, pasándole TEMM_PNN como la matriz X en el modelo
REG.dlm <- buildTVP(reg.mle$par, TEM_PNN)
REG.dlm

#Filtrado
REG.f <- dlmFilter(y = indice, mod = REG.dlm)
class(REG.f)
names(REG.f)
REG.f

# Estimaciones óptimas de ??_t dada la información disponible en el momento T.
REG.s <- dlmSmooth(REG.f)
class(REG.s)
names(REG.s)
REG.s

# Proyección usando el filtro de Kalman

# Construimos 10 valores faltantes al final de la muestra
new.xts <-xts(rep(NA, 10), seq.Date(from= as.Date(1), by="months", length.out=11)[-1])
Indice.ext <-merge(indice, new.xts)[,1]
REG.ext.f <-dlmFilter(Indice.ext, REG.dlm)
head(REG.ext.f$m)

#Comprobaciones residuales

# NORMALIDAD
qqnorm(IND.res)
qqline(IND.res)

shapiro.test(residuals(REG.f, sd = FALSE))

#COmportamiento de los residuales
datos$prediccion <- REG.f$f
datos$residuos   <-residuals(REG.f, sd = FALSE)

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Homocedasticidad

ggplot(data = datos, aes(datos$prediccion, datos$residuos)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  geom_smooth(se = FALSE, color = "firebrick") +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


#Multicolinealidad

corrplot(cor(dplyr::select(datos, TEM_PNN, indice)),
         method = "number", tl.col = "black")

# Autocorrelación de residuos

#Autocorrelación

dwt(datos$residuos, alternative = "two.sided")

ggplot(data = datos, aes(x = seq_along(residuos), y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_line(size = 0.3) +
  labs(title = "Distribución de los residuos", x = "index", y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


