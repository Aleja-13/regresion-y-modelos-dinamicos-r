

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

datos <- read_excel("datos/BD_Indice.xlsx")
View(datos)

# Mostramos las series de tiempo:

tiempo<-(as.Date(datos$year))
indice<-datos$`indice de cambio en el area`

ggplot() + geom_line(data = datos, aes(x = tiempo, y = indice), color = 4, lwd = 1, linetype = 1) + ylab('Cambio en el Indice')+ geom_point(data = datos, aes(x = tiempo, y = indice))

datos$year<-NULL

TS<- ts(data=bd, start = 2009, end = 2019, frequency = 12)
plot (TS)



#cálculo del coeficiente de correlación 

round(cor(x = TS, method = "pearson"), 3)

#Graficamos 

multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"), main = "")

ggpairs(datos, lower = list(continuous = "smooth"), diag = list(continuous = "barDiag"), axisLabels = "none")

# Modelo de regresion lineal multiple.

modelo <-lm(datos$indice ~ datos$HUM_BOS + datos$TEM_BOS + datos$TEM_CAL + datos$TEM_PNN + datos$PRE_SECA, data = datos)
summary(modelo)

#Graficamos
plot1 <- ggplot(data = datos, aes(HUM_BOS, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos, aes(TEM_BOS, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos, aes(TEM_PNN, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos, aes(TEM_CAL, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot5 <- ggplot(data = datos, aes(PRE_SECA, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4, plot5)

#Selección de los mejores predictores

step(object = modelo, direction = "both", trace = 1)

modelo_1<-lm(formula = datos$indice~ datos$TEM_PNN, data = datos)

# intervalo de confianza para cada uno de los coeficientes parciales

confint(lm(formula=datos$indice ~ datos$TEM_PNN, data = datos))

#Validación de condiciones para la regresión múltiple lineal

#La variable significativa en el modelo

ggplot(data = datos, aes(TEM_PNN, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

#Normalidad
qqnorm(modelo_1$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

# Homocedasticidad

ggplot(data = datos, aes(modelo$fitted.values, modelo$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()


bptest(modelo)

#Multicolinealidad


corrplot(cor(dplyr::select(datos, TEM_PNN, indice)),
         method = "number", tl.col = "black")


#Análisis de Inflación de Varianza (VIF) ###Nota: No se realiza para el modelo_1 porque solo tiene una variable

vif(modelo)


#Autocorrelación

dwt(modelo_1, alternative = "two.sided")


#Identificación de posibles valores atípicos o influyentes

datos$studentized_residual_1 <- rstudent(modelo_1)
ggplot(data = datos, aes(x = predict(modelo_1), y = abs(studentized_residual_1))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
  geom_point(aes(color = ifelse(abs(studentized_residual_1) > 3, 'red', 'black'))) +
  scale_color_identity() +
  labs(title = "Distribución de los residuos studentized",
       x = "predicción modelo") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

which(abs(datos$studentized_residual_1) > 3)

summary(influence.measures(modelo_1))

#gráfica de las influencias

influencePlot(modelo_1)

#Prediccion

prediccion <- predict.lm(modelo_1)
plot(prediccion)

pred <- forecast(modelo_1, newdata= as.data.frame(datos$studentized_residual_1), h=12)
summary (pred)
plot(pred)


### Como un modelo de espacio de estados ###

dat_temp <- read_excel("datos/BD_Indice.xlsx")
View(dat_temp)
dat_temp$year<- NULL

#Serie de tiempo
TEM_PNN_TS <- ts(dat_temp$TEM_PNN, start = c(2009, 1), end = c(2019, 12), frequency = 12)
indice_TS <- ts(dat_temp$indice , c(2009, 1), end = c(2019, 12), frequency = 12)

ts_unida<-ts.union(TEM_PNN_TS, indice_TS)

plot(ts.union(TEM_PNN_TS, indice_TS), plot.type = "single")

pred_1 <- forecast(ts_unida, h=30)
plot(pred_1)

#Construimos el modelo

fn <- function(parm) {
  mod <- dlmModPoly(order = 1) + dlmModReg(indice_TS, addInt = FALSE)
  V(mod) <- exp(parm[1])
  diag(W(mod))[1:2] <- exp(parm[2:3])
  return(mod)
}

fit <- dlmMLE(TEM_PNN_TS, rep(0,3), build = fn, hessian = TRUE)

conv <- fit$convergence

loglik <- dlmLL(TEM_PNN_TS, dlmModPoly(1) + dlmModReg(indice_TS, addInt = FALSE))

n.coef <- 3
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef))
r.bic <- (2 * (loglik)) + (log(length(TEM_PNN_TS))) * (n.coef)

mod1 <- fn(fit$par)
obs.error.var <- V(mod1)
state.error.var <- diag(W(mod1))

filtered <- dlmFilter(TEM_PNN_TS, mod = mod1)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s[, 1])
betas <- dropFirst(smoothed$s[, 2])
mu.1 <- mu[1]
mu.end <- mu[length(mu)]
beta.1 <- betas[1]
beta.end <- betas[length(mu)]

#Graficamos los resultados:

par(mfrow = c(3, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(ts.union(TEM_PNN_TS, mu), col = c("darkgrey", "red"), plot.type = "single", xlab = "", ylab = "", lwd = c(1.5, 1))
legend("right", legend = c("Observed Deflator", "Stochastic level"), lwd = c(2, 1), col = c("darkgrey", "red"), bty = "n")

plot.ts(betas, col = "darkgrey", xlab = "", ylab = "", lwd = 1.5)
legend("topright", legend = "Coefficient", lwd = 1.5, col = "darkgrey", bty = "n")

plot.ts(resids, ylab = "", xlab = "", col = "darkgrey", lwd = 1.5)
lines (h = 0)
legend("topright", legend = "Residuals", lwd = 1.5, col = "darkgrey", bty = "n")

#Predicciones

modFilt = dlmFilter(dat_temp,mod1) 
var.2 <- unlist(modFilt$Q)
wid.2 <- qnorm(0.05, lower = FALSE) * sqrt(var.2)
comb.fore <- cbind(modFilt$f, modFilt$f + wid.2, modFilt$f - 
                     wid.2)

result <- ts(rbind(dat_temp$TEM_PNN, dat_temp$indice), start = 2009, end =2019, frequency = 12)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(result, col = c("black", "red", "red"), plot.type = "single", 
        xlab = "", ylab = "", lty = c(1, 2, 2), ylim = c(-2, 
                                                         8))
lines(dat_temp, col = "darkgrey", lwd = 1.5)
abline(v = c(2014, 1), col = "blue", lwd = 1, lty = 3)
legend("topleft", legend = c("Observed Deflator", "Stochastic level"), 
       lwd = c(1.5, 1), col = c("darkgrey", "black"), bty = "n")

### Modelo de regresion

datos <- read_excel("datos/BD_Indice.xlsx")
datos$year<- NULL
datos

modreg<-dlmModReg(datos, addInt = TRUE, dV=sd(datos$`indice de cambio en el area`))
summary(modreg)
class(modreg)
head (modreg)

modregFilt<-dlmFilter(datos, modreg)
summary(modregFilt)
head (modregFilt)

predicciones<- forecast(modregFilt, h=12)

modregsmooth<-dlmSmooth(datos, modreg)
modregFilt$f

plot(modregFilt$f)

plot(modregsmooth$s)

PredModelo<-predict(modregsmooth)

pred <- forecast(modFitl, h=30)
summary (pred)
plot(pred)


filtered <- dlmFilter(datos, mod = modregrFilt)
set.seed(123)
Prediccion <- forecast(modregFilt, nAhead = 20, method="svd", sampleNew = FALSE)
var.2 <- unlist(filtered$Q)
wid.2 <- qnorm(0.05, lower = FALSE) * sqrt(var.2)
comb.fore <- cbind(modFilt$f, modFilt$f + wid.2, modFilt$f - 
                     wid.2)