### Regresion Multivariante

#Arreglamos los datos:
library(dplyr)
library(readxl)
datos <- read_excel("datos/BD_Indice.xlsx")
View(datos)
datos$year<- NULL

TS<- ts(data=datos, start = 2009, end = 2019, frequency = 12)
plot (TS)

#cálculo del coeficiente de correlación 

round(cor(x = datos, method = "pearson"), 3)

#Graficamos 
library(psych)
multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"), main = "")

library(GGally)
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


# Modelo de regresion multiple.

modelo <-lm(datos$`indice de cambio en el area` ~ datos$HUM_BOS + datos$TEM_BOS + datos$TEM_CAL + datos$TEM_PNN + datos$PRE_SECA, data = datos)
summary(modelo)

library(ggplot2)
library(gridExtra)

#Todas las variables
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

modelo_1<-lm(formula = datos$`indice de cambio en el area` ~ datos$TEM_PNN, data = datos)

# intervalo de confianza para cada uno de los coeficientes parciales

confint(lm(formula=datos$`indice de cambio en el area` ~ datos$TEM_PNN, data = datos))

#Validación de condiciones para la regresión múltiple lineal


#La variable significativa en el modelo

ggplot(data = datos, aes(TEM_PNN, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

#Normalidad
qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

# Homocedasticidad

ggplot(data = datos, aes(modelo$fitted.values, modelo$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()

library(lmtest)
bptest(modelo)

#Multicolinealidad
library(corrplot)

    #todo el modelo

corrplot(cor(dplyr::select(datos, HUM_BOS, TEM_CAL, TEM_BOS, TEM_PNN, PRE_SECA)),
         method = "number", tl.col = "black")

    #Variable significativa
corrplot(cor(dplyr::select(datos, indice de cambio en el area, TEM_PNN)),
         method = "number", tl.col = "black")

#Análisis de Inflación de Varianza (VIF)
library(car)
vif(modelo)


#Autocorrelación

dwt(modelo, alternative = "two.sided")


#Identificación de posibles valores atípicos o influyentes

library(dplyr)
      # Todo el modelo
datos$studentized_residual <- rstudent(modelo)
ggplot(data = datos, aes(x = predict(modelo), y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
  geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
  scale_color_identity() +
  labs(title = "Distribución de los residuos studentized",
       x = "predicción modelo") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

which(abs(datos$studentized_residual) > 3)

    # una variable
datos$studentized_residual_1 <- rstudent(modelo_1)
ggplot(data = datos, aes(x = predict(modelo_1), y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
  geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
  scale_color_identity() +
  labs(title = "Distribución de los residuos studentized",
       x = "predicción modelo") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

which(abs(datos$studentized_residual_1) > 3)

summary(influence.measures(modelo))
summary(influence.measures(modelo_1))

#gráfica de las influencias

influencePlot(modelo)
influencePlot(modelo_1)

#Prediccion

prediccion <- predict.lm(modelo)
plot(prediccion)

library(bigmemory)


### Como un modelo de espacio de estados ###

datos1 <- read_excel("datos/BD_Indice.xlsx")
datos1$year<- NULL
datos1


modreg<-dlmModReg(datos, addInt = TRUE, dV=sd(datos$`indice de cambio en el area`))
summary(modreg)
class(modreg)
head (modreg)

modregFilt<-dlmFilter(datos, modreg)
summary(modregFilt)
head (modregFilt)

modregsmooth<-dlmSmooth(datos, modreg)
modregFilt$f

plot(modregFilt$f)

plot(modregsmooth$s)

predict(modregsmooth)

#Modelo ARMA

#COmpleto

library(readxl)
BD_Indice <- read_excel("datos/BD_Indice.xlsx")
View(BD_Indice)

tiempo<-as.Date(BD_Indice$year)
indice<-BD_Indice$`indice de cambio en el area`

plot(tiempo,indice, type="p", xlab= "Año", ylab="Índice de cambios en el área", main="Serie temporal", col="blue")

BD_Indice$year<-NULL
T.s<-ts(as.matrix(BD_Indice, 132, 6), start = 2009, end = 2019, frequency = 12)
class(T.s)
head(T.s) # as "matrix"
plot(T.s)
plot(T.s, plot.type = "single", lty = 1:3)

## A phase plot:
plot(nhtemp, lag(nhtemp, 1), cex = .8, col = "blue",
     main = "Lag plot of New Haven temperatures")


#Tomar base de datos completa y especificar la variable "respuesta" en este caso el indice

modARMA<- dlmModARMA(ar = as.matrix(T.s), ma = as.matrix(indice), sigma2 =sd(indice))
summary(modARMA)
is.dlm(modARMA)                  
modARMA$V
modARMA$W

w<- as.data.frame(modARMA$w[1:133])

plot(modARMA$W)

head(modARMA)

str(modARMA)

modFilt<- dlmFilter(T.s, modARMA)

modFlit
summary(modFilt)
plot(modFilt$f)

library(devtools)
install_github("bwlewis/irlba")

# Variable significativa
ts_TEM_PNN <-ts(data=datos$TEM_PNN, start = 2009, end = 2019, frequency = 12)
plot (ts_TEM_PNN)
ts_Indice<-ts(data=datos$`indice de cambio en el area`, start = 2009, end = 2019, frequency = 12)
plot(ts_Indice)

modARMA2<- dlmModARMA(ar= as.matrix(ts_TEM_PNN), ma= as.matrix(ts_Indice), sigma2 = diag(2))

summary(modARMA2)
is.dlm(modARMA2)

modARMA2$V
modARMA2$W

str(modARMA2,1)

modFilt2<-dlmFilter(datos,modARMA2)
modFlit2
summary(modFilt2)

plot(ts_TEM_PNN, main= "Serie filtrada")
lines(dropFirst(modFilt2$m), col= "red")

plot(modARMA2$W)

plot(modFilt2$f)


####NOTAS:

#En el modelo pricipal:
#V para las observaciones 
#W varianza para los estados 


#En el modelo filtrado: 
#y : Es la serie de datos con todas sus observaciones
#$ mod:
#$ m : es la media de la distribución de los valores filtrados del vector de estados, 
#U.C y D.C  contienen la descomposición en valores singulares de la varianza de los errores de filtrado UC es una lista de matrices y DC es una matriz
#$ a : es el vector que incluye las medias de las estimaciones del modelo
#U.R y D.R contienen descomposición de la matriz de errores de predicción y pueden ser útiles para reconstruir varianzas
#$ f : contiene la serie con la predicción un paso adelante de nuestro modelo.


