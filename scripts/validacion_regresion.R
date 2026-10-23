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
library(ppcor)
library(ggrepel)



# En primer lugar se representan las dos variables mediante un diagrama de dispersión para intuir si existe relación lineal o monotónica. Si no la hay, no tiene sentido calcular este tipo de correlaciones.

ggplot(data = datos, aes(x = indice, y = TEM_PNN)) + 
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Test de correlacion entre las dos variables del modelo

cor.test(x =datos$TEM_PNN, y = datos$indice, method="pearson")

#MODELO DE REGRESION

modelo_1<-lm(formula = datos$indice~ datos$TEM_PNN, data = datos)

#Representacion grafica del modelo
ggplot(data = datos, mapping = aes(x = TEM_PNN, y = indice)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Indice ~ TEM_PNN', x  =  'TEM_PNN') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# intervalo de confianza para cada uno de los coeficientes parciales

confint(lm(formula=datos$indice ~ datos$TEM_PNN, data = datos))
ggplot(data = datos, mapping = aes(x = TEM_PNN, y = indice)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Diagrama de dispersión', x  =  'TEM_PNN') +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Validación de condiciones para la regresión lineal

#Analisis de los residuos

datos$prediccion <- modelo_1$fitted.values
datos$residuos   <- modelo_1$residuals

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


#Normalidad

ggplot(data = datos, aes(x = residuos)) +
  geom_histogram(aes(y = ..density..)) +
  labs(title = "histograma de los residuos") +
  theme_light()

qqnorm(modelo_1$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

# Homocedasticidad

ggplot(data = datos, aes(modelo_1$fitted.values, modelo_1$residuals)) +
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

bptest(modelo)

#Multicolinealidad

corrplot(cor(dplyr::select(datos, TEM_PNN, indice)),
         method = "number", tl.col = "black")

# Autocorrelación de residuos

#Autocorrelación

dwt(modelo_1, alternative = "two.sided")

ggplot(data = datos, aes(x = seq_along(residuos), y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_line(size = 0.3) +
  labs(title = "Distribución de los residuos", x = "index", y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


lm(formula = indice ~ TEM_PNN, data = datos)$coefficients
