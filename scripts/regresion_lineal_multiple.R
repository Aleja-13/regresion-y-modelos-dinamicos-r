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

#Validación de condiciones para la regresión lineal

#La variable significativa en el modelo

ggplot(data = datos, aes(TEM_PNN, modelo_1$residuals)) +
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