Modelos de espacio de estados multivariados

library(tsm)
library(dlm)
library(readxl)
BD_Indice <- read_excel("datos/BD_Indice.xlsx")
View(BD_Indice)

#Para eliminar la variable año de nuestra base de datos:
BD_Indice$year <- NULL

# Creamos la serie de tiempo basado en los datos. 

TS<-ts(BD_Indice, start = 2009, end = 2019, frequency = 12)
plot.ts(TS)

#Construimos el modelo
fn <- function(parm) {
  dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2]))
}
fit <- dlmMLE(TS, rep(0, 2), build = fn, hessian = TRUE)
(conv <- fit$convergence)

#la función de verosimilitud 
loglik <- dlmLL(TS, dlmModPoly(1))
n.coef <- 2
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef))  #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(TS))) * (n.coef)


#matriz de varianza-covarianza
mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- W(mod)


#filtro Kalman

filtered <- dlmFilter(TS, mod = mod)
smoothed <- dlmSmooth(filtered)

resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s)
mu.1 <- mu[1]
mu.end <- mu[length(mu)]

# Graficar los resultados
par(mfrow = c(2, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(TS, col = "darkgrey", xlab = "", ylab = "", lwd = 1.5)
lines(mu, col = "black")
legend("topright", legend = c("Observed Deflator", "Stochastic level"), 
       lwd = c(2, 1), col = c("darkgrey", "black"), bty = "n")

plot.ts(resids, ylab = "", xlab = "", col = "darkgrey", lwd = 1.5)
lines(h = 0)
legend("topright", legend = "Residuals", lwd = 1.5, col = "darkgrey", bty = "n")


par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
alpha <- mu + gammas
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(BD_Indice, col = "darkgrey", xlab = "", ylab = "", lwd = 2)
lines(alpha, col = "black")
legend("topright", legend = c("Observed Deflator", "State Components"), 
       lwd = c(2, 1), col = c("darkgrey", "black"), bty = "n")


# Resultados de los criterios de información, así como la varianza de los respectivos errores
cat("AIC", r.aic)
cat("BIC", r.bic)
cat("V.variance", obs.error.var)
cat("W.variance", state.error.var)

# Diagnósticos de los residuos
ac(resids)

Box.test(resids, lag = 12, type = "Ljung", fitdf = 2)

shapiro.test(as.matrix(resids))
             
#Pronostico
conf.tmp <- unlist(dlmSvd2var(smoothed$U.S, smoothed$D.S))
conf <- ts(conf.tmp[-1], start = c(1960, 2), frequency = 4)
wid <- qnorm(0.05, lower = FALSE) * sqrt(conf)

conf.pos <- mu + wid
conf.neg <- mu - wid

comb.state <- cbind(mu, conf.pos, conf.neg)

forecast <- dlmForecast(filtered, nAhead = 12)
var.2 <- unlist(forecast$Q)
wid.2 <- qnorm(0.05, lower = FALSE) * sqrt(var.2)
comb.fore <- cbind(forecast$f, forecast$f + wid.2, forecast$f - 
                     wid.2)

result <- ts(rbind(comb.state, comb.fore), start = c(1960, 
                                                     2), frequency = 4)


#Graficando
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(result, col = c("black", "red", "red"), plot.type = "single", 
        xlab = "", ylab = "", lty = c(1, 2, 2), ylim = c(-2, 
                                                         8))
lines(inf, col = "darkgrey", lwd = 1.5)
abline(v = c(2014, 1), col = "blue", lwd = 1, lty = 3)
legend("topleft", legend = c("Observed Deflator", "Stochastic level"), 
       lwd = c(1.5, 1), col = c("darkgrey", "black"), bty = "n")

### MULTIVARIANTE: 

BD_Indice <- read_excel("datos/BD_Indice.xlsx")
View(BD_Indice)

#Para eliminar la variable año de nuestra base de datos:
BD_Indice$year <- NULL

# Creamos la serie de tiempo basado en los datos. 

TS<-ts(BD_Indice, start = 2009, end = 2019, frequency = 12)
plot.ts(TS)


#Construimos el modelo que no tenga restricciones en los términos de covarianza 

build1 <- function(par) {
  m <- dlmModPoly(2)
  m$FF <- m$FF %x% diag(2)
  m$GG <- m$GG %x% diag(2)
  W1 <- diag(1e-07, nr = 2)
  W2 <- diag(exp(par[1:2]))
  W2[1, 2] <- W2[2, 1] <- tanh(par[3]) * prod(exp(0.5 * 
                                                    par[1:2]))
  m$W <- bdiag(W1, W2)
  V <- diag(exp(par[4:5]))
  V[1, 2] <- V[2, 1] <- tanh(par[6]) * prod(exp(0.5 * 
                                                  par[4:5]))
  m$V <- V
  m$m0 <- rep(0, 4)
  m$C0 <- diag(4) * 1e+07
  return(m)
}

# Para estimar los parámetros y considerar el ajuste del modelo
mle <- vector("list", 2)
init <- cbind(as.matrix(expand.grid(rep(list(c(-3, 3)), 
                                        4))), matrix(0, nc = 2, nr = 16))
init <- init[, c(1, 2, 5, 3, 4, 6)]
dimnames(init) <- NULL
for (i in 1:length(mle)) {
  mle[[i]] <- try(dlmMLE(TS, parm = init[i, ], build = build1, 
                         control = list(trace = 3, REPORT = 2)))
}

good <- seq(along = mle)[sapply(mle, function(x) length(x) > 
                                  1 && x$conv == 0)]
mleX <- sapply(mle[good], function(x) unlist(x[1:2]))
mleX <- mleX[, order(mleX["value", ])]

fit1 <- build1(mleX[-NROW(mleX), 1])
loglik1 <- dlmLL(TS, fit1)
aic1 <- (2 * (loglik1)) + 2 * (sum(8))


build2 <- function(par) {
  m <- dlmModPoly(2)
  m$FF <- m$FF %x% diag(2)
  m$GG <- m$GG %x% diag(2)
  W1 <- diag(1e-07, nr = 2)
  W2 <- diag(exp(par[1:2]))
  # W2[1,2] <- W2[2,1] <- tanh(par[3]) * prod(exp(0.5 *
  # par[1 : 2]))
  m$W <- bdiag(W1, W2)
  V <- diag(exp(par[3:4]))
  V[1, 2] <- V[2, 1] <- tanh(par[5]) * prod(exp(0.5 * 
                                                  par[3:4]))
  m$V <- V
  m$m0 <- rep(0, 4)
  m$C0 <- diag(4) * 1e+07
  return(m)
}

mle <- vector("list", 2)
init <- cbind(as.matrix(expand.grid(rep(list(c(-3, 3)), 
                                        4))), matrix(0, nc = 2, nr = 16))
init <- init[, c(1, 2, 4, 3, 5)]
dimnames(init) <- NULL
for (i in 1:length(mle)) {
  mle[[i]] <- try(dlmMLE(TS, parm = init[i, ], build = build2, 
                         control = list(trace = 3, REPORT = 2)))
}

good <- seq(along = mle)[sapply(mle, function(x) length(x) > 
                                  1 && x$conv == 0)]
mleX <- sapply(mle[good], function(x) unlist(x[1:2]))
mleX <- mleX[, order(mleX["value", ])]

fit2 <- build2(mleX[-NROW(mleX), 1])
loglik2 <- dlmLL(TS, fit2)
aic2 <- (2 * (loglik2)) + 2 * (sum(6))


#Para comparar los criterios de información, podemos mirar los criterios de información:
  
cat("Model 1 - AIC: ", aic1)

cat("Model 2 - AIC: ", aic2)

##### el primer modelo está más cerca del infinito negativo, proporciona un mejor ajuste de los datos.

# Suavizado

modelSmooth <- dlmSmooth(BD_Indice, mod = fit1)
Smooth.level.1 <- dropFirst(modelSmooth$s[, 1])
Smooth.level.2 <- dropFirst(modelSmooth$s[, 2])
Smooth.level.3 <- dropFirst(modelSmooth$s[, 3])
Smooth.level.4 <- dropFirst(modelSmooth$s[, 4])

#Graficar

fig.1 <- ts.union(window(BD_Indice[, 1]), window(Smooth.level.1))

fig.2 <- ts.union(window(BD_Indice[, 2]), window(Smooth.level.2))

par(mfrow = c(2, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot(fig.1, plot.type = "single", col = c("blue", "green"), 
     ylab = "")
legend("topright", legend = c("Inflation", "Smoothed"), 
       lty = 1, col = c("blue", "green"), bty = "n")
plot(fig.2, plot.type = "single", col = c("blue", "green"), 
     ylab = "")
legend("bottomright", legend = c("Ouput", "Smoothed"), lty = 1, 
       col = c("blue", "green"), bty = "n")




### Modelo de Regresion

ModReg<-dlmModReg(TS, addInt=TRUE)
ModRegFilter<-dlmFilter(BD_Indice,ModReg)

myMod <- dlmModReg(BD_Indice, addInt=TRUE)
mymodfilt<-dlmFilter(BD_Indice,myMod)

plot(TS, main= "Serie filtrada")
lines(dropFirst(mymodfilt$m), col= "red")

