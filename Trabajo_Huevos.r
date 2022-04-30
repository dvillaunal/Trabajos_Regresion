## ----setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
library(readr)
library(tidyverse)
library(kableExtra)
library(magrittr)
library(ggExtra)
library(GGally)
library(janitor)
library(tidystats)
library(car)
library(faraway)
library(lmtest)
library(graphics)
datos <- read_delim("eggs.csv", delim = ";") %>% clean_names()
names(datos)[3] <- "diametro"

myQQnorm <- function(modelo, student = F, ...){
  if(student){
    res <- rstandard(modelo)
    lab.plot <- "Normal Q-Q Plot of Studentized Residuals"
  } else {
    res <- residuals(modelo)
    lab.plot <- "Normal Q-Q Plot of Residuals"
  }
  shapiro <- shapiro.test(res)
  shapvalue <- ifelse(shapiro$p.value < 0.001, "P value < 0.001", paste("P value = ", round(shapiro$p.value, 4), sep = ""))
  shapstat <- paste("W = ", round(shapiro$statistic, 4), sep = "")
  q <- qqnorm(res, plot.it = FALSE)
  qqnorm(res, main = lab.plot, ...)
  qqline(res, lty = 2, col = 2)
  text(min(q$x, na.rm = TRUE), max(q$y, na.rm = TRUE)*0.95, pos = 4, 'Shapiro-Wilk Test', col = "blue", font = 2)
  text(min(q$x, na.rm = TRUE), max(q$y, na.rm = TRUE)*0.80, pos = 4, shapstat, col = "blue", font = 3)
  text(min(q$x, na.rm = TRUE), max(q$y, na.rm = TRUE)*0.65, pos = 4, shapvalue, col = "blue", font = 3)
}


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
cor.test(datos$diametro, datos$peso)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
plot(datos$diametro, datos$peso, xlab = "diámetro en mm",
     ylab = "Peso en gr", main = "diámetro vs Peso",
     cex.main = 0.95, pch=20)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
modelo1 <- lm(peso~diametro, data=datos)
summary(modelo1)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
plot(datos$diametro, datos$peso, xlab = "diámetro en mm",
     ylab = "Peso en gr", pch=20)
abline(modelo1)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
summary(modelo1)$coefficients


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
MSR.ancho <- mean(summary(modelo1)$residuals^2)


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
confint(modelo1, level = 0.95)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
anova(modelo1)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
plot(fitted(modelo1), residuals(modelo1), xlab = "Ancho",
ylab = "Residuales", main = "Residuales vs. valores ajustados",pch=20)
abline(h = 0, lty = 2, col = 2)


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
datos$fitted.modelo1 <- fitted(modelo1)
datos$residuals.modelo1 <- residuals(modelo1)
datos$rstudent.modelo1 <- rstudent(modelo1)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
modelo1 %>% myQQnorm()


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
bptest(modelo1)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
plot(log(datos$diametro), log(datos$peso), xlab = "diámetro",
     ylab = "Peso", main = "diámetro vs Peso (Escala Log)",
     cex.main = 0.95, pch=20)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
modelo2 <- lm(log(peso)~log(diametro), data=datos)
summary(modelo2)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
plot(log(datos$diametro), log(datos$peso), xlab = "diámetro en mm",
     ylab = "Peso en gr", pch=20)
abline(modelo2)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
summary(modelo2)$coefficients


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
MSR.log.ancho <- mean(summary(modelo2)$residuals^2)


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
confint(modelo2, level = 0.95)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
anova(modelo2)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
plot(fitted(modelo2), residuals(modelo2), xlab = "Ancho",
ylab = "Residuales", main = "Residuales vs. valores ajustados",pch=20)
abline(h = 0, lty = 2, col = 2)


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
datos$fitted.modelo2 <- fitted(modelo2)
datos$residuals.modelo2 <- residuals(modelo2)
datos$rstudent.modelo2 <- rstudent(modelo2)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
modelo2 %>% myQQnorm()


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
bptest(modelo2)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
plot(datos$residuals.modelo1, pch = 20,
     ylab = "Residuos", xlab = "Índices")
abline(h = cor(datos$peso, datos$diametro))


## ----------------------------------------------------------------------
dwtest(peso~diametro, alternative = "two.sided", data = datos)


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
x0 <- seq(min(datos$diametro), max(datos$diametro), length = 15)
dfp <- data.frame(diametro = x0)
pred.ip <- predict(modelo1, dfp, interval = "prediction", se.fit = TRUE, data = datos)
head(pred.ip$fit)


## ----------------------------------------------------------------------
matplot(x0, pred.ip$fit, type = "l", xlab = "diametro", ylab = "peso")


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
x0 <- seq(min(datos$diametro), max(datos$diametro), length = 15)
dfp <- data.frame(diametro = x0)
pred.ip <- predict(modelo2, dfp, interval = "prediction", se.fit = TRUE, data = datos)
pred.ip1 <- predict(modelo1, dfp, interval = "prediction", se.fit = TRUE, data = datos)
head(pred.ip$fit)
newpred <- exp(pred.ip$fit)
head(newpred)


## ----------------------------------------------------------------------
pred.ic <- predict(modelo1, dfp, interval = "confidence", se.fit = TRUE, data = datos)
head(pred.ic$fit)


## ----------------------------------------------------------------------
matplot(x0, pred.ic$fit, type = "l", xlab = "diametro", ylab = "peso")


## ----------------------------------------------------------------------
plot(datos$diametro, datos$peso, pch = 20, xlab = "diametro", ylab = "peso", col="blue")

# Añadimos las bandas
matlines(dfp$diametro, pred.ic$fit, lty = c(1, 2, 2), 
         lwd = 1.5, col = "red")

matlines(dfp$diametro, pred.ip1$fit, lty = c(1, 3, 3),
         lwd = 1.5, col= "black")
title(main= "R.L.S. Peso vs Diámetro (IC's & IP's)")


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
cor.test(datos$largo, datos$peso)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
plot(datos$largo, datos$peso, xlab = "largo en mm",
     ylab = "Peso en gr", main = "largo vs Peso",
     cex.main = 0.95, pch=20)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
modelo3 <- lm(peso~largo, data=datos)
summary(modelo3)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
plot(datos$largo, datos$peso, xlab = "diámetro en mm",
     ylab = "Peso en gr", pch=20)
abline(modelo3)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
summary(modelo3)$coefficients


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
MSR.largo <- mean(summary(modelo3)$residuals^2)


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
confint(modelo3, level = 0.95)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
anova(modelo3)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
plot(fitted(modelo3), residuals(modelo3), xlab = "Largo",
ylab = "Residuales", main = "Residuales vs. valores ajustados",pch=20)
abline(h = 0, lty = 2, col = 2)


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------
datos$fitted.modelo3 <- fitted(modelo3)
datos$residuals.modelo3 <- residuals(modelo3)
datos$rstudent.modelo3 <- rstudent(modelo3)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
modelo3 %>% myQQnorm()


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
bptest(modelo3)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
plot(datos$residuals.modelo3, pch = 20, ylab = "Residuos", xlab = "Índices")
abline(h = cor(datos$peso , datos$largo))


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
dwtest(peso~largo, alternative = "two.sided", data = datos)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
scatterplot(peso~largo,data = datos,smooth = F, pch=19,
            regLine = F, xlab = "Largo", ylab = "Peso")
title(main = "Scatter Plot | Peso vs Largo")


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
outlierTest(modelo3, cutoff = 0.05, n.max = 10, order = TRUE)
influencePlot(modelo3, id.n = 2)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
cook3 <- cooks.distance(modelo3)
labels3 <- rownames(datos)
halfnorm(cook, 3, labs = labels, ylab = "Distancia de Cook")
abline(h=4/30, lty = 2, col = 2)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
x0.l <- seq(min(datos$largo), max(datos$largo), length = 15)
dfp.l <- data.frame(largo = x0.l)
pred.ip.l <- predict(modelo3, dfp.l, interval = "prediction",
                     se.fit =TRUE, data = datos)
head(pred.ip.l$fit)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
matplot(x0.l, pred.ip.l$fit, type = "l", xlab = "largo", ylab = "peso")


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
pred.ic.l <- predict(modelo3, dfp.l, interval = "confidence", se.fit = TRUE, data = datos)
head(pred.ic.l$fit)


## ----------------------------------------------------------------------
matplot(x0.l, pred.ic.l$fit, type = "l", xlab = "largo", ylab = "peso")


## ----------------------------------------------------------------------
plot(datos$largo, datos$peso, pch = 20, xlab = "largo", ylab = "peso", col="blue")

# Añadimos las bandas
matlines(dfp.l$largo, pred.ic.l$fit, lty = c(1, 2, 2), 
         lwd = 1.5, col = "red")

matlines(dfp.l$largo, pred.ip.l$fit, lty = c(1, 3, 3),
         lwd = 1.5, col= "black")

title(main= "R.L.S. Peso vs Largo (IC's & IP's)")

