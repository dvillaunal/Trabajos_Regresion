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


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------
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

matlines(dfp$diametro, pred.ip$fit, lty = c(1, 3, 3),
         lwd = 1.5, col= "black")

