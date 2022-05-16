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
library(graphics)
library(magrittr)
library(readr)
library(lmtest)
eggs <- read_delim("eggs.csv", delim = ";", 
                   escape_double = FALSE,
                   trim_ws = TRUE)
#ancho con p valor menor a 0.05, no es normal
shapiro.test(eggs$ancho)
boxplot(eggs$ancho)
summary(eggs$ancho)
shapiro.test(eggs$largo)
boxplot(eggs$largo)
summary(eggs$largo)
#creo un data.frame sin el dato atipico

eggs_a <- eggs
eggs_a$largo[eggs_a$largo>56]  <- NA
eggs_a <- na.omit(eggs_a)

#p valor 0.002116 menor a 0.05 
cor.test(eggs$peso, eggs$largo)

cor.test(eggs$peso, eggs$ancho)

plot(eggs$largo, eggs$peso, pch = 20, xlab = "Largo en mm", ylab = "Peso en gr", 
     main = "Diagrama de dispersion", cex.main = 0.95)

plot(eggs$ancho, eggs$peso, pch = 20, xlab = "Ancho en mm", ylab = "Peso en gr", 
     main = "Diagrama de dispersion", cex.main = 0.95)



#lm peso vs largo
modelo_la_a <-lm(peso  ~ largo, data = eggs_a) #con dato atipico
modelo_la <-lm(peso  ~ largo, data = eggs) #sin dato atipico


eggs$fitted.modelo_la <- fitted(modelo_la)
eggs$residuals.modelo_la <- residuals(modelo_la)
eggs$rstudent.modelo_la <- rstudent(modelo_la)
#p valor mayor a 0.05, errores normales para largo
shapiro.test(eggs$rstudent.modelo_la)


#con dato atipico a los datos
plot(eggs$largo, eggs$peso, pch = 20, xlab = "Largo en mm", ylab = "Peso en gr")
abline(modelo_la)
abline(modelo_la_a, col = "red")
legend("topleft",
       legend = c("Con dato atípico","Sin dato atípico"),
       lty = c(1, 1),
       col = c("black", "red"),
       cex = 0.9)



#Como se puede ver en el modelo la diferencia entre los modelos no es 
#significativa por lo que se puede dejar el dato atípico sin que influya
#mayormente en el análisis de los datos

library(car)
outlierTest(modelo_la, cutoff = 0.05, n.max = 10, order = TRUE)
influencePlot(modelo_la)
cook <- cooks.distance(modelo_la)
labels <- rownames(eggs)
shapiro.test(eggs$rstudent.modelo_la)
#Datos "atipicos" al modelo de regresion
#Ninguno es influyente, mas solamente tenemos un dato atipico al modelo de regresion
#Entonces se continua con todos los datos 


#p valor mayor a 0.05, no se pueden sacar las mismas conclusiones
summary(modelo_la)$coefficients

#contiene el 0, no se pueden sacar las mismas conclusiones

confint(modelo_la, level = 0.95)
#SSM < SSR, F > 1, pvalor mayor a 0.05, no se pueden sacar las mismas conclusiones
anova(modelo_la)

eggs$fitted.modelo_la <- fitted(modelo_la)
eggs$residuals.modelo_la <- residuals(modelo_la)
eggs$rstudent.modelo_la <- rstudent(modelo_la)
#p valor mayor a 0.05, errores normales para largo
shapiro.test(eggs$rstudent.modelo_la)



plot(eggs$residuals.modelo_la, pch = 20, ylab = "Residuos", xlab = "Indices")
abline(h = cor(eggs$peso, eggs$largo))
#
dwtest(peso ~ largo, alternative = "two.sided", data = eggs)

modelo_la %>% myQQnorm()

#Existe homogeneidad pues la significación es mayor de 0.05,
#la varianza es constante a lo largo de la muestra.
bptest(modelo_la)

x0 <- seq(min(eggs$largo), max(eggs$largo), length = 15)
dfp <- data.frame(largo = x0)
pred.ip <- predict(modelo_la, dfp, interval = "prediction", se.fit = TRUE, data = eggs)
head(pred.ip$fit)

matplot(x0, pred.ip$fit, type = "l", xlab = "Largo", ylab = "Peso")

pred.ic <- predict(modelo_la, dfp, interval = "confidence", se.fit = TRUE, data = eggs)
head(pred.ic$fit)

library(graphics)
matplot(x0, pred.ic$fit, type = "l", xlab = "Largo", ylab = "Peso")

plot(eggs$largo, 
     eggs$peso, 
     pch = 20, ylim = range(eggs$largo, pred.ip, na.rm = TRUE),
     xlab = "Largo", ylab = "Peso")
matlines(dfp$largo, pred.ic$fit, lty = c(1, 2, 2), lwd = 1.5, col = 1)
matlines(dfp$largo, pred.ip$fit, lty = c(1, 3, 3), lwd = 1.5, col = 1)



#------------------------------------------ANCHO-------------------------------------------------#
#lm peso vs ancho
modelo_an <-lm(peso  ~ ancho, data = eggs)
plot(eggs$ancho, eggs$peso, pch = 20, xlab = "Ancho en mm", ylab = "Peso en gr")
abline(modelo_an)
modelo_an %>% myQQnorm()
#p valores menores a 0.05, si se pueden sacar las mismas conclusiones
summary(modelo_an)$coefficients
#no contiene el 0, se pueden sacar las mismas conclusiones de rechazar la hipotesis nula  
confint(modelo_an, level = 0.95)

#SSM > SSR, F mayor a 1, p valor menos a 0.05, se pueden sacar las mismas conclusiones
anova(modelo_an)


eggs$fitted.modelo_an <- fitted(modelo_an)
eggs$residuals.modelo_an <- residuals(modelo_an)
eggs$rstudent.modelo_an <- rstudent(modelo_an)
#p valor mayor a 0.05, errores normales para ancho

shapiro.test(eggs$rstudent.modelo_an)
#esto se podria omitir?
qqnorm(eggs$rstudent.modelo_an, main = "Normal(0,1)")
qqline(eggs$rstudent.modelo_an)
#p valor menor a 0.05, varianza no constante
bptest(modelo_an)

plot(eggs$residuals.modelo_an, pch = 20, ylab = "Residuos", xlab = "?ndices")
abline(h = cor(eggs$peso, eggs$ancho))

dwtest(peso ~ ancho, alternative = "two.sided", data = eggs)

par(mfrow = c(1, 1))
qqnorm(eggs$rstudent.modelo_an, main = "normal(0,1)")
qqline(eggs$rstudent.modelo_an)
plot(eggs$rstudent.modelo_an, pch = 20, ylab = "Residuaos", xlab = "?ndices")

