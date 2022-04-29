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
#esto se podria omitir entonces?
qqnorm(eggs$rstudent.modelo_la, main = "Normal(0,1)")
qqline(eggs$rstudent.modelo_la)
#p valor mayor a 0.05, varianza constante
bptest(modelo_la)


plot(eggs$residuals.modelo_la, pch = 20, ylab = "Residuos", xlab = "Indices")
abline(h = cor(eggs$peso, eggs$largo))
#
dwtest(peso ~ largo, alternative = "two.sided", data = eggs)


library(car)
outlierTest(modelo_la, cutoff = 0.05, n.max = 10, order = TRUE)
influencePlot(modelo_la)
cook <- cooks.distance(modelo_la)
labels <- rownames(eggs)
shapiro.test(eggs$rstudent.modelo_la)
#Datos "atipicos" al modelo de regresion
#Ninguno es influyente, mas solamente tenemos un dato atipico al modelo de regresion
#Entonces se continua con todos los datos 

#lm peso vs ancho
modelo_an <-lm(peso  ~ ancho, data = eggs)
plot(eggs$ancho, eggs$peso, pch = 20, xlab = "Ancho en mm", ylab = "Peso en gr")
abline(modelo_an)

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


