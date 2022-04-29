library(readr)
library(lmtest)
outliersReplace <- function(data, lowLimit, highLimit){
  data[data < lowLimit] <- mean(data)
  data[data > highLimit] <- median(data)
  data     #devolvemos el dato       
}


eggs <- read_delim("eggs.csv", delim = ";", 
                   escape_double = FALSE,
                   trim_ws = TRUE)

par(mfrow = c(1,1))


#ancho con p valor menor a 0.05, no es normal
shapiro.test(eggs$ancho)
boxplot(eggs$ancho)
summary(eggs$ancho)
shapiro.test(eggs$largo)
boxplot(eggs$largo)
#Aqui quito el valor atipico de 58 y se vuelve normal
largos <- outliersReplace(eggs$largo,0,56)
shapiro.test(largos)

eggs$largo[ eggs$largo>56] <- NA
eggs <- na.omit(eggs)




#p valor 0.002116 menor a 0.05 
cor.test(eggs$peso, eggs$largo)

cor.test(eggs$peso, eggs$ancho)

plot(eggs$largo, eggs$peso, pch = 20, xlab = "Largo en mm", ylab = "Peso en gr", 
     main = "Diagrama de dispersi?n", cex.main = 0.95)

plot(eggs$ancho, eggs$peso, pch = 20, xlab = "Ancho en mm", ylab = "Peso en gr", 
     main = "Diagrama de dispersi?n", cex.main = 0.95)



#lm peso vs largo
modelo_la <-lm(peso  ~ largo, data = eggs)
plot(eggs$largo, eggs$peso, pch = 20, xlab = "Largo en mm", ylab = "Peso en gr")
abline(modelo_la)

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


plot(eggs$residuals.modelo_la, pch = 20, ylab = "Residuos", xlab = "?ndices")
abline(h = cor(eggs$peso, eggs$largo))

dwtest(peso ~ largo, alternative = "two.sided", data = eggs)



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

par(mfrow = c(1, 2))
qqnorm(eggs$rstudent.modelo_an, main = "normal(0,1)")
qqline(eggs$rstudent.modelo_an)
plot(eggs$rstudent.modelo_an, pch = 20, ylab = "Residuaos", xlab = "?ndices")


