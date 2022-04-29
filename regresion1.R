library(readr)
library(lmtest)

eggs <- read_delim("eggs.csv", delim = ";", 
                   escape_double = FALSE,
                   trim_ws = TRUE)
#P valor < 0.05 no es normal? ¿categoria = largo y ancho?
#solo el largo tiene 1 valor atipico
shapiro.test(eggs$largo)
shapiro.test(eggs$ancho)
#peso si es normal
shapiro.test(eggs$peso)

cor.test(eggs$peso, eggs$largo)
cor.test(eggs$peso, eggs$ancho)

plot(eggs$largo, eggs$peso, pch = 20, xlab = "Largo en mm", ylab = "Peso en gr", 
     main = "Diagrama de dispersión", cex.main = 0.95)

plot(eggs$ancho, eggs$peso, pch = 20, xlab = "Ancho en mm", ylab = "Peso en gr", 
     main = "Diagrama de dispersión", cex.main = 0.95)



#lm peso vs largo
modelo_la <-lm(peso  ~ largo, data = eggs)
plot(eggs$largo, eggs$peso, pch = 20, xlab = "Categoria", ylab = "Ausencias")
abline(modelo_la)

summary(modelo_la)$coefficients


confint(modelo_la, level = 0.95)

anova(modelo_la)

eggs$fitted.modelo_la <- fitted(modelo_la)
eggs$residuals.modelo_la <- residuals(modelo_la)
eggs$rstudent.modelo_la <- rstudent(modelo_la)
#p valor mayor a 0.05, errores normales para largo
shapiro.test(eggs$rstudent.modelo_la)
#esto se podria omitir entonces?
qqnorm(eggs$rstudent.modelo_la, main = "Normal(0,1)")
qqline(eggs$rstudent.modelo_la)

bptest(modelo_la)


plot(eggs$residuals.modelo_la, pch = 20, ylab = "Residuos", xlab = "Índices")
abline(h = cor(eggs$peso, eggs$largo))

dwtest(peso ~ largo, alternative = "two.sided", data = eggs)



#lm peso vs ancho
modelo_an <-lm(peso  ~ ancho, data = eggs)
plot(eggs$ancho, eggs$peso, pch = 20, xlab = "Categoria", ylab = "Ausencias")
abline(modelo_an)

summary(modelo_an)$coefficients

confint(modelo_an, level = 0.95)

anova(modelo_an)



eggs$fitted.modelo_an <- fitted(modelo_an)
eggs$residuals.modelo_an <- residuals(modelo_an)
eggs$rstudent.modelo_an <- rstudent(modelo_an)
#p valor mayor a 0.05, errores normales para ancho
shapiro.test(eggs$rstudent.modelo_an)

qqnorm(eggs$rstudent.modelo_an, main = "Normal(0,1)")
qqline(eggs$rstudent.modelo_an)
#esto se podria omitir
bptest(modelo_an)

plot(eggs$residuals.modelo_an, pch = 20, ylab = "Residuos", xlab = "Índices")
abline(h = cor(eggs$peso, eggs$ancho))

dwtest(peso ~ ancho, alternative = "two.sided", data = eggs)

par(mfrow = c(1, 2))
qqnorm(eggs$rstudent.modelo_an, main = "normal(0,1)")
qqline(eggs$rstudent.modelo_an)
plot(eggs$rstudent.modelo_an, pch = 20, ylab = "Residuaos", xlab = "Índices")


