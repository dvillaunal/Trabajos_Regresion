ggplot(eggs,aes(x=1/(diametro), y=1/(peso)))+
  geom_point()


eggs <- read_delim("dataeggs.csv", delim = ";")

eggs <- eggs[-c(3),]

shapiro.test(eggs$diametro)

cor.test(eggs$diametro, eggs$peso)


modelo1 <- lm(peso~diametro, data=datos)
modelo2 <- lm(peso~diametro, data=datos2)
modelo3 <- lm(peso~diametro, data=eggs)

bptest(modelo1)

bptest(modelo2)

ggplot(eggs, aes(x=diametro,y=peso))+geom_point()+
  geom_smooth(se=F, color="blue")
library(lmtest)
bptest(modelo3)

bptest(modelo4)

anova(modelo4)


eggs$fitted.modelo3 <- fitted(modelo3)
eggs$residuals.modelo3 <- residuals(modelo3)
eggs$rstudent.modelo3 <- rstudent(modelo3)

shapiro.test(eggs$rstudent.modelo3)

qqnorm(eggs$rstudent.modelo3, main = "Normal(0,1)")
qqline(eggs$rstudent.modelo3)


plot(eggs$residuals.modelo3, pch = 20, ylab = "Residuos", xlab = "Índices")
abline(h = cor(eggs$peso, eggs$diametro))


dwtest(peso ~ diametro, alternative = "two.sided", data = eggs)


library(car)
outlierTest(modelo3, cutoff = 0.05, order = TRUE)

influencePlot(modelo3, id.n = 2)

cook3 <- cooks.distance(modelo3)
labels3 <- rownames(eggs)

library(faraway)
halfnorm(cook3, 3, labs = labels3, ylab = "Distancia de Cook")

datos <- datos[-c(27),]

eggs <- eggs[-c(3),]

shapiro.test(eggs$diametro)


cor.test(eggs$diametro, eggs$peso)

modelo4 <- lm(peso~diametro, data = eggs)


outlierTest(modelo4, cutoff = 0.05, order = TRUE)

influencePlot(modelo4, id.n = 2)

cook3 <- cooks.distance(modelo4)
labels3 <- rownames(eggs)

halfnorm(cook4, 3, labs = labels3, ylab = "Distancia de Cook")

plot(eggs$diametro, eggs$peso, pch = 20)
abline(modelo3)
abline(modelo4, lty=2)

eggs$fitted.modelo4 <- fitted(modelo4)
eggs$residuals.modelo4 <- residuals(modelo4)
eggs$rstudent.modelo4 <- rstudent(modelo4)

shapiro.test(eggs$rstudent.modelo4)

plot(eggs$residuals.modelo4, pch = 20)
abline(h = cor(eggs$diametro, eggs$peso))


dwtest(peso ~ diametro, alternative = "two.sided", data = eggs)




