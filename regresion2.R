library(magrittr)
library(readr)
library(psych)
library(knitr)

estaturas <- read_csv("estaturas.csv")
kable(rbind(head(estaturas, n = 5),
            rep(".", ncol(estaturas)), rep(".", ncol(estaturas)),
            rep(".", ncol(estaturas)),
            tail(estaturas, n = 5)),digits = 30, align = "c")

str(estaturas)
estaturas$genero %<>% as.factor()
estaturas$exp_sujeto %<>% as.Date()
estaturas$exp_madre %<>% as.Date() 
estaturas$exp_padre %<>% as.Date() 

estaturas$ced_sujeto %<>% as.character()
estaturas$ced_madre %<>% as.character()
estaturas$ced_padre %<>% as.character()
esta <-  data.frame(estaturas$est_madre,estaturas$est_padre,estaturas$est_sujeto)
pairs(esta, panel = panel.smooth)

cor(esta, use = "everything", method = "pearson")
corr.test(esta, use = "complete", method = "pearson")

modelo1 <- lm(data = estaturas, est_sujeto ~  genero+est_madre+est_padre)
summary(modelo1)
#El error típico residual es 11.2, La R^2 es 0.3025, aunque para el modelo múltiple es mejor fijarnos en su valor ajustado
#R2a = 0.222, . Esto que significa que la recta de regresión explica el 22% de la variabilidad del modelo. 
#ademas


#comparacion de modelos
modelo2 <- lm(data = estaturas, est_sujeto ~ est_madre )
summary(modelo2)


modelo3 <- lm(data = estaturas, est_sujeto ~ est_padre)
summary(modelo3)

modelo4 <- lm(data = estaturas, est_sujeto ~ genero )
summary(modelo4)


anova(modelo1,modelo2)
anova(modelo1,modelo3)
