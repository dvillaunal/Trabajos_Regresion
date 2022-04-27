require(magrittr)
require(tidyverse)
require(readxl)
require(knitr)

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
datos <- read_excel("buebos.xlsx")
#figura 1
kable(rbind(head(datos,n=3),rep(".",ncol(datos)),rep(".",ncol(datos)),
rep(".",ncol(datos)),tail(datos,n=3)),col.names = c("ID","Altura","Ancho","Peso"),
digits = 3,align = 'c')
#figura 2
datos %>% select(-"Huevos") %>% summary() %>% kable(align = 'c')

#figura 3 diametro
boxplot(datos$ancho, main = "Boxplot Diametro", col = "Grey", ylab = "Centimetros")
legend("bottomright",c("Diametro en MM"))
# se presento un dato atipico

#figura 4 altura
boxplot(datos$largo,main="Boxplot Altura",col="pink", ylab = "Centimetros")
legend("bottomright",c("Altura en CM"))

#figura 5 
boxplot(datos$peso,main="Boxplot Peso",col="red", ylab = "Gramos")
legend("bottomright", c("Peso en gramos"))
# se presentaron 2 datos atipicos

#figura 6
plot(datos$ancho,datos$peso,main = "Peso vs Ancho")
abline(lm(peso~ancho,data = datos),col ="blue")
legend("bottomright",c("Recta ajustada de RLS"),bty="n",col = "blue",lwd=2,lty = 1)

modelo_ancho<-lm(peso~ancho,data = datos)
summary(modelo)

#figura 7
plot(datos$largo,datos$peso,main = "Peso vs Altura",xlab = "Altura",
     ylab = "Peso")
abline(lm(peso~largo,data = datos),col ="blue")
legend("bottomright",c("Recta ajustada de RLS"),bty="n",col = "blue",lwd=2,lty = 1)

modelo_largo<-lm(peso~largo,data = datos)
modelo$coefficients
modelo %>% myQQnorm()

#validacion del supuesto de varianza constante
#figura 8 para el ancho validacion
plot(fitted(modelo_ancho), residuals(modelo_ancho), xlab = "Ancho",
     ylab = "Residuales", main = "Residuales vs. valores ajustados")
abline(h = 0, lty = 2, col = 2)
#figura 9 para el largo validacion

plot(fitted(modelo_largo), residuals(modelo_largo), xlab = "Ancho",
     ylab = "Residuales", main = "Residuales vs. valores ajustados")
abline(h = 0, lty = 2, col = 2)

##falta sxx Suma de cuadrados corregidos en x clase 1
