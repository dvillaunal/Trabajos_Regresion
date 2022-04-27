## ----setup, include=FALSE--------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE, include=FALSE----
library(magrittr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(janitor)

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


## ----message=TRUE, warning=TRUE, include=FALSE----
# Lectura de los datos:
datos <- read_delim("eggs.csv", delim = ";") %>% clean_names()


## ----echo=FALSE------------------------
## figura 1
kableExtra::kable(rbind(head(datos,n=3),rep(".",ncol(datos)),rep(".",ncol(datos)),tail(datos,n=3)),col.names = c("ID","Altura","Ancho","Peso"),digits = 3,align = 'c')


## ----echo=FALSE------------------------
## figura 2
datos %>% select(-"huevos") %>% summary() %>% kable(align = 'c')


## ----echo=FALSE------------------------
#figura 3 diametro
boxplot(datos$ancho, main = "Boxplot Diametro", col = "Grey", ylab = "Milimetros")

#figura 4 altura
boxplot(datos$largo,main="Boxplot Altura",col="pink", ylab = "Milimetros")


## ----echo=FALSE, message=FALSE, warning=FALSE----
#figura 5 
boxplot(datos$peso,main="Boxplot Peso",col="red", xlab = "Gramos", horizontal = T)


## ----echo=FALSE, message=FALSE, warning=FALSE----
attach(datos)
ggplot(datos, aes(x=ancho, y=peso))+
  geom_point()+geom_smooth(method = "lm", color="red",se=F)+
  theme_bw()+
  labs(x="Ancho en mm", y="Peso en gr", title = "Peso vs Ancho")+
  annotate(geom = "text", y=52.5, x=44, label="Recta Ajustada de RLS", color="red")


## ----echo=FALSE, message=FALSE, warning=FALSE----
lm(peso~ancho,data = datos) %>% myQQnorm()


## ----echo=FALSE, message=FALSE, warning=FALSE----
modelo1 <- lm(peso~ancho, data=datos)
plot(fitted(modelo1), residuals(modelo1), xlab = "Ancho",
ylab = "Residuales", main = "Residuales vs. valores ajustados")+
abline(h = 0, lty = 2, col = 2)


## ----echo=FALSE, message=FALSE, warning=FALSE----
ggplot(datos, aes(x=largo, y=peso))+
  geom_point()+geom_smooth(method = "lm", color="red",se=F)+
  theme_bw()+
  labs(x="Largo en mm", y="Peso en gr", title = "Peso vs Largo")+
  annotate(geom = "text", y=55, x=57, label="Recta Ajustada de RLS", color="red")


## ----echo=FALSE, message=FALSE, warning=FALSE----
modelo2 <- lm(peso~largo,data=datos)
modelo2 %>% myQQnorm()


## ----echo=FALSE, message=FALSE, warning=FALSE----
plot(fitted(modelo2), residuals(modelo2), xlab = "Peso",
     ylab = "Residuales", main = "Residuales vs. valores ajustados")+
abline(h=0,lty = 2, col = 2)

