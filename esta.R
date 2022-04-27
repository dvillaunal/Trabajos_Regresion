library(tidyverse)
library(readxl)
library(ggExtra)
library(GGally)
library(magrittr)
library(knitr)

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

#boxplot o violin?

estaturas %>% ggplot(aes( x = genero, y = est_sujeto, fill = genero)) + 
  geom_boxplot() + xlab("Genero") + ylab("Estatura en cm") + 
  ggtitle("Estatura por genero") + geom_jitter(colour = "black",width = 0.05)

#agrupar por padre y madre, decorarar

l <- layout(matrix(c(1, 2),  # Primero, segundo
                   nrow = 2,
                   ncol = 2,
                   byrow = TRUE))

layout.show(l)
#Estatura del padre sin diferenciar por sexo
estaturas %>% ggplot(aes( y = est_padre)) + 
  geom_boxplot(fill='red', color="black", alpha = 0.4) + ylab("Estatura en cm") + 
  ggtitle("Estatura del padre") 

#Estatura del padre diferenciando por genero

estaturas %>% ggplot(aes(x = genero, y = est_padre, fill =genero)) +
  geom_boxplot() + ylab("Estatura en cm") + ggtitle("Estatura del padre") +
  geom_jitter(colour = "black",width = 0.05)

#Estatura de la madre sin diferenciar por genero
estaturas %>% ggplot(aes( y = est_madre)) + 
  geom_boxplot(fill='red', color="black", alpha = 0.4) +  ylab("Estatura en cm") + 
  ggtitle("Estatura de la madre")

#Estatura de la madre diferenciando por genero
estaturas %>% ggplot(aes(x = genero, y = est_madre, fill =genero)) +
  geom_boxplot() + ylab("Estatura en cm") + ggtitle("Estatura de la madre")+
  geom_jitter(colour = "black",width = 0.05)


modelo1 <- lm(data = estaturas, est_sujeto ~  genero+est_madre+est_padre)
summary(modelo1)
#normalidad con test de shapiro SIN SUMAR 1

modelo1 %>% myQQnorm()




