## ----setup, include=FALSE-------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE, include=FALSE--------------------------
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
# lectura de la base de datos:
stature <- read_csv("estaturas.csv")


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
kable(rbind(head(stature, n = 5),rep(".", ncol(stature)),
            rep(".", ncol(stature)),rep(".", ncol(stature)),
            tail(stature, n = 5)),digits = 30, align = "c")


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
stature$genero %<>% as.factor()
stature$exp_sujeto %<>% as.Date(format="%d/%m/%Y")
stature$exp_padre %<>% as.Date(format="%d/%m/%Y")
stature$exp_madre %<>% as.Date(format="%d/%m/%Y")
stature$ced_madre %<>%  as.character()
stature %>% ggpairs(.,columns=c(1,2,4,5), aes(color=genero,alpha=0.5))


## ----message=FALSE, warning=FALSE, include=FALSE--------------------------
colnames(stature) <- c("est","genero","exp","est_ma","est_pa","ced",
                         "ced_ma","ced_pa","exp_pa","exp_ma")


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
modelo.m <- lm(est ~ est_pa + est_ma + genero, data = stature)
summary(modelo.m)


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
modelo2 <- lm(est~est_ma+genero, data=stature)
modelo3 <- lm(est~est_pa+genero, data=stature)
modelo4 <- lm(est~est_ma, data=stature)
modelo5 <- lm(est~est_pa, data=stature)
modelo6 <- lm(est~genero, data=stature)

summary(modelo2)
summary(modelo3)
summary(modelo4)
summary(modelo5)
summary(modelo6)


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
anova(modelo2,modelo3)
anova(modelo2,modelo4)
anova(modelo2,modelo5)
anova(modelo2,modelo6)
print("Mejor modelo del 2 al 6 con menor RSS")
anova(modelo2,modelo.m)


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
# Eliminamos la variables que no nos interesan en el modelo:
df <- stature[, c(1,2,4,5)]
summary(df)


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
cor.test(df$est, df$est_ma)
cor.test(df$est, df$est_pa)
cor.test(df$est_pa, df$est_ma)


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
df %>% ggplot(., aes(x=genero, y=est, color=genero))+
  geom_boxplot()+
  geom_jitter(width = 0.1)+
  theme_bw()+theme(legend.position = "none")+
  labs(x="Estatura del sujeto encuestado",
       y="Genero del sujeto encuestado",
       title = "Boxplot Genero vs Estatura")


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
summary(modelo2)$coefficients


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
summary(modelo2)


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
df %>% ggplot(.,aes(x=est_ma, y=modelo2$residuals))+
  geom_point()+
  geom_smooth(color="firebrick")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  labs(x="Estatura de las madres de los sujetos",
       y="Residuales")


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------
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

modelo2 %>% myQQnorm()


## -------------------------------------------------------------------------
ggplot(data = data.frame(predict_values = predict(modelo2),
                                residuos = residuals(modelo2)),
              aes(x = predict_values, y = residuos))+
         geom_point()+
         geom_smooth(color = "firebrick", se = FALSE)+
         geom_hline(yintercept = 0)+
         theme_bw()

bptest(modelo2)


## -------------------------------------------------------------------------
dwt(modelo2,alternative = "two.sided")


## -------------------------------------------------------------------------
outlierTest(modelo2)


## -------------------------------------------------------------------------
summary(influence.measures(modelo2))
influencePlot(modelo2)

