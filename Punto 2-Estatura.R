library(tidyverse)
library(readxl)
library(ggExtra)
library(GGally)
Est <- read_excel("C:/Users/chiqu/Mi unidad/SEMESTRE-2022-1/ANALISIS DE REGRESION/Parciales/Parcial 1/Punto 2-Estatura/Base_Estatura.xlsx")
attach(Est)
View(Est)


Est%>%str()

##variables a factor y numericas
Est <- within(Est, {
  Estatura <- as.numeric(Estatura)
  Estatura_Madre <- as.numeric(Estatura_Madre)
  Estatura_Padre <- as.numeric(Estatura_Padre)
  Genero <- as.factor(Genero)
})
##
Est%>%mutate(Est,Prom_estaturas=Estatura_Madre+Estatura_Padre)
Est%>%colnames()

##resumen numerico
summary(Est)

## histograma
ggplot(data=Est)+geom_histogram(aes(x=Estatura, y=..count..),colour="black",fill=topo.colors(14),alpha = 0.8,binwidth =0.03)+  
  ggtitle("Histograma Estatura del sujeto por Genero")+
  labs(x="Estatura",y="Frecuencia")+facet_wrap(~Genero)

##boxplot estatura sujeto

plot_1 <- ggplot(data = Est, aes(x = Genero, y = Estatura, fill = Genero)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  geom_boxplot(outlier.colour = "transparent") + 
  geom_jitter(colour = "black", width = 0.1, height = 0) + 
  xlab("Genero") + 
  ylab("Estatura") + 
  labs(fill = "Genero") + 
  labs(title = "Boxplot Estatura sujeto") + 
  theme_bw(base_size = 14, base_family = "sans")
plot_1

##boxplot estatura madre
plot_2 <- ggplot(data = Est, aes(x = Genero, y = Estatura_Madre, fill = Genero)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  geom_boxplot(outlier.colour = "transparent") + 
  geom_jitter(colour = "black", width = 0.1, height = 0) + 
  xlab("Genero") + 
  ylab("Estatura Madre") + 
  labs(fill = "Genero") + 
  labs(title = "Boxplot Estatura Madre") + 
  theme_bw(base_size = 14, base_family = "sans")
plot_2

##boxplot estatura padre
plot_3 <- ggplot(data = Est, aes(x = Genero, y = Estatura_Padre, fill = Genero)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  geom_boxplot(outlier.colour = "transparent") + 
  geom_jitter(colour = "black", width = 0.1, height = 0) + 
  xlab("Genero") + 
  ylab("Estatura Padre") + 
  labs(fill = "Genero") + 
  labs(title = "Boxplot Estatura Padre") + 
  theme_bw(base_size = 14, base_family = "sans")
plot_3





##modelo multiple

modelo<-lm(data = Est,Estatura~Estatura_Madre+Estatura_Padre+Genero)
summary(modelo)



pairs(Est)

#Test de normalidad sobre residuales
test<-shapiro.test(residuals(modelo)) #Test de normalidad sobre residuales

#Gráfico de normalidad con información del test Shapiro
qqnorm(residuals(modelo),cex=1.5,bty="n",font=3,font.main=3)
qqline(residuals(modelo),lty=1,lwd=2,col=2)
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test$statistic,test$p.value),digits=4)),cex=0.8)



#Estatura sujeto vs Estatura madre
ggplt <- ggplot(Est,aes(x=Estatura_Madre,y=Estatura))
plot_1.1<-ggplt+geom_point()+geom_smooth(method = "lm",color="#8A9497")+
  ggtitle("Estatura sujeto vs Estatura madre")+
  theme(panel.background = element_rect(fill = "gray86",color="Black",size = 1))
plot_1.1
##grafico marginal
ggMarginal(plot_1.1,type="density")



#Estatura sujeto vs Estatura padre

ggplt <- ggplot(Est,aes(x=Estatura_Padre,y=Estatura,fill=Genero))
plot_2.1<-ggplt+geom_point()+geom_smooth(method = "lm",color="#8A9497")+
  ggtitle("Estatura sujeto vs Estatura Padre")+
  theme(panel.background = element_rect(fill = "gray86",color="Black",size = 1))
plot_2.1
ggMarginal(plot_2.1,type="density")

?geom_smooth
#genero
ggplt <- ggplot(Est,aes(x=Genero,y=Estatura))
ggplt+geom_point()+geom_smooth(method = "lm",color="#8A9497")+
  ggtitle("Estatura sujeto vs Genero")+
  theme(panel.background = element_rect(fill = "gray86",color="Black",size = 1))


##residuales
plot(modelo$residuals,
     ylab="Residual crudo",xlab="observacíon")
abline(h=0,col="red")


##
plot(modelo$residuals,type = "l",
     ylab="Residual crudo",xlab="observacíon")
abline(h=0,col="red")

##
plot(rstandard(modelo),ylim=c(-3,3),
     ylab="Residual estandarizado",xlab="observacíon")
abline(h=c(0,-2.3,2.3),col="red")

##
plot(rstudent(modelo),ylim=c(-4,3),
     ylab="Residual Studentizado",xlab="observacíon")
abline(h=c(0,-2.2,2.2),col="red")

ggpairs(Est,aes(color = Genero, alpha = 0.5),lower = list(continuous = "smooth"))
corrplot(cor(Est),method = "")

