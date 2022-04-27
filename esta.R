library(tidyverse)
library(readxl)
library(ggExtra)
library(GGally)
library(magrittr)
estaturas <- read_csv("estaturas.csv")
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
  ggtitle("Estatura por genero") #+ geom_jitter(colour = "black",width = 0.3)

#proximo boxplot agrupar por padre y madre con y = estatura de cada uno
estaturas %>% ggplot(aes( x = genero, y = est_padre, fill = genero)) + 
  geom_boxplot() + xlab("Genero") + ylab("Estatura en cm") + 
  ggtitle("Estatura por genero")


