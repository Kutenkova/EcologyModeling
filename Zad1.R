#Кутенкова М.В. для Алексеевского района докажите что высота родов Липа и Ель значимо отличаются.
# задание 1
# очистим полностью память
rm(list=ls())

# Проверка рабочей директории
getwd()
# Рабочая директория
setwd ("D:/AR/Zad1")

# Считаем данные в переменную adat и просмотрим их
greendb=read.csv("greendb.csv", sep=",",dec="."); greendb

#install.packages("dplyr")
library(dplyr)
#install.packages("reader")
library(reader)
#install.packages("stringr")
library(stringr)

# Но - высота обоих родов деревьев значимо не отличаются
# Н1 - высота обоих родов деревьев значимо отличаются
# для район Нагатино-Садовники докажите что высота родов Липа и Ель значимо отличаются 
#(в Алексеевском районе нет рода Ель)
spec=greendb$species_ru
spec
#род
genus=stringr::str_split(spec, pattern=" ",simplify=T)[,1]
genus
data=greendb%>%mutate(Genus=genus)
data

data=data%>%filter(Genus%in% c("Липа","Ель")) %>%
  filter(adm_region=="район Нагатино-Садовники")

greendb$Genus%>%unique()
greendb$adm_region%>%unique()

#Да, Если отвеРгАЕм НО то значимо отличаются
data.aov = aov(d_trunk_m ~ Genus, data=data)
summary(data.aov)
# высота родов Липа и Ель значимо отличаются