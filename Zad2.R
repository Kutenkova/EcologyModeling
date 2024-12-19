#постройте картосхему максимальных высот 
#стволов деревьев родов Липа и Ель с диаметрами ствола более 15 см

# Установим необходимые пакеты (если не установлены)
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")

library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# Очистим полностью память 
rm(list=ls())

# Проверка рабочей директории
getwd()
# Рабочая директория
setwd ("D:/AR/Zad2")

# Считаем данные в переменные
greendb = read.csv("greendb.csv") 
map = sf::read_sf("moscow.geojson")

# График с заливкой
ggplot(map) + geom_sf(aes(fill = NAME)) + theme(legend.position = "none")

# Фильтруем данные для Липы и Ели с диаметрами ствола более 15 см
spec = greendb$species_ru
genus = stringr::str_split(spec, pattern = " ", simplify = TRUE)[, 1]
data = greendb %>% mutate(Genus = genus)

# Фильтрация по диаметру и группировка по региону и роду
max_height = data %>%
  group_by(adm_region, Genus,d_trunk_m) %>%
  filter(d_trunk_m > 0.15) %>%  # Условие для диаметра ствола
  summarise(max_h = max(height_m, na.rm = TRUE), .groups = "drop") %>%  # Условия для родов
  filter(Genus %in% c("Липа", "Ель"))  # Условия для родов

# Преобразуем данные в широкий формат
max_height= pivot_wider(max_height, names_from = Genus, values_from = max_h)

# Объединяем данные с картой
map = map %>% mutate(adm_region = NAME)
map = left_join(map, max_height, by = "adm_region")

# Построение картосхемы для Липы
ggplot(map) +
  geom_sf(aes(fill = `Липа`)) + 
  theme() 
  

# Построение картосхемы для Ели
ggplot(map) +
  geom_sf(aes(fill = `Ель`)) + 
  theme()
