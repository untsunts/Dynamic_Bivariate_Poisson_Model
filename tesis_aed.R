# rm(list = ls())
library(tidyverse)
library(magrittr)
library(data.table)
library(lubridate)
library(stringr)
###### lectura bases de datos con resultados de partidos -----
setwd("Desktop/ITAM/Tesis/")
setwd("datos")
nombres_dir <- dir()
nombres_liga <- nombres_dir[nombres_dir %>% str_detect(pattern = "laliga_")]
nombres_premier <- nombres_dir[nombres_dir %>% str_detect(pattern = "premier_")]
partidos <- data.frame()
for(i in nombres_premier){
  partidos_aux <- read_csv(i, col_types = list(Date = col_date(format = "%d/%m/%y")))
  partidos <- rbind(partidos, data.frame(partidos_aux[,1:7], season = gsub(i, pattern = ".csv", replacement = "")))
}

partidos[sapply(partidos, is.character)] <- data.frame(lapply(partidos[sapply(partidos, is.character)], factor)) 
partidos %>% summary()
partidos %>% 
  dplyr::filter(!is.na(HomeTeam)) %>% 
  ggplot(aes(x = Date, y = FTHG, colour = FTR)) + 
  facet_wrap(~HomeTeam) + 
  geom_line(size = 1.5, col = "gray", alpha = 0.2) +
  geom_point(alpha = 0.35) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 7)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%y")

partidos %>% 
  dplyr::filter(!is.na(HomeTeam)) %>% 
  ggplot(aes(x = Date, y = FTAG, colour = FTR)) + 
  facet_wrap(~HomeTeam) + 
  geom_line(size = 1.5, col = "gray", alpha = 0.2) +
  geom_point(alpha = 0.35) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 7)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%y")

partidos %>% 
  dplyr::filter(!is.na(HomeTeam)) %>% 
  ggplot(aes(x = Date, y = FTHG-FTAG, colour = FTR)) + 
  facet_wrap(~HomeTeam) + 
  geom_line(size = 1.5, col = "gray", alpha = 0.2) +
  geom_point(alpha = 0.35) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 7)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%y")

partidos %>% 
  dplyr::filter(!is.na(HomeTeam)) %>% 
  ggplot(aes(x = Date, y = FTHG-FTAG, colour = FTR)) + 
  facet_wrap(~HomeTeam) + 
  geom_line(size = 1.5, col = "gray", alpha = 0.2) +
  geom_point(alpha = 0.35) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 7)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%y")
partidos %>% 
  dplyr::filter(!is.na(HomeTeam)) %>% 
  dplyr::mutate(diff_G = FTHG - FTAG) %>% 
  ggplot(aes(x = Date, y = diff_G, colour = season)) + 
  facet_wrap(~HomeTeam) + 
  geom_line(size = 1.5, col = "gray", alpha = 0.2) +
  geom_point(alpha = 0.35) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 7)) +
  geom_hline(yintercept = 0, col = "black") +
  scale_x_date(date_breaks = "2 years", date_labels = "%y") +
  ylim(c(-8,8))
