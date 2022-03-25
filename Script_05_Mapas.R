# Script 05 - Mapas e Desempenho

# Bibliotecas:
library(descr)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)

load("dados/SERGIPE_ALUNO.Rdata")
load("dados/ESCOLAS_SE.Rdata")
load("dados/RANKING.Rdata")

# Carregando o mapa de Sergipe:

map_se <- read_sf("dados/mapa/28MUE250GC_SIR.shp")

map_se <- rename(map_se, CO_MUNICIPIO = CD_GEOCODM )

map_se$CO_MUNICIPIO = as.numeric(map_se$CO_MUNICIPIO)



# Distribuição Espacial do IDEB - Figura 3

map_ideb <- left_join(map_se, RANKING, by = "CO_MUNICIPIO")



ggplot()+
  geom_sf(data = map_ideb, aes(geometry=geometry, fill = IDEB_VALIDO), color="black", size= 0.3)+
  scale_fill_gradient(name = "Ideb", low ="#F8C301", high ="#29166F")+
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank())

ggsave(filename = "viz/map_se.png")



# Mapas de localização - Mapas das páginas 29 a 103.

map_se <- read_sf("dados/mapa/28MUE250GC_SIR.shp")
map_se$CD_GEOCODM <- as.numeric(map_se$CD_GEOCODM)
map_se <- rename(map_se, ID_MUNICIPIO = CD_GEOCODM )

cod_mun_SE <- read_delim("dados/cod_mun_SE.csv", ";", escape_double = FALSE, trim_ws = TRUE)
names(cod_mun_SE) <- c("ID_MUNICIPIO", "NO_MUNICIPIO")
cod_mun_SE$ID_MUNICIPIO <- as.numeric(cod_mun_SE$ID_MUNICIPIO)

map_se <- left_join(map_se,cod_mun_SE, by= "ID_MUNICIPIO")

for(element in map_se$NO_MUNICIPIO){
  plot <- ggplot()+
    geom_sf(data = map_se, aes(fill = NO_MUNICIPIO == element, color = NO_MUNICIPIO == element))+
    scale_fill_manual(values = c("#F8C301", "#009B40"))+
    scale_color_manual(values = c("#F8C301", "#009B40"))+
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
      panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  nome <- paste("mapas_prontos/", element,".png", sep="")
  ggsave(plot, filename = nome,  bg = "transparent", width = 7, height = 10, units = "cm")
  }



# Carregando todos os indicadores 

for(i in 1:10){
  load(paste("dados/indicadores/IND0",as.character(i),".RData", sep=""))}

load("dados/indicadores/IND10.RData")
load("dados/RANKING.Rdata")





