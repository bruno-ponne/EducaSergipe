# Script 05 - Mapas e Desempenho

# Bibliotecas:
library(descr)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)

load("dados/SERGIPE_ALUNO.Rdata")
load("dados/ESCOLAS_SE.Rdata")


# Desempenho em Português

DESEMP_PORT_M <- SERGIPE_ALUNO %>% 
  group_by(ID_MUNICIPIO) %>%
  summarise(media = mean(PROFICIENCIA_LP_SAEB, na.rm = TRUE))

# Carregando o mapa de Sergipe:

map_se <- read_sf("dados/mapa/28MUE250GC_SIR.shp")

# Desempenho + mapa

map_se <- rename(map_se, ID_MUNICIPIO = CD_GEOCODM )

map_se$ID_MUNICIPIO = as.numeric(map_se$ID_MUNICIPIO)

MAP_DADOS <- left_join(map_se, DESEMP_PORT_M, by = "ID_MUNICIPIO")

ggplot()+
  geom_sf(data = MAP_DADOS, aes(geometry=geometry, fill = media), size=0.05, color="gray")+
  scale_fill_gradient(name = "Nota em Portugês", low = '#f1eef6', high = '#034e7b')+
  labs(title = "Desempenho no SAEB",
       subtitle = "Português") +
  theme(panel.background = element_rect(fill = "white", colour = "gray"))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

# Bibliotecas

freq(ESCOLAS_SE$IN_BIBLIOTECA) # 17,6% NA

ESCOLAS_SE$IN_BIBLIOTECA[ESCOLAS_SE$IN_BIBLIOTECA==0] <- "não"
ESCOLAS_SE$IN_BIBLIOTECA[ESCOLAS_SE$IN_BIBLIOTECA==1] <- "sim"

RESUMO_BIBLIOTECAS <- ESCOLAS_SE %>% 
  select(CO_MUNICIPIO, IN_BIBLIOTECA) %>% 
  na.omit()%>% 
  group_by(CO_MUNICIPIO)%>% 
  summarise(Sim = mean(IN_BIBLIOTECA=="sim")*100,
            Não = mean(IN_BIBLIOTECA=="não")*100) %>% 
  arrange(desc(Sim))

# Mapa + Bibliotecas

map_se <- rename(map_se, CO_MUNICIPIO = ID_MUNICIPIO)


MAP_BIB <- left_join(map_se, RESUMO_BIBLIOTECAS, by = "CO_MUNICIPIO")

sem_bib <- MAP_BIB %>% filter(Sim==0)

ggplot()+
  geom_sf(data = MAP_DADOS, aes(geometry=geometry, fill = media), size=0.05, color="gray")+
  scale_fill_gradient(name = "Nota em Portugês", low = '#f1eef6', high = '#034e7b')+
  labs(title = "Desempenho no SAEB",
       subtitle = "Em vermelho, municípios sem escola com biblioteca") +
  geom_sf(data = sem_bib, aes(geometry=geometry), fill = "transparent", color="#99000d")+
  theme(panel.background = element_rect(fill = "white", colour = "gray"))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())


