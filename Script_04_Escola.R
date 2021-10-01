# Script 04 - Eixo Escola

# Bibliotecas:
library(descr)
library(dplyr)
library(readr)

load("dados/ESCOLAS_SE.Rdata")

freq(ESCOLAS_SE$IN_QUADRA_ESPORTES) # 17,6% NA

ESCOLAS_SE$IN_QUADRA_ESPORTES[ESCOLAS_SE$IN_QUADRA_ESPORTES==0] <- "não"
ESCOLAS_SE$IN_QUADRA_ESPORTES[ESCOLAS_SE$IN_QUADRA_ESPORTES==1] <- "sim"

RESUMO_QUADRA <- ESCOLAS_SE %>% 
  select(NO_MUNICIPIO, IN_QUADRA_ESPORTES) %>% 
  na.omit()%>% 
  group_by(NO_MUNICIPIO)%>% 
  summarise(Sim = mean(IN_QUADRA_ESPORTES=="sim")*100,
            Não = mean(IN_QUADRA_ESPORTES=="não")*100) %>% 
  arrange(desc(Sim))



# Acessibilidade: rampa de acesso

freq(ESCOLAS_SE$IN_ACESSIBILIDADE_RAMPAS)  # 17,6% NA

ESCOLAS_SE$IN_ACESSIBILIDADE_RAMPAS[ESCOLAS_SE$IN_ACESSIBILIDADE_RAMPAS==0] <- "não"
ESCOLAS_SE$IN_ACESSIBILIDADE_RAMPAS[ESCOLAS_SE$IN_ACESSIBILIDADE_RAMPAS==1] <- "sim"

RESUMO_RAMPAS <- ESCOLAS_SE %>% 
  select(NO_MUNICIPIO, IN_ACESSIBILIDADE_RAMPAS) %>% 
  na.omit()%>% 
  group_by(NO_MUNICIPIO)%>% 
  summarise(Sim = mean(IN_ACESSIBILIDADE_RAMPAS=="sim")*100,
            Não = mean(IN_ACESSIBILIDADE_RAMPAS=="não")*100) %>% 
  arrange(desc(Sim))


# Estrutura: Biblioteca

freq(ESCOLAS_SE$IN_BIBLIOTECA) # 17,6% NA

ESCOLAS_SE$IN_BIBLIOTECA[ESCOLAS_SE$IN_BIBLIOTECA==0] <- "não"
ESCOLAS_SE$IN_BIBLIOTECA[ESCOLAS_SE$IN_BIBLIOTECA==1] <- "sim"

RESUMO_BIBLIOTECAS <- ESCOLAS_SE %>% 
  select(NO_MUNICIPIO, IN_BIBLIOTECA) %>% 
  na.omit()%>% 
  group_by(NO_MUNICIPIO)%>% 
  summarise(Sim = mean(IN_BIBLIOTECA=="sim")*100,
            Não = mean(IN_BIBLIOTECA=="não")*100) %>% 
  arrange(desc(Sim))




# Conectividade: Laboratório de Informática

freq(ESCOLAS_SE$IN_LABORATORIO_INFORMATICA) # 17,6% NA


ESCOLAS_SE$IN_LABORATORIO_INFORMATICA[ESCOLAS_SE$IN_LABORATORIO_INFORMATICA==0] <- "não"
ESCOLAS_SE$IN_LABORATORIO_INFORMATICA[ESCOLAS_SE$IN_LABORATORIO_INFORMATICA==1] <- "sim"

RESUMO_LAB_INFO <- ESCOLAS_SE%>% 
  select(NO_MUNICIPIO, IN_LABORATORIO_INFORMATICA)%>% 
  na.omit()%>% 
  group_by(NO_MUNICIPIO)%>% 
  summarise(Sim = mean(IN_LABORATORIO_INFORMATICA=="sim")*100,
            Não = mean(IN_LABORATORIO_INFORMATICA=="não")*100) %>% 
  arrange(desc(Sim))



# Conectividade: Acesso à Internet 

freq(ESCOLAS_SE$IN_INTERNET) # 17,6% NA

ESCOLAS_SE$IN_INTERNET[ESCOLAS_SE$IN_INTERNET==0] <- "não"
ESCOLAS_SE$IN_INTERNET[ESCOLAS_SE$IN_INTERNET==1] <- "sim"

RESUMO_INTERNET <- ESCOLAS_SE%>% 
  select(NO_MUNICIPIO, IN_INTERNET)%>% 
  na.omit()%>% 
  group_by(NO_MUNICIPIO)%>% 
  summarise(Sim = mean(IN_INTERNET=="sim")*100,
            Não = mean(IN_INTERNET=="não")*100) %>% 
  arrange(desc(Sim))


