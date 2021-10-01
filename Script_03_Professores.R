# Script 03 - Eixo Professores

# Bibliotecas:
library(descr)
library(dplyr)
library(readr)

load("dados/PROF_SE.RData")

N_PROF_MUN <- PROF_SE %>% 
  group_by(NO_MUNICIPIO) %>% 
  summarise(n())

# Complementação pedagógica

freq(PROF_SE$IN_COMPLEMENTACAO_PEDAGOGICA)
# Não há valores faltantes


PROF_SE$IN_COMPLEMENTACAO_PEDAGOGICA[PROF_SE$IN_COMPLEMENTACAO_PEDAGOGICA==0] <- "não"
PROF_SE$IN_COMPLEMENTACAO_PEDAGOGICA[PROF_SE$IN_COMPLEMENTACAO_PEDAGOGICA==1] <- "sim"

RESUMO_COMP <- PROF_SE %>% 
  group_by(NO_MUNICIPIO) %>% 
  summarise(Sim = mean(IN_COMPLEMENTACAO_PEDAGOGICA=="sim")*100,
            Não = mean(IN_COMPLEMENTACAO_PEDAGOGICA=="não")*100) %>% 
  arrange(desc(Sim))

# Curso de Pós Graduação (Especialização)

PROF_SE$IN_ESPECIALIZACAO[PROF_SE$IN_ESPECIALIZACAO==0] <- "não"
PROF_SE$IN_ESPECIALIZACAO[PROF_SE$IN_ESPECIALIZACAO==1] <- "sim"

freq(PROF_SE$IN_ESPECIALIZACAO)
# 10% de NA que serão eliminados da análise

RESUMO_ESP <- PROF_SE %>% 
  select(NO_MUNICIPIO, IN_ESPECIALIZACAO) %>% 
  na.omit()%>% 
  group_by(NO_MUNICIPIO)%>% 
  summarise(Sim = mean(IN_ESPECIALIZACAO=="sim")*100,
            Não = mean(IN_ESPECIALIZACAO=="não")*100) %>% 
  arrange(desc(Sim))