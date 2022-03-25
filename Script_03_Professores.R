# Script 03 - Eixo Professores

# Bibliotecas:
library(descr)
library(dplyr)
library(readr)

load("dados/PROF_SE.RData")


# INDICADOR 3: Complementação pedagógica

freq(PROF_SE$IN_COMPLEMENTACAO_PEDAGOGICA)

# Não há valores faltantes

COMP <- PROF_SE %>% 
  select(NO_MUNICIPIO, CO_MUNICIPIO, IN_COMPLEMENTACAO_PEDAGOGICA, ID_DOCENTE) %>% 
  group_by(ID_DOCENTE, NO_MUNICIPIO, CO_MUNICIPIO) %>% 
  summarize(formacao = mean(IN_COMPLEMENTACAO_PEDAGOGICA))

IND03 <- COMP %>% 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) %>% 
  summarise(Sim = mean(formacao==1)*100,
            Não = mean(formacao==0)*100)%>%
  select(NO_MUNICIPIO, CO_MUNICIPIO, Sim) %>% 
  arrange(desc(Sim))

save(IND03, file = "dados/indicadores/IND03.RData")

write_xlsx(IND03, path = "dados/ind_excel/IND03.xlsx")


# Curso de Pós Graduação (Especialização)

freq(PROF_SE$IN_ESPECIALIZACAO)
# 10% de NA que serão eliminados da análise

POS <- PROF_SE %>% 
  select(NO_MUNICIPIO, CO_MUNICIPIO, IN_ESPECIALIZACAO, ID_DOCENTE) %>%
  na.omit() %>% 
  group_by(ID_DOCENTE, NO_MUNICIPIO, CO_MUNICIPIO) %>% 
  summarize(formacao = mean(IN_ESPECIALIZACAO))

freq(POS$formacao)

IND04 <- POS %>% 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) %>% 
  summarise(Sim = mean(formacao==1)*100,
            Não = mean(formacao==0)*100)%>%
  select(NO_MUNICIPIO, CO_MUNICIPIO, Sim) %>% 
  arrange(desc(Sim))

save(IND04, file = "dados/indicadores/IND04.RData")

write_xlsx(IND04, path = "dados/ind_excel/IND04.xlsx")

  
  