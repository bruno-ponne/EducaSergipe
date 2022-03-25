# Script 04 - Eixo Escola

# Bibliotecas:
library(descr)
library(dplyr)
library(readr)

# INDICADOR 05: Livro Didático

load("dados/DIR_SE.Rdata")

AUX <- DIR_SE %>% group_by(NO_MUNICIPIO) %>% 
  summarise(n())

DIR_SE$TX_RESP_Q056[DIR_SE$TX_RESP_Q056==""] <- NA

freq(DIR_SE$TX_RESP_Q056) # 8,8% NA

DIR_SE <- DIR_SE %>% na.omit()

DIR_SE$livro <- 0
DIR_SE$livro[DIR_SE$TX_RESP_Q056=="C"|DIR_SE$TX_RESP_Q056=="D"] <- 1


IND05 <- DIR_SE %>% 
  select(NO_MUNICIPIO, ID_MUNICIPIO, livro) %>% 
  group_by(NO_MUNICIPIO, ID_MUNICIPIO)%>% 
  summarise(livro = mean(livro==1)*100) %>% 
  arrange(desc(livro))

save(IND05, file = "dados/indicadores/IND05.RData")

write_xlsx(IND05, path = "dados/ind_excel/IND05.xlsx")



load("dados/ESCOLAS_SE.Rdata")

# INDICADOR 06: Quadra de esportes

freq(ESCOLAS_SE$IN_QUADRA_ESPORTES) # 17,6% NA

IND06<- ESCOLAS_SE %>% 
  select(NO_MUNICIPIO, CO_MUNICIPIO, IN_QUADRA_ESPORTES) %>% 
  na.omit()%>% 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO)%>% 
  summarise(quadras = mean(IN_QUADRA_ESPORTES==1)*100) %>% 
  arrange(desc(quadras))

save(IND06, file = "dados/indicadores/IND06.RData")


write_xlsx(IND06, path = "dados/ind_excel/IND06.xlsx")


# INDICADOR 07: rampa de acesso

freq(ESCOLAS_SE$IN_ACESSIBILIDADE_RAMPAS)  # 17,6% NA



IND07 <- ESCOLAS_SE %>% 
  select(NO_MUNICIPIO, CO_MUNICIPIO, IN_ACESSIBILIDADE_RAMPAS) %>% 
  na.omit()%>% 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO)%>% 
  summarise(acessibilidade = mean(IN_ACESSIBILIDADE_RAMPAS==1)*100) %>% 
  arrange(desc(acessibilidade))

save(IND07, file = "dados/indicadores/IND07.RData")

write_xlsx(IND07, path = "dados/ind_excel/IND07.xlsx")

# INDICADOR 08: Biblioteca

freq(ESCOLAS_SE$IN_BIBLIOTECA) # 17,6% NA


IND08 <- ESCOLAS_SE %>% 
  select(NO_MUNICIPIO, CO_MUNICIPIO, IN_BIBLIOTECA) %>% 
  na.omit()%>% 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO)%>% 
  summarise(bibliotecas = mean(IN_BIBLIOTECA==1)*100) %>% 
  arrange(desc(bibliotecas))

save(IND08, file = "dados/indicadores/IND08.RData")

write_xlsx(IND08, path = "dados/ind_excel/IND08.xlsx")


# INDICADOR 09: Laboratório de Informática

freq(ESCOLAS_SE$IN_LABORATORIO_INFORMATICA) # 17,6% NA

IND09 <- ESCOLAS_SE %>% 
  select(NO_MUNICIPIO, CO_MUNICIPIO, IN_LABORATORIO_INFORMATICA)%>% 
  na.omit()%>% 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO)%>% 
  summarise(lab= mean(IN_LABORATORIO_INFORMATICA==1)*100) %>% 
  arrange(desc(lab))

save(IND09, file = "dados/indicadores/IND09.RData")

write_xlsx(IND09, path = "dados/ind_excel/IND09.xlsx")


# INDICADOR 10: Acesso à Internet 

freq(ESCOLAS_SE$IN_INTERNET) # 17,6% NA


IND10 <- ESCOLAS_SE %>% 
  select(NO_MUNICIPIO, CO_MUNICIPIO,IN_INTERNET) %>% 
  na.omit() %>% 
  group_by(NO_MUNICIPIO,CO_MUNICIPIO)%>% 
  summarise(internet = mean(IN_INTERNET==1)*100) %>% 
  arrange(desc(internet))

save(IND10, file = "dados/indicadores/IND10.RData")

write_xlsx(IND10, path = "dados/ind_excel/IND10.xlsx")


