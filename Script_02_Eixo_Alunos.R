# Script 02 - Eixo Alunos

# Bibliotecas:
library(descr)
library(dplyr)
library(readr)
library(writexl)


load("dados/SERGIPE_ALUNO.Rdata")

# INDICADOR 1: FREQUÊNCIA DOS PAIS A REUNIÕES

freq(SERGIPE_ALUNO$TX_RESP_Q006E)

SERGIPE_ALUNO$TX_RESP_Q006E[SERGIPE_ALUNO$TX_RESP_Q006E=="*"] <- NA
SERGIPE_ALUNO$TX_RESP_Q006E[SERGIPE_ALUNO$TX_RESP_Q006E=="."] <- NA

# 25,63% de observações sem resposta para esse indicador

freq(SERGIPE_ALUNO$TX_RESP_Q006E)

IND01 <- SERGIPE_ALUNO %>%
  na.omit() %>% 
  group_by(NO_MUNICIPIO, ID_MUNICIPIO) %>% 
  summarise(Nunca_QuaseNunca = mean(TX_RESP_Q006E=="A")*100,
            De_vez_em_quando = mean(TX_RESP_Q006E=="B")*100,
            Sempre_QuaseSempre = mean(TX_RESP_Q006E=="C")*100) %>% 
  select(NO_MUNICIPIO, ID_MUNICIPIO, Nunca_QuaseNunca) %>% 
  arrange(Nunca_QuaseNunca)

save(IND01, file = "dados/indicadores/IND01.RData")

write_xlsx(IND01, path = "dados/ind_excel/IND01.xlsx")


# INDICADOR 2: REDE WI-FI EM CASA

freq(SERGIPE_ALUNO$TX_RESP_Q010B)

SERGIPE_ALUNO$TX_RESP_Q010B[SERGIPE_ALUNO$TX_RESP_Q010B=="*"] <- NA
SERGIPE_ALUNO$TX_RESP_Q010B[SERGIPE_ALUNO$TX_RESP_Q010B=="."] <- NA

# 21,25% de observações sem resposta para esse indicador
freq(SERGIPE_ALUNO$TX_RESP_Q010B)

IND02 <- SERGIPE_ALUNO %>% 
  na.omit() %>% 
  group_by(NO_MUNICIPIO, ID_MUNICIPIO) %>% 
  summarise(Não = mean(TX_RESP_Q010B=="A")*100,
            Sim = mean(TX_RESP_Q010B=="B")*100) %>% 
  select(NO_MUNICIPIO, ID_MUNICIPIO, Sim) %>% 
  arrange(desc(Sim))

save(IND02, file = "dados/indicadores/IND02.RData")

write_xlsx(IND02, path = "dados/ind_excel/IND02.xlsx")

