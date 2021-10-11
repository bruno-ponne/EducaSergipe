# Script 02 - Eixo Alunos

# Bibliotecas:
library(descr)
library(dplyr)
library(readr)


load("dados/SERGIPE_ALUNO.Rdata")


# INDICADOR 1: FREQUÊNCIA DOS PAIS A REUNIÕES

freq(SERGIPE_ALUNO$TX_RESP_Q006E)

SERGIPE_ALUNO$TX_RESP_Q006E[SERGIPE_ALUNO$TX_RESP_Q006E=="*"] <- "sem resposta"
SERGIPE_ALUNO$TX_RESP_Q006E[SERGIPE_ALUNO$TX_RESP_Q006E=="."] <- "sem resposta"

# 25,63% de observações sem resposta para esse indicador
freq(SERGIPE_ALUNO$TX_RESP_Q006E)

REUNIOES <- SERGIPE_ALUNO %>% 
  group_by(NO_MUNICIPIO) %>% 
  summarise(Nunca_QuaseNunca = mean(TX_RESP_Q006E=="A")*100,
            De_vez_em_quando = mean(TX_RESP_Q006E=="B")*100,
            Sempre_QuaseSempre = mean(TX_RESP_Q006E=="C")*100,
            Sem_Resposta = mean(TX_RESP_Q006E=="sem resposta")*100)

# INDICADOR 2: REDE WI-FI EM CASA

freq(SERGIPE_ALUNO$TX_RESP_Q010B)

SERGIPE_ALUNO$TX_RESP_Q010B[SERGIPE_ALUNO$TX_RESP_Q010B=="*"] <- "sem resposta"
SERGIPE_ALUNO$TX_RESP_Q010B[SERGIPE_ALUNO$TX_RESP_Q010B=="."] <- "sem resposta"

# 21,25% de observações sem resposta para esse indicador
freq(SERGIPE_ALUNO$TX_RESP_Q010B)

RESUMO_WIFI <- SERGIPE_ALUNO %>% 
  group_by(NO_MUNICIPIO) %>% 
  summarise(Não = mean(TX_RESP_Q010B=="A")*100,
            Sim = mean(TX_RESP_Q010B=="B")*100,
            Sem_Resposta = mean(TX_RESP_Q010B== "sem resposta")*100)

# INDICADOR 3: TEMPO DESTINADO AO ESTUDO

freq(SERGIPE_ALUNO$TX_RESP_Q017D)

SERGIPE_ALUNO$TX_RESP_Q017D[SERGIPE_ALUNO$TX_RESP_Q017D=="*"] <- "sem resposta"
SERGIPE_ALUNO$TX_RESP_Q017D[SERGIPE_ALUNO$TX_RESP_Q017D=="."] <- "sem resposta"

# 24,13% de observações sem resposta para esse indicador
freq(SERGIPE_ALUNO$TX_RESP_Q017D)

RESUMO_ESTUDO <- SERGIPE_ALUNO %>% 
  group_by(NO_MUNICIPIO) %>% 
  summarise(NãoEstuda = mean(TX_RESP_Q017D=="A")*100,
            Menos_de_1h = mean(TX_RESP_Q017D=="B")*100,
            Entre_1he2h = mean(TX_RESP_Q017D=="C")*100,
            Mais_de_2h = mean(TX_RESP_Q017D=="D")*100,
            Sem_Resposta = mean(TX_RESP_Q017D=="sem resposta")*100)


# Nova versão:
# exclui valores faltantes;
# reduz o número de categorias de cada indicador;
# cria o ranking

freq(SERGIPE_ALUNO$TX_RESP_Q006E)

SERGIPE_ALUNO$TX_RESP_Q006E[SERGIPE_ALUNO$TX_RESP_Q006E=="*"] <- NA
SERGIPE_ALUNO$TX_RESP_Q006E[SERGIPE_ALUNO$TX_RESP_Q006E=="."] <- NA

REUNIAO_DATA <- SERGIPE_ALUNO %>% 
  select(NO_MUNICIPIO,TX_RESP_Q006E) %>% 
  na.omit()



# 25,63% de observações sem resposta para esse indicador
freq(SERGIPE_ALUNO$TX_RESP_Q006E)

REUNIOES_2 <- REUNIAO_DATA %>% 
  group_by(NO_MUNICIPIO) %>% 
  summarise(Nunca_QuaseNunca = mean(TX_RESP_Q006E=="A")*100,
            De_vez_em_quando = mean(TX_RESP_Q006E=="B")*100,
            Sempre_QuaseSempre = mean(TX_RESP_Q006E=="C")*100) %>% 
  select(NO_MUNICIPIO, Nunca_QuaseNunca) %>% 
  arrange(Nunca_QuaseNunca)

REUNIOES_2$Ranking <- row.names(REUNIOES_2)
REUNIOES_2$MediaSergipe <- mean(REUNIOES_2$Nunca_QuaseNunca)
REUNIOES_2$DiferençaMédia <- REUNIOES_2$Nunca_QuaseNunca - REUNIOES_2$MediaSergipe
