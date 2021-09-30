# Script 01 - Carregando os dados

# Os dados aqui carregados são provenientes de duas fontes:

# 1) SAEB 2019, disponível no site do Ministério da Educação, 
# Arquivo 'TS_ALUNO_5EF.csv' referente aos alunos do 5º ano do Ensino Fundamental
# Fonte: https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/saeb

# 2) Censo escolar 2020
# Arquivo 'docentes_nordeste.csv' e 'escolas.csv'
# Fonte: https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/censo-escolar/resultados

#Bibliotecas necessárias:
library(dplyr)
library(readr)

# Nomes dos municípios sergipanos

cod_mun_SE <- read_delim("dados/cod_mun_SE.csv", ";", escape_double = FALSE, trim_ws = TRUE)

names(cod_mun_SE) <- c("ID_MUNICIPIO", "NO_MUNICIPIO")
cod_mun_SE$ID_MUNICIPIO <- as.numeric(cod_mun_SE$ID_MUNICIPIO)


# SAEB

ALUNO_5EF <- data.table::fread(input='TS_ALUNO_5EF.csv',integer64='character')

# Somente escolas municipais de Sergipe:

SERGIPE_COMP <- filter(ALUNO_5EF, ID_UF==28, ID_DEPENDENCIA_ADM==3)

# Identificando observações referentes a municípios "anonimizados":
# Alguns municípios não tem seus desempenhos divulgados por conta das regulamentações atuais.
# Esses municípios "fictícios" tem seu código iniciado em 6 e serão excluídos desta análise por não possibilitarem a identificação do município.
# Mai detalhes: SAEB 2109 - Leia-me.

SERGIPE_COMP$ANONIMIZACAO <- substr(SERGIPE_COMP$ID_MUNICIPIO, 1,1)

# Contabilizando perda de dados - 2449 de 22704 obs (10,8%)

SERGIPE_COMP %>% group_by(ANONIMIZACAO) %>% summarise(n())

SERGIPE <- filter(SERGIPE_COMP, ANONIMIZACAO==2)

# Selecionando variáveis de interesse:
# TX_RESP_Q006E - INDICADOR 1: FREQUÊNCIA DOS PAIS A REUNIÕES
# TX_RESP_Q010B - INDICADOR 2: REDE WI-FI
# TX_RESP_Q017D - INDICADOR 3: TEMPO DESTINADO AO ESTUDO

SERGIPE_ALUNO <- select(SERGIPE,
                  TX_RESP_Q006E,
                  TX_RESP_Q017D,
                  TX_RESP_Q010B,
                  PROFICIENCIA_MT_SAEB, 
                  PROFICIENCIA_LP_SAEB, 
                  ID_MUNICIPIO, 
                  ID_DEPENDENCIA_ADM,
                  ID_UF)

# Adicionando nomes dos municípios

SERGIPE_ALUNO <- left_join(SERGIPE_ALUNO, cod_mun_SE, by = "ID_MUNICIPIO")


save(SERGIPE_ALUNO, file = "dados/SERGIPE_ALUNO.Rdata")

# CENSO ESCOLAR

# Escola:

ESCOLAS <- read.csv2("escolas.csv", sep = "|")

# Filtrando apenas escolas municipais de Sergipe:

ESCOLAS_SE <- filter(ESCOLAS, CO_UF == 28, TP_DEPENDENCIA == 3)

names(cod_mun_SE) <- c("CO_MUNICIPIO", "NO_MUNICIPIO")
ESCOLAS_SE <- left_join(ESCOLAS_SE, cod_mun_SE, by = "CO_MUNICIPIO")

save(ESCOLAS_SE, file = "dados/ESCOLAS_SE.Rdata")

# Professores NE:

PROFESSORES <- read.csv2("docentes_nordeste.csv", sep = "|")

PROF_SE <- PROFESSORES %>% 
  filter(CO_UF == 28, TP_DEPENDENCIA == 3)

PROF_SE <- left_join(PROF_SE, cod_mun_SE, by = "CO_MUNICIPIO")

save(PROF_SE, file = "dados/PROF_SE.Rdata")










