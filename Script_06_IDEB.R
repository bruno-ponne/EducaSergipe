library(readxl)
library(dplyr)
library(ggplot2)

IDEB <- read_xlsx("dados/IDEB.xlsx")

IDEB$IDEB_2017 <- as.numeric(IDEB$IDEB_2017)
IDEB$IDEB_2019 <- as.numeric(IDEB$IDEB_2019)

# IDEB VÁLIDO 2017 para Malhador e Garuru e 2019 para o restante

IDEB$IDEB_VALIDO <- IDEB$IDEB_2019
IDEB$IDEB_VALIDO[IDEB$NO_MUNICIPIO=="Gararu"] <- IDEB$IDEB_2017[IDEB$NO_MUNICIPIO=="Gararu"]
IDEB$IDEB_VALIDO[IDEB$NO_MUNICIPIO=="Malhador"] <- IDEB$IDEB_2017[IDEB$NO_MUNICIPIO=="Malhador"]

RANKING <- IDEB %>% 
  arrange(desc(IDEB_VALIDO))

RANKING$Ranking <- paste(row.names(RANKING),"º", sep="")

ggplot(RANKING, aes(x=IDEB_VALIDO))+
  geom_histogram(color="red3", fill="red3")+
  theme_classic()