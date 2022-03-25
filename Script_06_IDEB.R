library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(descr)


IDEB <- read_xlsx("dados/IDEB.xlsx")

IDEB$IDEB_2017 <- as.numeric(IDEB$IDEB_2017)
IDEB$IDEB_2019 <- as.numeric(IDEB$IDEB_2019)

# IDEB VÁLIDO 2017 para Malhador e Garuru e 2019 para o restante:

IDEB$IDEB_VALIDO <- IDEB$IDEB_2019
IDEB$IDEB_VALIDO[IDEB$NO_MUNICIPIO=="Gararu"] <- IDEB$IDEB_2017[IDEB$NO_MUNICIPIO=="Gararu"]
IDEB$IDEB_VALIDO[IDEB$NO_MUNICIPIO=="Malhador"] <- IDEB$IDEB_2017[IDEB$NO_MUNICIPIO=="Malhador"]

RANKING <- IDEB %>% 
  arrange(desc(IDEB_VALIDO))

RANKING$Ranking <- paste(row.names(RANKING),"º", sep="")

save(RANKING, file="dados/RANKING.RData")

summary(RANKING$IDEB_VALIDO)

# Distribuição do IDEB em Sergipe - Figura 4:

load("dados/RANKING.RData")

ggplot(RANKING, aes(x=as.factor(IDEB_VALIDO)))+
  geom_dotplot( color= "#29166F", fill = "#29166F")+
  geom_vline(xintercept = 10.76, size = 1,  color= "#F8C301", linetype= "dashed")+
  geom_vline(xintercept = 21, size = 1,  color= "#009B40",  linetype= "dashed")+
  xlab("IDEB")+
  ylab("")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.y=element_blank(),
        axis.line.x = element_line(colour = "#bdbdbd"),
        axis.ticks.y=element_blank()) 

ggsave(filename = "viz/dist_se.eps", width = 14, height = 7, units = "cm")


# IDEB BRASIL - Figura 1

load("dados/IDEB_BRASIL.RData")

# Distribuição no Brasil:

ggplot(data = IDEB_BRASIL, aes(x= IDEB_2019, color = NO_UF == "SE"))+
  geom_density(size = 1)+
  scale_color_manual(name = "", labels = c("Brasil","Sergipe"), values = c("#29166F","#F8C301"))+
  scale_fill_manual(name = "", labels = c("Brasil","Sergipe"), values = c("#29166F","#F8C301"))+
  theme_bw()+
  xlab("Ideb 2019")+
  ylab("Densidade")+
  theme(axis.text.x = element_text(vjust = 0),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "#bdbdbd"),
        axis.line.y = element_line(colour = 'white'),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 13),
        text = element_text(size = 15))+
  guides(color = guide_legend(override.aes = list(collor = c("#29166F","#F8C301"), 
                                                  fill = c("#29166F","#F8C301"))))

ggsave(filename = "viz/dist_br.eps", width = 8, height = 10, units = "cm")



mean(IDEB_BRASIL$IDEB_2019)

# Figura 2

IDEB_MUNICIPAL_BR <- IDEB_BRASIL %>% 
  group_by(NO_UF) %>% 
  summarize(IDEB = mean(IDEB_2019), frequency = n())

ggplot(data= IDEB_MUNICIPAL_BR, aes(x= reorder(NO_UF, -IDEB), y= IDEB, color = NO_UF == "SE", fill = NO_UF == "SE"))+
  scale_colour_manual(values = c("#bdbdbd", "#F8C301"))+
  scale_fill_manual(values = c("#bdbdbd", "#F8C301"))+
  geom_bar(stat="identity", width = 0.5)+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(axis.text.x = element_text(vjust = 0),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "#bdbdbd"))

ggsave(filename = "viz/ranking_br.eps", width = 6, height = 3)


mean(IDEB_MUNICIPAL_BR$IDEB)

sum(IDEB_MUNICIPAL_BR$IDEB*IDEB_MUNICIPAL_BR$frequency)/sum(IDEB_MUNICIPAL_BR$frequency)

# ranking com empates

IDEB$RANKING_EMPATE <- rank(desc(IDEB$IDEB_VALIDO), ties.method = "min")

write_xlsx(IDEB, path = "dados/IDEB_RANKING.xlsx")


