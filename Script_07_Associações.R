# Script 07: Associação entre os indicadores 1 e 2 e o desempenho no SAEB

library(dplyr)
library(readr)
library(descr)
library(ggplot2)



load("dados/ASSOCIACOES.RData")

freq(ASSOCIACOES$TX_RESP_Q006E)

REUNIOES <- select(ASSOCIACOES, PROFICIENCIA_LP_SAEB, PROFICIENCIA_MT_SAEB, TX_RESP_Q006E) %>% na.omit()

freq(REUNIOES$TX_RESP_Q006E)

REUNIOES$TX_RESP_Q006E[REUNIOES$TX_RESP_Q006E=="."]<-NA

REUNIOES$TX_RESP_Q006E[REUNIOES$TX_RESP_Q006E=="*"]<-NA

REUNIOES <- REUNIOES %>% na.omit() # 27,4%

grupos_reuniao <- REUNIOES

grupos_reuniao$TX_RESP_Q006E[grupos_reuniao$TX_RESP_Q006E=="A"]<- "A"
grupos_reuniao$TX_RESP_Q006E[grupos_reuniao$TX_RESP_Q006E=="B"]<- "B"
grupos_reuniao$TX_RESP_Q006E[grupos_reuniao$TX_RESP_Q006E=="C"]<- "B"

grupoA <- filter(grupos_reuniao, TX_RESP_Q006E == "A" )
grupoB <- filter(grupos_reuniao, TX_RESP_Q006E == "B" )

grupo <- c("Nunca/Quase Nunca", "Maior frequência","Nunca/Quase Nunca", "Maior frequência")
disciplina <- c("Matemática","Matemática", "Português", "Português")
media <- c(mean(grupoA$PROFICIENCIA_MT_SAEB), mean(grupoB$PROFICIENCIA_MT_SAEB),
           mean(grupoA$PROFICIENCIA_LP_SAEB), mean(grupoB$PROFICIENCIA_LP_SAEB))


# se mat

n_sample_A_m <- length(grupoA$PROFICIENCIA_MT_SAEB)
n_sample_B_m <- length(grupoB$PROFICIENCIA_MT_SAEB)

sd_sample_A_m <- sd(grupoA$PROFICIENCIA_MT_SAEB)
sd_sample_B_m <- sd(grupoB$PROFICIENCIA_MT_SAEB)

se_sample_A_m <- sd_sample_A_m/sqrt(n_sample_A_m)
se_sample_B_m <- sd_sample_B_m/sqrt(n_sample_B_m)



# se port
n_sample_A_p <- length(grupoA$PROFICIENCIA_LP_SAEB)
n_sample_B_p <- length(grupoB$PROFICIENCIA_LP_SAEB)

sd_sample_A_p <- sd(grupoA$PROFICIENCIA_LP_SAEB)
sd_sample_B_p <- sd(grupoB$PROFICIENCIA_LP_SAEB)

se_sample_A_p <- sd_sample_A_p/sqrt(n_sample_A_p)
se_sample_B_p <- sd_sample_B_p/sqrt(n_sample_B_p)

se <- c(se_sample_A_m,se_sample_B_m,se_sample_A_p,se_sample_B_p)

DATA <- data.frame(grupo, disciplina, media, se)

DATA <- mutate(DATA, Conf_Low = media -1.96*se, Conf_High = media +1.96*se )

ggplot(DATA)+
  geom_linerange(aes(x = grupo, 
                     ymin = Conf_Low,
                     ymax = Conf_High),
                 color = "#bdbdbd",
                 lwd = 1.5) +
  geom_point(aes(x = grupo, 
                 y = media,
                 color = grupo), size = 2.5)+
  scale_color_manual(name = "", values=c("#29166F","#F8C301"))+
  ylab("Pontuação Saeb")+
  xlab("")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white", linetype = "blank"),
        strip.text = element_text(colour = 'black'),
        axis.line = element_line(colour = "#bdbdbd"),
        panel.border = element_rect(colour = "#bdbdbd"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        panel.spacing = unit(0.5, "cm"))+
  coord_flip()+
  facet_grid(,vars(disciplina), scales="free")

ggsave(filename = "viz/reunioes.eps", width = 4.5, height = 2)


## REDE WI FI

freq(SERGIPE_COMP$TX_RESP_Q010B)

WIFI <- select(ASSOCIACOES, PROFICIENCIA_LP_SAEB, PROFICIENCIA_MT_SAEB, TX_RESP_Q010B) %>% na.omit()

freq(WIFI$TX_RESP_Q010B)

WIFI$TX_RESP_Q010B[WIFI$TX_RESP_Q010B=="."]<-NA

WIFI$TX_RESP_Q010B[WIFI$TX_RESP_Q010B=="*"]<-NA

WIFI <- na.omit(WIFI)

wifi_a <- filter(WIFI, TX_RESP_Q010B == "A" )
wifi_b <- filter(WIFI, TX_RESP_Q010B == "B" )

t.test(wifi_a$PROFICIENCIA_LP_SAEB, wifi_b$PROFICIENCIA_LP_SAEB, var.equal = FALSE)

wifi_group <- c("Não", "Sim","Não", "Sim")
disciplina <- c("Matemática","Matemática", "Português", "Português")
wifi_media <- c(mean(wifi_a$PROFICIENCIA_MT_SAEB), mean(wifi_b$PROFICIENCIA_MT_SAEB),
           mean(wifi_a$PROFICIENCIA_LP_SAEB), mean(wifi_b$PROFICIENCIA_LP_SAEB))


# se mat - wifi

n_wifi_A_m <- length(wifi_a$PROFICIENCIA_MT_SAEB)
n_wifi_B_m <- length(wifi_b$PROFICIENCIA_MT_SAEB)

sd_wifi_A_m <- sd(wifi_a$PROFICIENCIA_MT_SAEB)
sd_wifi_B_m <- sd(wifi_b$PROFICIENCIA_MT_SAEB)

se_wifi_A_m <- sd_wifi_A_m/sqrt(n_wifi_A_m )
se_wifi_B_m <- sd_wifi_B_m/sqrt(n_wifi_B_m )

# se port - wifi

n_wifi_A_p <- length(wifi_a$PROFICIENCIA_LP_SAEB)
n_wifi_B_p <- length(wifi_b$PROFICIENCIA_LP_SAEB)

sd_wifi_A_p <- sd(wifi_a$PROFICIENCIA_LP_SAEB)
sd_wifi_B_p <- sd(wifi_b$PROFICIENCIA_LP_SAEB)

se_wifi_A_p <- sd_wifi_A_p/sqrt(n_wifi_A_p )
se_wifi_B_p <- sd_wifi_B_p/sqrt(n_wifi_B_p )

se_wifi <- c(se_wifi_A_m, se_wifi_B_m, se_wifi_A_p, se_wifi_B_p)

DATA_WIFI <- data.frame(wifi_group, disciplina, wifi_media, se_wifi)

DATA_WIFI <- mutate(DATA_WIFI, Conf_Low = wifi_media -1.96*se_wifi, Conf_High = wifi_media +1.96*se_wifi )

ggplot(DATA_WIFI)+
  geom_linerange(aes(x = wifi_group, 
                     ymin = Conf_Low,
                     ymax = Conf_High),
                 color = "#bdbdbd",
                 lwd = 1.5) +
  geom_point(aes(x = wifi_group, 
                 y = wifi_media,
                 color = wifi_group), size = 3)+
  scale_color_manual(name = "", values=c("#F8C301","#29166F"))+
  ylab("Pontuação Saeb")+
  xlab("")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white", linetype = "blank"),
        strip.text = element_text(colour = 'black'),
        axis.line = element_line(colour = "#bdbdbd"),
        panel.border = element_rect(colour = "#bdbdbd"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        panel.spacing = unit(0.5, "cm"))+
  coord_flip()+
  facet_grid(,vars(disciplina), scales="free")


ggsave(filename = "viz/wifi.eps" , width = 4.5, height = 2)

