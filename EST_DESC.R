library(PNADcIBGE)
library(dplyr)
library(tidyverse)
library(srvyr)
library(survey)
library(convey)
library(magrittr)
library(rio)
library(ggplot2)
library(sf)
library(geobr)
library(readr)
library(extrafont)

#Trabalhando com os dados da PNAD Continua 2019
#O objetivo desse projeto é entender a realidade das crianças brasileiras, 
#pensando especificamente nas discrepâncias regionais quanto a pobreza e extrema
#pobreza

#Utilizara do indice de FGT para descobrir o PO, P1 e P3

#Configurações
options(survey.lonely.psu = "adjust") 
options(OutDec=",")
windowsFonts(Times=windowsFont("Times New Roman"))


#Importando os dados 
dados <-read_pnadc("PNADC_2019_visita1.txt", "input_PNADC_2019_visita1.txt")

#Selecionado as variaveis que serao utilizadas
D19 <- dados %>%
  select("Ano", "UF","Trimestre", "UPA", "Estrato", "V1008", "V1014", "V1022", "V1030", "V1031", "V1032", "posest", 
         "V2001", "V2003", "V2005", "V2007", "V2009", "V2010", "VD2002", "VD2004", "VD3004", "VD4001",
         "VD5012", "VD5011")

#Diversos trabalhos da literatura sobre pobreza infantil indicam que o 
#sexo e a escolaridade da referencia domiciliar influenciam na probabilidade
#da crianca ser pobre ou não. Como faz parte do tratamento da base considerar
#apenas as crianças, para captar essas duas variaveis sera necessario espelhar
#essas informações para a criança. 

#Para isso, necessita-se descobrir as pessoas que estão no mesmo domicilio
D19$grupos<-paste(D19$UPA, D19$V1008, D19$V1014)
grupos<- unique(D19$grupos)
grupos<-as.data.frame(grupos)
grupos$numero = 1:nrow(grupos)
D19 <- inner_join(grupos, D19, by = "grupos")

#Diante a informação acima, descobre-se a escolaridade: 
resp <- D19[which(as.numeric(D19$V2005) == 1),c(1,23)]
colnames(resp)[2] <- "ESC_REF"
D19 <- inner_join (resp, D19, by = "grupos")

#Descobrindo o sexo da pessoa de referência:
ref= D19[which(as.numeric(D19$V2005) == 1),c(1,19)]
colnames(ref)[2] <- "SEXO_REF"
D19 <- inner_join (ref, D19, by = "grupos")

#Descobrindo se o domicílio tem crianças e transferindo essa informação
#para todas as pessoas da casa:
D19 %<>%
  mutate(id_menor = ifelse(V2009 <= 13, 1,0))

D19 %<>%
  group_by(grupos) %>%
  mutate(id_menor = sum(id_menor, na.rm = T)) %>%
  ungroup()

D19 %<>% mutate(id_menor = if_else(id_menor>=1, 1,0))

#Anexando a base que tem informações sobre o deflacionamento 
deflator <- import("deflator_example.xls")

#Criando chave para unir as duas bases:
#Para PNAD_deflator
deflator$chave <- paste0(deflator$ano, deflator$trim, deflator$uf)

#Selecionando as variaveis realmente importantes 
deflator %<>% filter(ano == 2019) %>%
  select(chave, CO1)

#Criando chave para D19: 
D19$chave <- paste0(D19$Ano, D19$Trimestre, D19$UF)

#Unindo as bases:
D19 <- inner_join(D19, deflator, by = "chave")

#Descobrindo o valor deflacionado da variavel de rendimento efetivo
D19$REND_EF <- D19$VD5011*D19$CO1

#Declarando o conjunto de dados como sendo de amostragem complexa
D19 %<>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE)

#Considerando as regiões, variavel para calcular o total da populacao e
#se o individuo é extremamente pobre ou pobre: 
D19 %<>% mutate (one = 1,
                 REGIAO = case_when(
                   UF %in% 11:17 ~ "1",
                   UF %in% 21:29 ~ "2",
                   UF %in% 31:35 ~ "3",
                   UF %in% 41:43 ~ "4",
                   UF %in% 50:53 ~ "5"),
                 POBRE = if_else(VD5012 ==  1 | VD5012 ==  2 , 1, 0),
                 EXT_POBRE = if_else(VD5012 ==  1, 1,0))

#Dummy para indicar se é branco ou caso contrário: 
#Como branco foram incluidos os individuos auto-declarado branco e amarelo
# + Níveis de escolaridade
# + Situação do domicílio
D19 %<>% mutate(COR_RACA = if_else(V2010 ==1 | V2010 == 3, "1","0"), 
                ESC = case_when(ESC_REF == "1"| ESC_REF == "2" ~ "1",
                                ESC_REF == "3"| ESC_REF == "4" ~ "2",
                                ESC_REF == "5" ~ "3",
                                ESC_REF == "6"| ESC_REF == "7" ~ "4"),
                URB_RUR = if_else(V1022 == 1, "1", "0"))

#Calculanado algumas estatísticas descritivas para as crianças
#que são pobres e extremamente pobres: 
#Filtrando quem é criança e pobre 

cri_pb <- D19 %>% subset(V2009 <= 13 & POBRE == 1)

cri_ext <- D19 %>% subset(V2009 <= 13 & EXT_POBRE == 1)

#Para calcular o total de observações pelo Brasil e por região:
svytotal(~one, cri_pb, na.rm = T)

svytotal(~REGIAO, cri_pb, na.rm = T)

#Quantas informações sem o peso amostral
s_peso <- cri_pb$variables
#Total: 45.113
s_peso %>% filter(REGIAO == 5) %>%
  summarise(soma = sum(one))
#Norte: 9436, Nordeste = 22.624, Sudeste = 7.231, Sul = 3.155, 
#Centro-Oeste = 2.66

#Algumas estatísticas para o Brasil relativo a pobrexa e extrema pobreza:

#Proporção de crianças por sexo
svymean(~V2007, cri_pb, na.rm = T)
svymean(~V2007, cri_ext, na.rm = T)

#Proporção da referencia do domicilio por sexo
svymean(~SEXO_REF, cri_pb, na.rm = T)
svymean(~SEXO_REF, cri_ext, na.rm = T)

#Proporção de crianças brancas
svymean(~COR_RACA, cri_pb, na.rm = T)
svymean(~COR_RACA, cri_ext, na.rm = T)

#Nível de escolaridade dos pais:
svymean(~ESC, cri_pb, na.rm = T)
svymean(~ESC, cri_ext, na.rm = T)

#Situação domiciliar
svymean(~URB_RUR, cri_pb, na.rm = T)
svymean(~URB_RUR, cri_ext, na.rm = T)

#Média de quantidade por pessoas:
#Para essa variavel o tratamtento tem que ser diferente:
#Como na base crianças pobres podem ter varias crianças que estão 
#na mesma casa. Será filtrar uma base que tem apenas uma informação de cada casa
#Como a referencia da casa é apenas uma pessoa, o filtro sera com base nesse
#individuo. Vale lembrar que a informacao quantidade de pessoas no domicilio
#é a mesma para todos da casa. 
#Filtrando a referencia da casa e quem é pobre: 
ref <- D19 %>% subset(V2005 == "01" & POBRE == 1 & id_menor == 1)
ref <- D19 %>% subset(V2005 == "01" & EXT_POBRE == 1 & id_menor == 1)

svymean(~V2001, ref, na.rm = T)

#Proporção de crianças por sexo em cada região
svyby(~V2007, ~REGIAO, cri_pb, svymean, na.rm = T)

#Proporção da referencia do domicilio por sexo em cada região
svyby(~SEXO_REF, ~REGIAO, cri_pb, svymean, na.rm = T)

#Proporção de crianças brancas em cada região
svyby(~COR_RACA, ~REGIAO, cri_pb, svymean, na.rm = T)

#Proporção de crianças de acordo com o nível de escolaridade dos pais:
svyby(~ESC, ~REGIAO, cri_pb, svymean, na.rm = T)

#Proporção de crianças de acordo com a situação domiciliar:
svyby(~URB_RUR, ~REGIAO, cri_pb, svymean, na.rm = T)

#Média de pessoas no domicílio
svyby(~V2001,~REGIAO, ref, svymean, na.rm = T)


#Agora fazendo os passos anteriores para as crianças extremamente pobres:
#Total por regiões
total <- svytotal(~REGIAO, cri_pb, na.rm = T)

#Meninas
svyby(~V2007, ~REGIAO, cri_ext, svymean, na.rm = T)

#Mulher como ref.
svyby(~SEXO_REF, ~REGIAO, cri_ext, svymean, na.rm = T)

#Cor_Raça
svyby(~COR_RACA, ~REGIAO,cri_ext, svymean, na.rm = T)

#Escolaridadee
svyby(~ESC, ~REGIAO,cri_ext, svymean, na.rm = T)

#Urbano 
svyby(~URB_RUR, ~REGIAO, cri_ext, svymean, na.rm = T)

#Filtrando a referencia da casa e quem é extremamente pobre: 
ref <- D19 %>% subset(V2005 == "01" & EXT_POBRE == 1 & id_menor == 1)

svyby(~V2001,~REGIAO, ref, svymean, na.rm = T)

#Utilizando o pacote convey para estimar os índices de FGT: P0, P1 e P2:
#Preparando a base da PNAD considerando o convey

#Considerando na amostra apenas as crianças: 
FGT <- convey_prep(D19)

#Filtrando as crianças:
cri <- FGT %>% subset(V2009 <= 13)

#Para pobres:
svyfgt(~REND_EF, cri, g=0, abs_thresh = 499, na.rm = TRUE)
svyfgt(~REND_EF, cri, g=1, abs_thresh = 499, na.rm = TRUE)
svyfgt(~REND_EF, cri, g=2, abs_thresh = 499, na.rm = TRUE)

#Para extremamentes pobres:
svyfgt(~REND_EF, cri, g=0, abs_thresh = 249.50, na.rm = TRUE)
svyfgt(~REND_EF, cri, g=1, abs_thresh = 249.50, na.rm = TRUE)
svyfgt(~REND_EF, cri, g=2, abs_thresh = 249.50, na.rm = TRUE)


#Agora em relação os indices de FGT para as regiões, iremos construir um mapa: 
reg_cri <- FGT %>% subset(V2009 <= 13 & REGIAO == 1)
reg_cri <- FGT %>% subset(V2009 <= 13 & REGIAO == 2)
reg_cri <- FGT %>% subset(V2009 <= 13 & REGIAO == 3)
reg_cri <- FGT %>% subset(V2009 <= 13 & REGIAO == 4)
reg_cri <- FGT %>% subset(V2009 <= 13 & REGIAO == 5)

#PRIMEIRAMENTE PARA POBRES: 
svyfgt(~REND_EF, reg_cri, g=0, abs_thresh = 499, na.rm = TRUE)
svyfgt(~REND_EF, reg_cri, g=1, abs_thresh = 499, na.rm = TRUE)
svyfgt(~REND_EF, reg_cri, g=2, abs_thresh = 499, na.rm = TRUE)


REG <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro Oeste")


P0 <- c(0.64291,0.6754,0.33557,0.25932,0.32141)

P1 <- c(0.33082,0.35667,0.1395,0.097997,0.12408)

P2 <- c(0.21204,0.23693,0.081627,0.054934, 0.069761)

dat_P0 <- data.frame(REG, P0)
dat_P1 <- data.frame(REG, P1)
dat_P2 <- data.frame(REG, P2)

#Baixando os dados para as regiões brasileiras
regiao <- read_region(year=2019)

#Construindo o mapa P0:
dat_P0 %<>% 
  mutate(P0 = 100*P0,
         P0 = round(P0, digits = 1),
         P0 = paste0(P0, "%"))

regP0 <- left_join(regiao, dat_P0, by = c("name_region" = "REG"))

group.colors <- c("67,5%" = "#006d2c", "64,3%" = "#31a354", "33,6%" = "#74c476",
                  "32,1%" = "#bae4b3"  , "25,9%" = "#edf8e9")

ggplot() +
  geom_sf(data=regP0, aes(fill=as.factor(P0)), color = NA,size=.15) + 
  scale_fill_manual(name = "P0", values= group.colors,
                    breaks = c("67,5%", "64,3%", "33,6%","32,1%","25,9%"),
                    labels = c("67,5%", "64,3%", "33,6%","32,1%","25,9%")) +
  theme_void (base_size = 11, base_family = "Times")


#Construindo o mapa P1:
dat_P1 %<>% 
  mutate(P1 = 100*P1,
         P1 = round(P1, digits = 1),
         P1 = paste0(P1, "%"))

regP1 <- left_join(regiao, dat_P1, by = c("name_region" = "REG"))

regP1$P1

group.colors <- c("35,7%" = "#993504", "33,1%" = "#d95f0e", "14%" = "#fe9929",
                  "9,8%" = "#ffffd4"  , "12,4%" = "#fed98e")

ggplot() +
  geom_sf(data=regP1, aes(fill=as.factor(P1)), color = NA,size=.15) + 
  scale_fill_manual(name = "P1", values= group.colors,
                    breaks = c("35,7%", "33,1%", "14%","12,4%","9,8%"),
                    labels = c("35,7%", "33,1%", "14,0%","12,4%","9,8%")) +
  theme_void (base_size = 11, base_family = "Times")


#Construindo o mapa P2:
dat_P2 %<>% 
  mutate(P2 = 100*P2,
         P2 = round(P2, digits = 1),
         P2 = paste0(P2, "%"))

regP2 <- left_join(regiao, dat_P2, by = c("name_region" = "REG"))

regP2$P2

group.colors <- c("23,7%" = "#a50f15", "21,2%" = "#de2d26", "8,2%" = "#fb6a4a",
                  "7%" = "#fcae91"  , "5,5%" = "#fee5d9")
ggplot() +
  geom_sf(data=regP2, aes(fill=as.factor(P2)), color = NA,size=.15) + 
  scale_fill_manual(name = "P2", values= group.colors,
                    breaks = c("23,7%", "21,2%", "8,2%","7%","5,5%"),
                    labels = c("23,7%", "21,2%", "8,2%","7,0%","5,5%")) +
  theme_void (base_size = 11, base_family = "Times")

#Construindo mapa para extremamente pobres: 
reg_cri <- FGT %>% subset(V2009 <= 13 & REGIAO == 1)
reg_cri <- FGT %>% subset(V2009 <= 13 & REGIAO == 2)
reg_cri <- FGT %>% subset(V2009 <= 13 & REGIAO == 3)
reg_cri <- FGT %>% subset(V2009 <= 13 & REGIAO == 4)
reg_cri <- FGT %>% subset(V2009 <= 13 & REGIAO == 5)


svyfgt(~REND_EF,reg_cri, g=0, abs_thresh = 249.50, na.rm = TRUE)
svyfgt(~REND_EF,reg_cri, g=1, abs_thresh = 249.50, na.rm = TRUE)
svyfgt(~REND_EF,reg_cri, g=2, abs_thresh = 249.50, na.rm = TRUE)


P0 <- c(0.33733,0.36581,0.11555,0.073738,0.093403)

P1 <- c(0.14805,0.17372,0.050946,0.033032,0.041965)

P2 <- c(0.088392,0.11037,0.03415,0.022342,0.027848)

dat_P0 <- data.frame(REG, P0)
dat_P1 <- data.frame(REG, P1)
dat_P2 <- data.frame(REG, P2)


#Construindo o mapa P0:
dat_P0 %<>% 
  mutate(P0 = 100*P0,
         P0 = round(P0, digits = 1),
         P0 = paste0(P0, "%"))

regP0 <- left_join(regiao, dat_P0, by = c("name_region" = "REG"))

regP0$P0

group.colors <- c("36,6%" = "#993404", "33,7%" = "#d95f0e", "11,6%" = "#fb6a4a",
                  "9,3%" = "#fcae91"  , "7,4%" = "#fee5d9")

ggplot() +
  geom_sf(data=regP0, aes(fill=as.factor(P0)), color = NA,size=.15) + 
  scale_fill_manual(name = "P0", values= group.colors,
                    breaks = c("36,6%", "33,7%", "11,6%","9,3%","7,4%"),
                    labels = c("36,6%", "33,7%", "11,6%","9,3%","7,4%")) +
  theme_void (base_size = 11, base_family = "Times")


#Construindo o mapa P1:
dat_P1 %<>% 
  mutate(P1 = 100*P1,
         P1 = round(P1, digits = 1),
         P1 = paste0(P1, "%"))

regP1 <- left_join(regiao, dat_P1, by = c("name_region" = "REG"))

regP1$P1

group.colors <- c("17,4%" = "#bd0026", "14,8%" = "#f03b20", "5,1%" = "#fd8d3c",
                  "4,2%" = "#fccc5c"  , "3,3%" = "#ffffb2")

ggplot() +
  geom_sf(data=regP1, aes(fill=as.factor(P1)), color = NA,size=.15) + 
  scale_fill_manual(name = "P1", values= group.colors,
                    breaks = c("17,4%", "14,8%", "5,1%","4,2%" ,"3,3%"),
                    labels = c("17,4%", "14,8%", "5,1%","4,2%" ,"3,3%")) +
  theme_void (base_size = 11, base_family = "Times")


#Construindo o mapa P2:
dat_P2 %<>% 
  mutate(P2 = 100*P2,
         P2 = round(P2, digits = 1),
         P2 = paste0(P2, "%"))

regP2 <- left_join(regiao, dat_P2, by = c("name_region" = "REG"))

regP2$P2

group.colors <- c("11%" = "#404040", "8,8%" = "#bababa", "3,4%" = "#fb6a4a",
                  "2,8%" = "#f4a582"  , "2,2%" = "#bd0026")

ggplot() +
  geom_sf(data=regP2, aes(fill=as.factor(P2)), color = NA,size=.15) + 
  scale_fill_manual(name = "P2", values= group.colors,
                    breaks = c("11%", "8,8%", "3,4%","2,8%","2,2%"),
                    labels = c("11,0%", "8,8%", "3,4%","2,8%","2,2%")) +
  theme_void (base_size = 11, base_family = "Times")


















