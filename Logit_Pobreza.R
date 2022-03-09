#Continuacao do arquivo Pobreza_FGT
library(car)
library(erer)
library(questionr)


#Fazendo um logit para medir as razoes de chance da crianca ser pobre: 
#Para o Brasil e para as regioes:

#Para o Brasil e considerando o nordeste como 1:
#Renomeando algumas variaveis 
D19 %<>% mutate (one = 1,
                 REGIAO = case_when(
                   UF %in% 11:17 ~ "2",
                   UF %in% 21:29 ~ "1",
                   UF %in% 31:35 ~ "3",
                   UF %in% 41:43 ~ "4",
                   UF %in% 50:53 ~ "5"),
                 POBRE = if_else(REND_EF < 499, 1, 0),
                 EXT_POBRE = if_else(REND_EF < 249.5, 1,0))

D19 %<>% mutate(IDADE = V2009, 
                SEXO_REF = if_else(SEXO_REF == 1, "0", "1"),
                CRI_SEXO = if_else(V2007 == 1, "0", "1"), 
                QTDE_DOM = V2001)

D19 %<>% filter(IDADE <= 13)

logitP <- svyglm(POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR + REGIAO, D19, family = "binomial", x=TRUE,y=TRUE)

#Outras formas de visualizar os coeficientes: 
#Efeitos marginais
maBina(w=logitP,digits=20)

#Estimando Razao de chances
odds.ratio(logitP)

#Testes econometricos
#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhanca
logitPfit <- svyglm(POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)

#Logit Norte
D_N <- D19 %>% filter(REGIAO == 2)

logitP <- svyglm(POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR , D_N, family = "binomial", x=TRUE,y=TRUE)


#Estimando Razao de chances
odds.ratio(logitP)

#Testes econometricos

#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhança
logitPfit <- svyglm(POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)

###################################
#Logit Nordeste
D_NE <- D19 %>% filter(REGIAO == 1)

logitP <- svyglm(POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR , D_NE, family = "binomial", x=TRUE,y=TRUE)

#Estimando Razao de chances
odds.ratio(logitP)

#Testes econometricos
#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhanca
logitPfit <- svyglm(POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)

######################################
#Logit Sudeste
D_SE <- D19 %>% filter(REGIAO == 3)

logitP <- svyglm(POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR ,D_SE, family = "binomial", x=TRUE,y=TRUE)

#Estimando Razao de chances
odds.ratio(logitP)

#Testes econométricos
#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhanca
logitPfit <- svyglm(POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   #TESTANDO QUAL MODELO e melhor atraves do anova
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)

###############################################
#Logit Sul 
D_S <- D19 %>% filter(REGIAO == 4)

logitP <- svyglm(POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR ,D_S, family = "binomial", x=TRUE,y=TRUE)

#Estimando Razao de chances
odds.ratio(logitP)

#Testes econometricos
#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhanca
logitPfit <- svyglm(POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)


#################################################
D_CO <- D19 %>% filter(REGIAO == 5)

logitP <- svyglm(POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR ,D_CO, family = "binomial", x=TRUE,y=TRUE)

#Estimando Razao de chances
odds.ratio(logitP)

#Testes econometricos
#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhanca
logitPfit <- svyglm(POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   #TESTANDO QUAL MODELO é MELHOR através do anova
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)

######################################################################
#Extrema Pobreza 
#Para o Brasil 
logitP <- svyglm(EXT_POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR + REGIAO, D19, family = "binomial", x=TRUE,y=TRUE)

#Estimando Razao de chances
odds.ratio(logitP)

#Testes econometricos
#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhanca
logitPfit <- svyglm(EXT_POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   #TESTANDO QUAL MODELO é MELHOR através do anova
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)

#Logit Nordeste
D_NE <- D19 %>% filter(REGIAO == 1)

logitP <- svyglm(EXT_POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR , D_NE, family = "binomial", x=TRUE,y=TRUE)

#Estimando Razao de chances
odds.ratio(logitP)

#Testes econometricos
#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhaca
logitPfit <- svyglm(EXT_POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   #TESTANDO QUAL MODELO e MELHOR atraves do anova
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)

#Logit Norte
D_N <- D19 %>% filter(REGIAO == 2)

logitP <- svyglm(EXT_POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR , D_N, family = "binomial", x=TRUE,y=TRUE)

#Estimando Razao de chances
odds.ratio(logitP)

#Testes econometricos
#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhanca
logitPfit <- svyglm(EXT_POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)

#########################################
#Logit Sudeste
D_SE <- D19 %>% filter(REGIAO == 3)

logitP <- svyglm(EXT_POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR ,D_SE, family = "binomial", x=TRUE,y=TRUE)

#Estimando Razao de chances
odds.ratio(logitP)
#Testes econometricos
#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhanca
logitPfit <- svyglm(EXT_POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)

################################
#Logit Sul 
D_S <- D19 %>% filter(REGIAO == 4)

logitP <- svyglm(EXT_POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR ,D_S, family = "binomial", x=TRUE,y=TRUE)

#Estimando Razao de chances
odds.ratio(logitP)

#Testes econometricos
#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhanca
logitPfit <- svyglm(EXT_POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)

##########
#Logit Centro-Oeste
D_CO <- D19 %>% filter(REGIAO == 5)

logitP <- svyglm(EXT_POBRE ~ CRI_SEXO + COR_RACA + ESC + SEXO_REF +
                   IDADE + QTDE_DOM + URB_RUR ,D_CO, family = "binomial", x=TRUE,y=TRUE)

#Estimando Razao de chances
odds.ratio(logitP)

#Testes econometricos
#teste de heterocedasticidade
bptest(logitP)
#Reestimando sem heterocedasticidade
coeftest(logitP)
#teste de verossimilhanca
logitPfit <- svyglm(EXT_POBRE ~ CRI_SEXO, D19, family = "binomial", x=TRUE,y=TRUE)
anova(logitP, logitPfit, test ="Chisq")   
#R2
psrsq(logitP, type="Nagelkerke")
#Teste de multicolinearidade VIF
vif(logitP)
