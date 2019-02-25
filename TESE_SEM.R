library (installr)
updateR() 
install.packages("tidyverse")


install.packages(knitr)
install.packages(semPlot)
install.packages(tidyverse)
install.packages("psych")
install.packages("lavaan")
install.packages("ggplot2")
install.packages("dplyr")

library(knitr)
library(semPlot)
library(tidyverse)
library(psych)
library(lavaan)
library(ggplot2)
library(dplyr)




#DATA
library(readxl)
CAPITAIS_2006_2016 <- read_excel("ESTUDO/DOUTORADO/TESE/Dados/TESE/CAPITAIS_2006_2016.xlsx", 
                                 sheet = "BAYS_NET_AF_GERAL")



DADOS<- CAPITAIS_2006_2016
DADOS_TRANSF<-log10(DADOS)

plot(DADOS_TRANSF$ANO, DADOS_TRANSF$TEMPMIN ~ DADOS_TRANSF$atilaz)

plot(DADOS$ANO, DADOS_TRANSF$TEMPMIN, xlab="Ano", ylab="log10",col="blue")
lines(DADOS$ANO, DADOS_TRANSF$atilaz,col="red")

dados_SEM<-"
A=~ MORTCRIME + MORTACIDE + POP + POP_FEM + FROTA_100K + FROTA_100K + VEIUC_100K + BUS_100K + OCU_AF_PROP + EMP_AF_PROP 
C=~ INSO_H + PRECIP_VOL + TEMPMAX + TEMPMIN + UMIDREL + UMIDREL_MIN + UMIDREL_MAX
P=~ GINI + PLANSAUDE_100K + PIB_PERCAPITA + INV_ESPLAZ_PCAPT + INV_SAU_PCAPT + COB_AGUA
S=~ EXPC_VIDA + EXPEC_VIDA_H + EXPEC_VIDA_M + ESF + COB_EAB
E=~ CESTA_BASICA + MEIO_SM + MEIO_a_UM_SM + UM_A_DOIS_SM + MIOR_DOIS_SM
I=~ fumante + excpeso + hortareg + frutareg + flvreg + carneg + franpl +gordura + leiteint + refritl5 + saruim + hart + diab
CS =~ inativo + tv_d3
"

dados_SEM.fit <- cfa(dados_SEM, data = DADOS_TRANSF, std.lv = TRUE)
summary(dados_SEM.fit, fit.measure=TRUE, standardized=TRUE, rsquare=TRUE) 
fitMeasures( dados_SEM.fit, fit.measures = c("chisq", "df", "cfi", "tli", "rmsea","srmr"))

semPaths(dados_SEM.fit, whatLabels = "std", edge.label.cex=0.8, node.width = 0.5, layout =  "tree2", rotation =2)

dados_SEM2<-"
A=~ MORTCRIME + MORTACIDE + POP + POP_FEM + FROTA_100K + FROTA_100K + VEIUC_100K + BUS_100K + OCU_AF_PROP + EMP_AF_PROP 
C=~ INSO_H + PRECIP_VOL + TEMPMAX + TEMPMIN + UMIDREL + UMIDREL_MIN + UMIDREL_MAX
P=~ GINI + PLANSAUDE_100K + PIB_PERCAPITA + INV_ESPLAZ_PCAPT + INV_SAU_PCAPT + COB_AGUA
S=~ EXPC_VIDA + EXPEC_VIDA_H + EXPEC_VIDA_M + ESF + COB_EAB
E=~ CESTA_BASICA + MEIO_SM + MEIO_a_UM_SM + UM_A_DOIS_SM + MIOR_DOIS_SM
I=~ fumante + excpeso + hortareg + frutareg + flvreg + carneg + franpl +gordura + leiteint + refritl5 + saruim + hart + diab
CS =~ inativo + tv_d3
CS ~ A + C + P + S + E + I
"





dados_SEM.fit2 <- sem(dados_SEM3, std.lv = TRUE)
summary(dados_SEM.fit2 , fit.measure=TRUE, standardized=TRUE, rsquare=TRUE)
fitMeasures( dados_SEM.fit2, fit.measures = c("chisq", "df", "cfi", "tli", "rmsea","srmr"))

semPaths(DADOS, whatLabels = "std", edge.label.cex=0.8, node.width = 0.5, layout =  "tree2", rotation =2)
