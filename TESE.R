
#### BAYESIAN NETWORK ######

#NET FROM DATABASE THESIS
#CONTINUOUS DATA (TYPE NUM)
#CRIATE FOR TIMOTHY GUSTAVO CAVAZZOTTO AT 14/01/2019


# PRE-PRECESSING ####

##### Load packages
library(bnlearn)
library(data.table)
library(readxl)
library(Rgraphviz)
library(ROCR)


#open Big DATA file 

dados<-fread("ESTUDO/DOUTORADO/TESE/Dados/TESE/BN_INDV.csv", dec = ",")



# Open XLSX Files

CAPITAIS_2006_2016_TX <- read_excel("ESTUDO/DOUTORADO/TESE/Dados/TESE/BN_INDV.xlsx")

DADOS_TX<- CAPITAIS_2006_2016_TX


#SELECT DATE AND BUILD NETWORK
bn_df <- data.frame(dados)
bn_df[] <- lapply(bn_df, function(x) { 
  if(is.integer(x)) as.numeric(x) else x
}) # all num

str(bn_df)

bn_df$ano<-as.factor(bn_df$ano)
bn_df$mesfim<-as.factor(bn_df$mesfim)
bn_df$regiao<-as.factor(bn_df$regiao)
bn_df$sexo<-as.factor(bn_df$sexo)
bn_df$civil<-as.factor(bn_df$civil)
bn_df$fumante<-as.factor(bn_df$fumante)
bn_df$excpeso<-as.factor(bn_df$excpeso)
bn_df$obesid<-as.factor(bn_df$obesid)
bn_df$hortareg<-as.factor(bn_df$hortareg)
bn_df$frutareg<-as.factor(bn_df$frutareg)
bn_df$flvreg<-as.factor(bn_df$flvreg)
bn_df$carneg<-as.factor(bn_df$carneg)
bn_df$franpl<-as.factor(bn_df$franpl)
bn_df$gordura<-as.factor(bn_df$gordura)
bn_df$leiteint<-as.factor(bn_df$leiteint)
bn_df$refritl5<-as.factor(bn_df$refritl5)
bn_df$atilaz<-as.factor(bn_df$atilaz)
bn_df$atiocu<-as.factor(bn_df$atiocu)
bn_df$atitrans<-as.factor(bn_df$atitrans)
bn_df$atidom<-as.factor(bn_df$atidom)
bn_df$inativo<-as.factor(bn_df$inativo)
bn_df$alcabu<-as.factor(bn_df$alcabu)
bn_df$saruim<-as.factor(bn_df$saruim)
bn_df$hart<-as.factor(bn_df$hart)
bn_df$diab<-as.factor(bn_df$diab)
bn_df$tv_d3<-as.factor(bn_df$tv_d3)

#discretize continuous data
dados_disc2<-discretize(bn_df, breaks = 3, ordered = FALSE, debug = TRUE)

bn_df<-dados_disc2

#K-fold
CV.HC<-bn.cv(bn_df, bn = "hc", runs = 10, loss.args = list("BIC-g") )
CV.MMHC<-bn.cv(bn_df, bn = "mmhc", runs = 10, loss.args = list("BIC-g"))
CV.HC
CV.MMHC

#blacklist
bl1 = matrix(c("atilaz", "inativo", "inativo", "atilaz"), ncol = 2, byrow = TRUE)
bl2 = matrix(c("atidom", "inativo", "inativo", "atidom"), ncol = 2, byrow = TRUE)
bl3 = matrix(c("atilaz", "atitrans", "atitrans", "atilaz"), ncol = 2, byrow = TRUE)
bl=matrix(c(bl1,bl2,bl3), ncol = 2, byrow = T)

#STRUCTURE ALGORITHM
res3 <- bnlearn::hc(bn_df)
res_fake<- random.graph ( names (bn_df))
res3

#NETWORK PARAMETERS CROSS-VALIDATION
a=bnlearn::score(res3, bn_df, type = "bic")
c=bnlearn::score(res3, bn_df, type = "aic")

a_f<-bnlearn::score(res_fake, bn_df, type = "bic")
c_f=bnlearn::score(res_fake, bn_df, type = "aic")

mat_f=matrix(c(a,c,a_f,c_f))
colnames(mat_f)<-(c("PAREMETERS"))
rownames(mat_f)<-(c("AIC","LOGLIK-G", "AIC_F", "LOGLIK-G_F"))
print(mat_f)

# PROCESSING ####

#STRUCTURE LEARNING
fittedbn<- bnlearn::bn.fit(res3, data = bn_df)
res3$nodes$tv_d3$parents

#CRIATE FIGURE
graphNELfitbn<- as.graphNEL(fittedbn)
attrs <- list(node=list(shape="rectangle", fixedsize=FALSE))
graph.par(list(nodes=list(fontsize=40)))
g1<-layoutGraph(graphNELfitbn, attrs=attrs)
renderGraph(g1)

#cross-validation strenght probability
boot<- boot.strength(bn_df, R = 20, algorithm = "hc")
bootC<-(boot[boot$strength > 0.85 & boot$direction >= 0.65, ])
avg.boot <- averaged.network(bootC,threshold = 0.80)
strength.plot(avg.boot,  bootC, layout = "dot", shape = "rectangle")


#CRIATE final FIGURE
graphNELfitbn2<- as.graphNEL(avg.boot)
attrs2 <- list(node=list(shape="rectangle", fixedsize=FALSE))
graph.par(list(nodes=list(fontsize=40)))
g2_G<-layoutGraph(graphNELfitbn2, attrs=attrs2)
renderGraph(g2_G)

#scores for boot - srenght BN
nodes<-names(avg.boot$nodes)
nodes2<-names(bn_df)
bn_df_sub<-subset(bn_df, select=(nodes))
c_boot=bnlearn::score(avg.boot, bn_df_sub, type = "bic")
c_boot

#Fit boot - strenght BN
FIT_BOOT<- bnlearn::bn.fit(avg.boot,bn_df_sub )
FIT_BOOT$atilaz

#ROC
pred = as.prediction(boot, res3)
perf = performance(pred, "tpr", "fpr")
plot(perf, main = "Arc Detection")
performance(pred, "auc")



#Probability Table
cpquery(avg.boot2,
        event = (atilaz = 1),
        evidence = regiao)



# LINEAR MODEL ####

#Linear Modell
res<-bnlearn::hc(bn_df_sub)
as.lm(FIT_BOOT, bn_df_sub)


FIT_BOOT$inativo

#plot lm models
layout(matrix(c(1,2,3,4),1,4)) # optional 4 graphs/page
dev.off()
plot(ina)

inativo<-lm(formula = inativo ~ EXPEC_VIDA_H + atilaz + atidom, data = bn_df_sub)
atidom<-lm(formula = atidom ~ frutareg + saruim, data = bn_df_sub)
atitrans<-lm(formula = atitrans ~ alcabu, data = bn_df_sub)
atiocu<-lm(formula = atiocu ~ refritl5, data = bn_df_sub)
tvd3<-lm(formula = tv_d3 ~ FROTA_100K, data = bn_df_sub)
fruta<-lm(formula = frutareg ~ INV_SAU_PCAPT + CESTA_BASICA, data = bn_df_sub)

summary(inativo)
summary(atidom)
summary(atiocu)
summary(tvd3)
summary(atitrans)
summary(fruta)
summary(bn_df$inativo-inativo$fitted.values)
plot(bn_df$inativo, inativo$fitted.values)
