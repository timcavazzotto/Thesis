
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
library(caTools)
library(pROC)
library(plotROC)


#open Big DATA file 

dados<-fread("~/ESTUDO/DOUTORADO/TESE/Dados/TESE/BNindv_dicretizaR.csv", sep = ";")

#SELECT DATE AND BUILD NETWORK
bn_dfall <- data.frame(dados)
bn_dfall[] <- lapply(bn_dfall, function(x) { 
  if(is.integer(x)) as.factor(x) else x
}) # all factor

str(bn_dfall)

#discretize continuous data
dados_disc2<-discretize(bn_df, breaks = 5, ordered = FALSE, debug = FALSE)

bn_df<-dados_disc2
str(bn_df)
write.csv2(bn_df, file ="ESTUDO/DOUTORADO/TESE/Dados/TESE/BNindv_dicretiza.csv",row.names=FALSE)

#subset atilaz - sensibility test # 
bn_df<-subset(bn_dfall, select = -c(ano, mesfim, regiao, atiocu, atitrans, atidom, inativo, tv_d3))
bn_df[] <- lapply(bn_df, function(x) { 
  if(is.integer(x)) as.factor(x) else x
}) # all factor


#K-fold
CV.HC<-bn.cv(bn_df, bn = "hc", runs = 10, loss.args = list("BIC-g") )
CV.MMHC<-bn.cv(bn_df, bn = "mmhc", runs = 10, loss.args = list("BIC-g"))
CV.HC
CV.MMHC


#STRUCTURE ALGORITHM
res3 <- bnlearn::hc(bn_df) # best algorithm 
res3$nodes$atilaz

# bn_df<-bn_dfall # run after sensibility test

#NETWORK PARAMETERS CROSS-VALIDATION
a=bnlearn::score(res3, bn_df, type = "bic")
c=bnlearn::score(res3, bn_df, type = "aic")
mat_f=matrix(c(a,c))
colnames(mat_f)<-(c("PAREMETERS"))
rownames(mat_f)<-(c("AIC","LOGLIK-G"))
print(mat_f) # table paremeters 

# PROCESSING ####

#STRUCTURE LEARNING
fittedbn<- bnlearn::bn.fit(res3, data = bn_df)
fittedbn$tv_d3$parents

#CRIATE FIGURE
graphNELfitbn<- as.graphNEL(fittedbn)
attrs <- list(node=list(shape="rectangle", fixedsize=FALSE))
graph.par(list(nodes=list(fontsize=40)))
g1<-layoutGraph(graphNELfitbn, attrs=attrs)
renderGraph(g1)

#cross-validation strenght probability
boot<- boot.strength(bn_df, R = 20, algorithm = "hc")
bootC<-(boot[boot$strength > 0.51 & boot$direction >= 0.65, ])
avg.boot <- averaged.network(bootC,threshold = 0.65)
strength.plot(avg.boot,  bootC, layout = "dot", shape = "rectangle")


#CRIATE final FIGURE
tiff("rede.tif", width = 21980, height = 18980, res = 900)
graphNELfitbn2<- as.graphNEL(avg.boot)
attrs2 <- list(node=list(shape="rectangle", fixedsize=FALSE))
graph.par(list(nodes=list(fontsize=25)))
g2_G<-layoutGraph(graphNELfitbn2, attrs=attrs2)
renderGraph(g2_G)
dev.off()

#scores for boot - srenght BN
nodes<-names(avg.boot$nodes)
nodes2<-names(bn_df)
bn_df_sub<-subset(bn_df, select=(nodes))
c_boot=bnlearn::score(avg.boot, bn_df_sub, type = "bic")
c_boot

#Fit boot - strenght BN
FIT_BOOT<- bnlearn::bn.fit(avg.boot,bn_df_sub )
FIT_BOOT$tv_d3$parents
bn.fit.dotplot(FIT_BOOT$atitrans)

#ROC
pred = as.prediction(boot, res3)
perf = performance(pred, "tpr", "fpr")
plot(perf, main = "Arc Detection")
performance(pred, "auc")


#Probability Table"
r1f1_1<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))
r1f1_2<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))
r1f1_3<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))
r1f1_4<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))
r1f1_5<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))
r1f1_6<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))
r1f1_7<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))
r1f1_8<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))
r1f1_9<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))
r1f1_10<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))
r1f1_11<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))
r1f1_12<-cpquery(FIT_BOOT, (atidom == 1) , (idade == 1 & sexo == 1 & civil == 1 & flvreg == 1))

matr1<-matrix(c(r1f1_1,
          r1f1_2,
          r1f1_3,
          r1f1_4,
          r1f1_5,
          r1f1_6,
          r1f1_7,
          r1f1_8,
          r1f1_9,
          r1f1_10,
          r1f1_11,
          r1f1_12))

(matr1)
t.test(matr1)

REDES<- avg.boot[["arcs"]] # create df for igraph


#igraph
library(igraph)
library(bipartite)
redesig<-graph.data.frame(unique(REDES), directed = T)
plot(redesig,
     vertex.color="gray",
     edge.arrow.size=.2,
     vertex.label.dist=1.5,
     vertex.size=5)
    
library(visNetwork)
visIgraph(redesig)
V(redesig)$color <- "gray80"
V(redesig)$label.cex = 1.5
V(redesig)$
visIgraph(redesig)

###### Predicted both ######
####################################### Paralell Processing #################################### 

clust <- parallel::makePSOCKcluster(4) # cria as cópias do R que rodam em paralelo
doParallel::registerDoParallel(clust) #define number of threads that will be used during analysis process
#clusterStop(clust) #parar o processamento paralelo

####################################### BOOT #######################################################################
#definig the boot control
#creating controlling variable
bootControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, allowParallel = TRUE, search = "random",
                            returnData= TRUE,  classProb= TRUE, savePredictions=TRUE, sampling="smote")


bayes_fit <- train(predictors_mh, mental_health$SLEEP_ANXIETY,
                   preProcess = c("center", "scale"),
                   method = "bayesglm", tuneLength = 7,
                   metric = "Kappa",
                   trControl = bootControl)

pred = as.prediction(boot, res3)
perf = performance(pred, "tpr", "fpr")
performance(pred, "auc")
plot(perf, main = "Arc Detection")

