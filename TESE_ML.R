

#Load packages
install.packages("easypackages")
library(easypackages)

#download any package needed
packages("PerformanceAnalytics","caTools", "DMwR", "abind", "pROC" , "mice", "arm", "bartMachine", "dataMaid","caret","AppliedPredictiveModeling","doParallel", "reshape2","foreign","data.table", "car", "mice",
         "corrplot", "PerformanceAnalytics", "ggplot2", "psych", "dplyr","arm","bartMachine","randomForest","caTools", "DMwR", "pROC", "plotROC","pROC")

#load any package need
libraries("dataMaid","caret","AppliedPredictiveModeling","doParallel", "reshape2","foreign","data.table", "car", "mice",
          "corrplot", "PerformanceAnalytics", "ggplot2", "psych", "dplyr","arm","bartMachine","randomForest","caTools", "DMwR", "pROC", "plotROC", "obliqueR","pROC")


#DATA
library(readxl)
CAPITAIS_2006_2016 <- read_excel("ESTUDO/DOUTORADO/TESE/Dados/TESE/CAPITAIS_2006_2016.xlsx", 
                                 sheet = "BAYS_NET_AF_GERAL")


View(CAPITAIS_2006_2016)
DADOS<- CAPITAIS_2006_2016


# Funcao para gerar dados de treino e dados de teste
splitData <- function(dataframe, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset = trainset, testset = testset)
}


predictors <- subset(DADOS, select = 
                          -c(atilaz,	
                             atiocu,
                             atitrans,
                             atidom,
                             inativo,
                             saruim,	
                             hart,	
                             diab,	
                             tv_d3
                             ))

af<- subset(DADOS, select = c(atilaz,	
                              atiocu,
                              atitrans,
                              atidom,
                              inativo,
                              saruim,	
                              hart,	
                              diab,	
                              tv_d3
                              ))

str(predictors)



####################################### Paralell Processing #################################### 

clust <- parallel::makePSOCKcluster(4) # cria as cópias do R que rodam em paralelo
doParallel::registerDoParallel(clust) #define number of threads that will be used during analysis process
#clusterStop(clust) #parar o processamento paralelo

####################################### BOOT #######################################################################
#definig the boot control
#creating controlling variable
bootControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, allowParallel = TRUE, search = "random",
                            returnData= TRUE,  savePredictions=TRUE)


####################################### MLMODELS####
#### KNN #####################################################################################
#Models - Instance-based Algorithms
#k-Nearest Neighbor (kNN)




#Learning Vector Quantization (LVQ)
#Self-Organizing Map (SOM)
#Locally Weighted Learning (LWL)


#model gbm
model_gbm<-train(predictors, af$atilaz, preProcess = c("center", "scale"), method='gbm', tuneLength = 7, trControl = bootControl)
model_knn<-train(predictors, af$atilaz,preProcess = c("center", "scale"), method='knn', tuneLength = 7, trControl = bootControl)
model_rf<-train(predictors, af$atilaz, preProcess = c("center", "scale"), method='rf', tuneLength = 7, trControl = bootControl)
model_nnet<-train(predictors, af$atilaz, preProcess = c("center", "scale"), method='nnet', tuneLength = 7, trControl = bootControl)
model_glm<-train(predictors, af$atilaz, preProcess = c("center", "scale"), method='glm', tuneLength = 7, trControl = bootControl)
model_neural<-train(predictors, af$atilaz, preProcess = c("center", "scale"), method='neuralnet', tuneLength = 7, trControl = bootControl)



#### NEURAL ##############################################################################
#Models - Artificial Neural Network Algorithms
#Neural Network
# here we are controling for size of tree, iterations and learning rate
nngrid <- expand.grid(size = 10, decay=2)

nnFit <- train(predictors, af$atilaz,
               preProcess = c("center", "scale"),
               method = "nnet", tuneLength = 7,
               trControl = bootControl,
               tuneGrid = nngrid)
nnFit

nnFit$results
nnFit.varimp <- varImp(nnFit, scale = FALSE)
plot(nnFit.varimp, main = "Importance of all Variables for 'Neural Network' model")




#### GBM ###############################################################################
#Models - Ensemble Algorithms
#Boosting
#Bootstrapped Aggregation (Bagging)
#AdaBoost
#Stacked Generalization (blending)
#Gradient Boosting Machines (GBM)
# Example with gradient boosting machine
# here we are controling for size of tree, iterations and learning rate
gbmGrid <- expand.grid(.interaction.depth = (1:3) * 2,
                       .n.trees = (1:10)*20, .shrinkage = .1, .n.minobsinnode= 20)
#set.seed(2)
gbmFit <- train(predictors, af$atilaz,
                method = "gbm", 
                trControl = bootControl, 
                verbose = FALSE,
                bag.fraction = 0.5, 
                tuneGrid = gbmGrid)

plot(gbmFit, metric = "Kappa")
plot(gbmFit, plotType = "level")
resampleHist(gbmFit)
gbmFit

resul_gbm <- gbmFit$pred
pred_gbm <- predict (resul_gbm, predictors)
ggplot(resul_gbm, aes(resul_gbm$pred, resul_gbm$obs))


#compute the var imp
gbmFit.varimp <- varImp(gbmFit, scale = F)
plot(gbmFit.varimp, main = "Importance of all Variables for 'gbm' model")


#Gradient Boosted Regression Trees (GBRT)


#Random Forest
#### RANDOMFOREST ############################################################################################
rfFit <- train(predictors, af$atilaz,
               preProcess = c("center", "scale"),
               method = "parRF", tuneLength = 7,
               trControl = bootControl)
rfFit
rfFit$finalModel

resul_rf <- rfFit$pred
cf_matrix_rf <- confusionMatrix(resul_rf$pred, resul_rf$obs)
cf_matrix_rf

rfFit.varimp <- varImp(rfFit)
plot(rfFit.varimp, main = "Importance of all Variables for 'rf' model")


#Variable importance

rfFit.varimp <- varImp(rfFit)
bagFit.varimp <- varImp(bagFit)
gbmFit.varimp <- varImp(gbmFit)

plot(rfFit.varimp, main = "Importance of all Variables for 'rf' model")
plot(bagTFit.varimp, main = "Importance of all Variables for 'bagged tree' model")
plot(gbmFit.varimp, main = "Importance of all Variables for 'gbm' model")



####################################### ROC ####################################################################################

#first the response and after the predictor(probabilities), that is the order of the objects. 
#create the curves for each model.
par(pty="s",mfrow = c(3,3)) #define a frame to plot several graphics simultaneously. 
par(pty="s") #define the plot area with square shape, must be runned before the graphic. 
dev.off() #deactivate the graphical parameters of the plots display area
#knn




nn_roc <- plot.roc(resul_nn$pred, resul_nn$obs, 
                   main="Neural network", percent= TRUE, print.auc= TRUE,
                   ci=TRUE, 
                   print.auc.x=ifelse(resul_nn$normal, 70,70),
                   print.auc.y=ifelse(resul_nn$normal, 10,10),
                   print.auc.cex = 1)

##CI PARA SENSIBILIDE E ESPECIFICIDADE (NÃO CONSEGUI RODAR, TALVEZ POR NÃO TER CRIADO O VETOR "TESTE")

testeci <- ci.se(teste, boot.n = 25000, parallel = TRUE, # CI of sensitivity
                 
                 specificities=seq(0, 100, 5)) # over a select set of specificities

plot(testeci, type="shape", col="#1c61b6AA") # plot as a blue shape

#### Create the matrix graphics ####

par(pty="s",mfrow = c(3,3))
#### K nearest neighbor ####
plot.roc(resul_knn$obs, resul_knn$normal, 
         main="K nearest neighbor", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_knn$normal, 70,70),
         print.auc.y=ifelse(resul_knn$normaly, 10,10),
         # max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Bayesian Generalized Linear Model ####
plot.roc(resul_bayes$obs, resul_bayes$normal, 
         main="Bayesian Generalized Linear Model", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_bayes$normal, 70,70),
         print.auc.y=ifelse(resul_bayes$normal, 10,10),
         # max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Ridge Regression ####
plot.roc(resul_ridge$obs, resul_ridge$normal, 
         main="Ridge Regression", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_ridge$normal, 70,70),
         print.auc.y=ifelse(resul_ridge$normal, 10,10),
         # max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Bagged tree ####
plot.roc(resul_bag$obs, resul_bag$normal, 
         main="Bagged tree", percent= TRUE,print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_bag$normal, 70,70),
         print.auc.y=ifelse(resul_bag$normal, 10,10),
         #max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Gradient boosting machine ####
plot.roc(resul_gbm$obs, resul_gbm$normal, 
         main="Gradient boosting machine", percent= TRUE,print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_gbm$normal, 70,70),
         print.auc.y=ifelse(resul_gbm$normal, 10,10),
         # max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Random forest ####
plot.roc(resul_rf$obs, resul_rf$normal, 
         main= "Random forest", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_rf$normal, 70,70),
         print.auc.y=ifelse(resul_rf$normal, 10,10),
         #max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Neural network ####
plot.roc(resul_nn$obs, resul_nn$normal, 
         main="Neural network", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_nn$normal, 70,70),
         print.auc.y=ifelse(resul_nn$normal, 10,10),
         #max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Single C5.0 Ruleset ####
plot.roc(resul_c5$obs, resul_c5$normal, 
         main="Single C5.0 Ruleset", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_c5$normal, 70,70),
         print.auc.y=ifelse(resul_c5$normal, 10,10),
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Bayesian Additive Regression Trees ####
plot.roc(resul_bayesNT$obs, resul_bayesNT$normal, 
         main="Bayesian Additive Regression Trees", percent= TRUE,print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_bayesNT$normal, 70,70),
         print.auc.y=ifelse(resul_bayesNT$normal, 10,10),
         #max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)

#### Create all curves in one graphic ####
dev.off()
par(pty="s") #define the plot area with square shape, must be runned before the graphic. 
#create the curves and lines
all_resul_bayes_roc <- plot.roc(resul_bayes$obs, resul_bayes$normal, 
                                main="Comparison among models", col="black", percent = TRUE,
                                grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black")
all_resul_ridge_roc <- lines.roc(resul_ridge$obs, resul_ridge$normal,percent = TRUE, col="blue3")
all_resul_rf_roc <- lines.roc(resul_rf$obs, resul_rf$normal, percent = TRUE,col="brown4")
all_resul_gbm_roc <- lines.roc(resul_gbm$obs, resul_gbm$normal, percent = TRUE,col="darkgoldenrod2")
all_resul_bayest_roc <- lines.roc(resul_bayesNT$obs, resul_bayesNT$normal,percent = TRUE,  col="darkmagenta")
all_resul_bag_roc <- lines.roc(resul_bag$obs, resul_bag$normal, percent = TRUE,col="springgreen4")
all_resul_nn_roc <- lines.roc(resul_nn$obs, resul_nn$normal,percent = TRUE, col="cyan3")
all_resul_c5_roc <- lines.roc(resul_c5$obs, resul_c5$normal, percent = TRUE,col="coral2")
all_resul_knn_roc <- lines.roc(resul_knn$obs, resul_knn$normal, percent = TRUE,col="dimgrey")

#plot the curves and lines

plot.roc(all_resul_bayes_roc, main="Comparison among models", col="black", percent = TRUE,
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black")
lines.roc(all_resul_ridge_roc,percent = TRUE, col="blue3")
lines.roc(all_resul_rf_roc, percent = TRUE,col="brown4")
lines.roc(all_resul_gbm_roc, percent = TRUE,col="darkgoldenrod2")
lines.roc(all_resul_bayest_roc,percent = TRUE,  col="darkmagenta")
lines.roc(all_resul_bag_roc, percent = TRUE,col="springgreen4")
lines.roc(all_resul_nn_roc,percent = TRUE, col="cyan3")
lines.roc(all_resul_c5_roc, percent = TRUE,col="coral2")
lines.roc(all_resul_knn_roc, percent = TRUE,col="dimgrey")
legend(legend=c("AUC 87.8% - Bay. Gen. Lin. Mod", "AUC 87.2% - Ridge Regression",
                "AUC 86.9% - Random forest","AUC 86.5% - Gradient boosting machine",
                "AUC 86.1% - Bay. Add. Reg. Tr.", "AUC 85.2% - Bagged tree","AUC 80.7% - Neural network",
                "AUC 80.9% - Single C5.0 Ruleset","AUC 69.9% - K nearest neighbors"),
       col=c("black", "blue3","brown4","darkgoldenrod2","darkmagenta","springgreen4","cyan3","coral2","dimgrey"),
       lwd=2, cex = .65, x= 63, y=38, border.col = "white")

#function to smoothing not to fantastic
smth <- function(x,y){
  lines(smooth(x, method="binormal"), col=y)
}

#### table do create list of CI by method ######

methods_used <- c("K nearest neighbor",
                  "Ridge Regression",
                  "Bagged tree",
                  "Bayesian Additive Regression Trees",
                  "Gradient boosting machine",
                  "Random forest"
)

methods_used_auc <- c(knn_roc$auc,
                      ridge_roc$auc,
                      bag_roc$auc,
                      bayes_nt_roc$auc,
                      gbm_roc$auc,
                      rf_roc$auc
)

methods_used_ci_inferior <- c(knn_roc$ci[1],
                              ridge_roc$ci[1],
                              bag_roc$ci[1],
                              bayes_nt_roc$ci[1],
                              gbm_roc$ci[1],
                              rf_roc$ci[1]
)

methods_used_ci_superior <- c(knn_roc$ci[3],
                              ridge_roc$ci[3],
                              bag_roc$ci[3],
                              bayes_nt_roc$ci[3],
                              gbm_roc$ci[3],
                              rf_roc$ci[3]
)

model_comparison <- data.frame(methods_used,methods_used_auc,methods_used_ci_inferior,methods_used_ci_superior)
write.csv(model_comparison, file = "model_comparison.csv")

#Creates a CI for a ROC object. 
knn_roc_CI<- ci.auc(knn_roc, method = "bootstrap" , boot.n = 20000, reuse.auc = TRUE, parallel = 4)
ridge_roc_CI<- ci.auc(ridge_roc, method = "bootstrap" , boot.n = 20000, reuse.auc = TRUE, parallel = 4)
bag_roc_CI<- ci.auc(bag_roc, method = "bootstrap" , boot.n = 20000, reuse.auc = TRUE, parallel = 4)
bayes_nt_roc_CI<- ci.auc(bayes_nt_roc, method = "bootstrap" , boot.n = 20000, reuse.auc = TRUE, parallel = 4)
gbm_roc_CI<- ci.auc(gbm_roc, method = "bootstrap" , boot.n = 20000, reuse.auc = TRUE, parallel = 4)
rf_roc_CI<- ci.auc(rf_roc, method = "bootstrap" , boot.n = 20000, reuse.auc = TRUE, parallel = 4)
bayes_roc_CI<-ci.auc(bayes_roc, method = "bootstrap" , boot.n = 20000, reuse.auc = TRUE, parallel = 4)

library(ggplot2)
################################### PLOT BEST MODEL ######################################################

#bestmodel = ???

best_model_varimp <-####$importance
  best_model_varimp$ansioso <- NULL
best_model_varimp$legend<-rownames(best_model_varimp)

#sorting the colums of importance.
best_model_varimp <- best_model_varimp[order(-best_model_varimp$Overall),c(1,2)]
order_graph <- rev(head(best_model_varimp$legend))

best_model_varimp1<-head(best_model_varimp)

graph<- ggplot(data=best_model_varimp, aes(x=legend, y=Overall)) +
  geom_bar(stat="identity", fill="darkblue")

graph + coord_flip()
graph + coord_flip()+ scale_x_discrete(name="Predictors", limits=order_graph)+ 
  scale_y_discrete(limits=c(0,25,50,75,100), name="Variable Importance")+
  theme_classic()