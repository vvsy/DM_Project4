file = "/Users/wongshnyau/Dropbox/mice.data.Rda"

load(file)

library(caret)
library(tidyr)
library(dplyr)
library(pROC)
micedf %>% select(-id) -> micedf


as.numeric(micedf$cno) -> micedf$cno

#splite data to train and test
trainIndices = createDataPartition(micedf$dmfail, p=0.7, list=F)

df_train = micedf %>% 
  slice(trainIndices)

df_test = micedf %>% 
  slice(-trainIndices)

#set cross-validation
cv_opts10 = trainControl(method= 'cv', number = 10)

#predict
## logistic regression 
results_regreg = train(dmfail~., 
                       data=df_train,
                       method = "glm", 
                       trControl = cv_opts10,
                       family=binomial())

save(results_regreg,file="~/Desktop/results_regreg.Rda")

regreg_pred<-predict(results_regreg,df_test)
confusionMatrix(regreg_pred,df_test$dmfail,positive = '1') -> cfm_regreg
save(cfm_regreg,file="~/Desktop/cfm_regreg.Rda")

regreg_pred<-predict(results_regreg,df_test,type="prob")
roc(df_test$dmfail,regreg_pred$`1`) -> roc_regreg
save(roc_regreg,file="~/Desktop/roc_regreg.Rda")

## logistic rigid 

regreg_opts = expand.grid(.alpha = 0,
                          .lambda = seq(.1, .5, length = 5))

results_rigid = train(dmfail~., 
                       data=df_train,
                       method = "glmnet", 
                       trControl = cv_opts10, 
                       tuneGrid = regreg_opts)

save(results_rigid,file="~/Desktop/results_rigid.Rda")

rigid_pred<-predict(results_rigid,df_test)
confusionMatrix(rigid_pred,df_test$dmfail,positive = '1') -> cfm_rigid
save(cfm_rigid,file="~/Desktop/cfm_rigid.Rda")

rigid_pred<-predict(results_rigid,df_test,type="prob")
roc(df_test$dmfail,rigid_pred$`1`) -> roc_rigid
save(roc_rigid,file="~/Desktop/roc_rigid.Rda")

## logistic lasso 

regreg_opts = expand.grid(.alpha = 1,
                          .lambda = seq(.1, .5, length = 5))

results_lasso = train(dmfail~., 
                      data=df_train,
                      method = "glmnet", 
                      trControl = cv_opts10, 
                      tuneGrid = regreg_opts)

save(results_lasso,file="~/Desktop/results_lasso.Rda")

lasso_pred<-predict(results_lasso,df_test)
confusionMatrix(lasso_pred,df_test$dmfail,positive = '1') -> cfm_lasso
save(cfm_lasso,file="~/Desktop/cfm_lasso.Rda")

lasso_pred<-predict(results_lasso,df_test,type="prob")
roc(df_test$dmfail,lasso_pred$`1`) -> roc_lasso
save(roc_lasso,file="~/Desktop/roc_lasso.Rda")

## logistic GAM

results_GAM = train(dmfail~., 
                         data=df_train,
                         method = "gam", 
                         trControl = cv_opts10)

save(results_GAM,file="~/Desktop/results_GAM.Rda")

GAM_pred<-predict(results_GAM,df_test)
confusionMatrix(GAM_pred,df_test$dmfail,positive = '1') -> cfm_GAM
save(cfm_GAM,file="~/Desktop/cfm_GAM.Rda")

GAM_pred<-predict(results_GAM,df_test,type="prob")
roc(df_test$dmfail,GAM_pred$`1`) -> roc_GAM
save(roc_GAM,file="~/Desktop/roc_GAM.Rda")

## Decision Tree

results_dtree = train(dmfail~., 
                      data = df_train,
                      method = 'rpart',
                      trControl=cv_opts10)

save(results_dtree,file="~/Desktop/results_dtree.Rda")

dtree_pred<-predict(results_dtree,df_test)
confusionMatrix(dtree_pred,df_test$dmfail,positive = '1') -> cfm_dtree
save(cfm_dtree,file="~/Desktop/cfm_dtree.Rda")

dtree_pred<-predict(results_dtree,df_test,type="prob")
roc(df_test$dmfail,dtree_pred$`1`) -> roc_dtree
save(roc_dtree,file="~/Desktop/roc_dtree.Rda")

## k-Nearest Neighbor Classification

knn_opts = data.frame(k=c(seq(3, 11, 2)))

results_knn = train(dmfail~., 
                      data=df_train, 
                      method='knn',
                      trControl=cv_opts10,
                      tuneGrid = knn_opts)

save(results_knn,file="~/Desktop/results_knn.Rda")

knn_pred<-predict(results_knn,df_test)
confusionMatrix(knn_pred,df_test$dmfail,positive = '1') -> cfm_knn
save(cfm_knn,file="~/Desktop/cfm_knn.Rda")

knn_pred<-predict(results_knn,df_test,type="prob")
roc(df_test$dmfail,knn_pred$`1`) -> roc_knn
save(roc_knn,file="~/Desktop/roc_knn.Rda")

## Random Forest

results_rf = train(dmfail~., 
                   data = df_train,
                   method = 'rf',
                   ntree=100,
                   trControl=cv_opts10)

save(results_rf,file="~/Desktop/results_rf.Rda")

rf_pred<-predict(results_rf,df_test)
confusionMatrix(rf_pred,df_test$dmfail,positive = '1') -> cfm_rf
save(cfm_rf,file="~/Desktop/cfm_rf.Rda")

rf_pred<-predict(results_rf,df_test,type="prob")
roc(df_test$dmfail,rf_pred$`1`) -> roc_rf
save(roc_rf,file="~/Desktop/roc_rf.Rda")

## Neural Networks

results_nnet = train(dmfail~., 
                     data = df_train,
                     method = 'avNNet',
                     trControl=cv_opts10,
                     trace=FALSE)

save(results_nnet,file="~/Desktop/results_nnet.Rda")

nnet_pred<-predict(results_nnet,df_test)
confusionMatrix(nnet_pred,df_test$dmfail,positive = '1') -> cfm_nnet
save(cfm_nnet,file="~/Desktop/cfm_nnet.Rda")

nnet_pred<-predict(results_nnet,df_test,type="prob")
roc(df_test$dmfail,nnet_pred$`1`) -> roc_nnet
save(roc_nnet,file="~/Desktop/roc_nnet.Rda")

## Bagging

results_bagg = train(dmfail~., 
                     data = df_train,
                     method = 'treebag',
                     trControl=cv_opts10)

save(results_bagg,file="~/Desktop/results_bagg.Rda")

bagg_pred<-predict(results_bagg,df_test)
confusionMatrix(bagg_pred,df_test$dmfail,positive = '1') -> cfm_bagg
save(cfm_bagg,file="~/Desktop/cfm_bagg.Rda")

bagg_pred<-predict(results_bagg,df_test,type="prob")
roc(df_test$dmfail,bagg_pred$`1`) -> roc_bagg
save(roc_bagg,file="~/Desktop/roc_bagg.Rda")

### test bagging model 

testfile = "/Users/wongshnyau/Dropbox/mice.testdf.Rda"
load(testfile)

bagg_pred<-predict(results_bagg,micetestdf)
bagg_pred

bagg_prob<-predict(results_bagg,micetestdf,type="prob")
bagg_prob

###  fill ANS!

ans <- read.csv("/Users/wongshnyau/Dropbox/dmclass2018predict.csv")
ans$dmfailpredyesno <- bagg_pred
ans$dmfailpredprob <- bagg_prob$`1`

save(ans,file="~/Dropbox/710661118dmclass2018predict.csv.Rda")
