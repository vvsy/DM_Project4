library(tidyverse)
file = "/Users/liguanzhi/Dropbox/mice.data.Rda"

load(file)

as.numeric(micedf$cno) -> micedf$cno
micedf %>% filter(med_cost <= quantile(micedf$med_cost,.85)) -> micedf
micedf %>% select(-id,-dmfail) -> micedf

library(caret)
library(tidyr)
library(dplyr)

#splite data to train and test
trainIndices = createDataPartition(micedf$med_cost, p=0.7, list=F)

df_train = micedf %>% 
  slice(trainIndices)

df_test = micedf %>% 
  slice(-trainIndices)

#set cross-validation
cv_opts10 = trainControl(method= 'cv', number = 10)

#lm
results_lm = train(med_cost~., 
                   data=df_train, 
                   method='lm',
                   trControl=cv_opts10)
results_lm
preds_lm = predict(results_lm, df_test)
lmtest=postResample(pred = preds_lm, obs = df_test$med_cost)
save(lmtest,file="~/Desktop/lmtest.Rda")
save(results_lm,file="~/Desktop/lm.Rda")


#lmback(model selection ) 
results_lmback = train(med_cost~., 
                       data=df_train, 
                       method='leapBackward',
                       trControl=cv_opts10)
results_lmback
preds_lmback = predict(results_lmback, df_test)
lmbacktest=postResample(pred = preds_lmback, obs = df_test$med_cost)
save(lmbacktest,file="~/Desktop/lmbacktest.Rda")
save(results_lmback,file="~/Desktop/lmback.Rda")


#lmforward(model selection ) 
results_lmforward = train(med_cost~., 
                       data=df_train, 
                       method='leapForward',
                       trControl=cv_opts10)
results_lmforward
preds_lmforward = predict(results_lmforward, df_test)
lmforwardtest=postResample(pred = preds_lmforward, obs = df_test$med_cost)
save(results_lmforward,file="~/Desktop/lmforward.Rda")
save(lmforwardtest,file="~/Desktop/lmforwardtest.Rda")

#ridge
regreg_ridge = expand.grid(.alpha = 0,
                           .lambda = seq(.1, .5, length = 5))
results_ridge = train(med_cost~., 
                      data=df_train,
                      method = "glmnet", 
                      trControl = cv_opts10, 
                      tuneGrid = regreg_ridge)
results_ridge
preds_ridge = predict(results_ridge, df_test)
ridgetest=postResample(pred = preds_ridge, obs = df_test$med_cost)
save(results_ridge,file="~/Desktop/ridge.Rda")
save(ridgetest,file="~/Desktop/ridgetest.Rda")

#lasso
regreg_lasso = expand.grid(.alpha = 1,
                           .lambda = seq(.1, .5, length = 5))
results_lasso = train(med_cost~., 
                      data=df_train,
                      method = "glmnet", 
                      trControl = cv_opts10, 
                      tuneGrid = regreg_lasso)
results_lasso
preds_lasso = predict(results_lasso, df_test)
lassotest=postResample(pred = preds_lasso, obs = df_test$med_cost)
save(results_lasso,file="~/Desktop/lasso.Rda")
save(lassotest,file="~/Desktop/lassotest.Rda")

#GAM
results_gam = train(med_cost~., 
                    data=df_train, 
                    method='gam',
                    trControl=cv_opts10)

results_gam
preds_gam = predict(results_gam, df_test)
gamtest=postResample(pred = preds_gam, obs = df_test$med_cost)
save(results_gam,file="~/Desktop/gam.Rda")
save(gamtest,file="~/Desktop/gamtest.Rda")

#pcr
results_pcr = train(med_cost~., 
                    data=df_train, 
                    method='pcr',
                    trControl=cv_opts10)

results_pcr
preds_pcr = predict(results_pcr, df_test)
pcrtest=postResample(pred = preds_pcr, obs = df_test$med_cost)
save(results_pcr,file="~/Desktop/pcr.Rda")
save(pcrtest,file="~/Desktop/pcrtest.Rda")

#plsr
results_plsRglm = train(med_cost~., 
                        data=df_train, 
                        method='plsRglm',
                        trControl=cv_opts10)

results_plsRglm
preds_plsRglm = predict(results_plsRglm, df_test)
postResample(pred = preds_plsRglm, obs = df_test$med_cost)

### test lasso model 

testfile = "~/Dropbox/mice.testdf.Rda"
load(testfile)

lasso_pred<-predict(results_lasso,micetestdf)
as.data.frame(lasso_pred)->lasso_pred

lasso_pred

###  fill ANS!

ans <- load("~/Dropbox/710661118dmclass2018predict.csv.Rda")

ans$med_costpred <- lasso_pred$lasso_pred

write.table(ans, file = "~/Dropbox/710661118dmclass2018predict.csv", sep = ",", row.names = FALSE)













