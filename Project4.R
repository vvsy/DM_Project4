filepath <- "~/dropbox/dmclass2018train.csv"

read.csv(filepath) -> df

# library
library(tidyverse)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(reshape2)
library(VIM)
library(mice)

# explore data
str(df)
dim(df)

## look at those unique value
lapply(df, function(x) length(unique(x))) 

## drop the col which is only one outcome -> fnf (all of them Femoral Neck Fracture), seq57 (all of them are 0)
df %>% select(-fnf,-seq57) -> df

## drop age cols *****************,-age,-aid21
cormat <- round(cor(df[5:7]),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
df %>% select(-age,-aid21) -> df

## convert variables to correct property
factor_col = c("id","dmfail","renal","male","bipolar","ch4cat","ch2cat","ch3cat","cin4cat","cin2cat","cin3cat","cindex","cno","seq25","seq26","seq27","seq28","seq29","seq30","seq31","seq32","seq33","seq34","seq35","seq36","seq37","seq38","seq39","seq40","seq41","seq42","seq43","seq44","seq45","seq46","seq47","seq48","seq49","seq50","seq51","seq52","seq53","seq54","seq55","seq56","seq58","seq59","hospvol4cat","areacode","area4cat","city7","city5cat","city7cat","nihno")
df[factor_col] <- lapply(df[factor_col], factor)
numeric_col = c("med_cost","hospvolume","insamt","paym")
df[numeric_col ]<- lapply(df[numeric_col], as.numeric)

## check the ratio of missing value
df[ df == "XX" ] <- NA
apply(df, 2, function(col)sum(is.na(col))/length(col)) -> ratio 
as.data.frame(ratio) -> ratio
ratio
aggr_plot <- aggr(df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis= .5, gap=1)
#發現55:59行的na值有為非缺失所以丟掉56:59僅留下55行來補值
ggplot(df,aes(x =df$city5cat)) + geom_bar()
ggplot(df,aes(x =df$city7cat)) + geom_bar()#觀察5分布比較平均所以刪掉7
df %>% select(-city7cat,-areacode) -> df

####
mice.data <- mice(df,
                  m = 1,           
                  maxit = 5,      # max iteration
                  method = "cart", 
                  seed = 188)

micedf <- complete(mice.data,1)
apply(micedf, 2, function(col)sum(is.na(col))/length(col)) -> miceratio
as.data.frame(miceratio) -> miceratio
miceratio

save(micedf,file="~/Dropbox/mice.data.Rda")

##outcome
ggplot(df,aes(y = df$med_cost)) + geom_boxplot() #離群值6250筆不處理
df %>% filter(df$med_cost > quantile(df$med_cost,0.75)) %>% summarise(x=n())
ggplot(df,aes(x = df$dmfail)) + geom_bar() #多6744筆佔總體0.26沒差不處理 
























































