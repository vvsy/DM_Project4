filepath <- "~/dropbox/dmclass2018train.csv"

read.csv(filepath) -> df

# library
library(tidyverse)
library(kableExtra)
library(dplyr)

# explore data

str(df)
dim(df)

## look at those unique value
lapply(df, function(x) length(unique(x))) 

## drop the col which is only one outcome -> fnf (all of them Femoral Neck Fracture), seq57 (all of them are 0)
df %>% select(-fnf,-seq57) -> df

## check the ratio of missing value
apply(df, 2, function(col)sum(is.na(col))/length(col)) -> ratio 
as.data.frame(ratio) -> ratio
ratio

## convert variables to correct property
factor_col = c("id","dmfail","renal","aid19","aid21","male","bipolar","ch4cat","ch2cat","ch3cat","cin4cat","cin2cat","cin3cat","cindex","cno","seq25","seq26","seq27","seq28","seq29","seq30","seq31","seq32","seq33","seq34","seq35","seq36","seq37","seq38","seq39","seq40","seq41","seq42","seq43","seq44","seq45","seq46","seq47","seq48","seq49","seq50","seq51","seq52","seq53","seq54","seq55","seq56","seq58","seq59","hospvol4cat","areacode","area4cat","city7","city5cat","city7cat","nihno")

df[factor_col] <- lapply(df[factor_col], factor)

numeric_col = c("med_cost","age","hospvolume","insamt","paym")
df[numeric_col ]<- lapply(df[numeric_col], as.numeric)

## fill NA by mice with 1 iteration (I will redo the outcome with 50 iteration by my PC's GPU later)
library(mice)
mice.data <- mice(df,
                  m = 1,           
                  maxit = 1,      # max iteration
                  method = "cart", 
                  seed = 188)

df <- complete(mice.data,1)

apply(df1, 2, function(col)sum(is.na(col))/length(col)) #I don't know why there still has NA in city7cat?


# EDA

