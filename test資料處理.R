testfile = "/Users/wongshnyau/Dropbox/dmclass2018test.csv"

# library
library(tidyverse)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(reshape2)
library(VIM)
library(mice)

read.csv(testfile) -> testdf
testdf %>% select(-fnf,-seq57,-age,-aid21) -> testdf

factor_col = c("id","renal","male","bipolar","ch4cat","ch2cat","ch3cat","cin4cat","cin2cat","cin3cat","cindex","seq25","seq26","seq27","seq28","seq29","seq30","seq31","seq32","seq33","seq34","seq35","seq36","seq37","seq38","seq39","seq40","seq41","seq42","seq43","seq44","seq45","seq46","seq47","seq48","seq49","seq50","seq51","seq52","seq53","seq54","seq55","seq56","seq58","seq59","hospvol4cat","areacode","area4cat","city7","city5cat","city7cat","nihno")
testdf[factor_col] <- lapply(testdf[factor_col], factor)
numeric_col = c("cno","hospvolume","insamt","paym")
testdf[numeric_col ]<- lapply(testdf[numeric_col], as.numeric)

testdf %>% select(-city7cat,-areacode) -> testdf
testdf %>% select(-id) -> testdf

apply(testdf, 2, function(col)sum(is.na(col))/length(col)) -> testdfratio
as.data.frame(testdfratio) -> testdfratio
testdfratio

mice.testdf <- mice(testdf,
                  m = 1,           
                  maxit = 5,      # max iteration
                  method = "cart", 
                  seed = 188)
micetestdf <- complete(mice.testdf,1)


apply(micetestdf, 2, function(col)sum(is.na(col))/length(col)) -> micetestdfratio
as.data.frame(micetestdfratio) -> micetestdfratio
micetestdfratio


save(micetestdf,file="~/Dropbox/mice.testdf.Rda")
