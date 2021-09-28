library("readxl");library(dplyr);library(tidyverse);library(RJafroc);library(WriteXLS);library(glue)
library("e1071");library(epiR);library(pROC);library(caret);

##comparing two diagnostic tests based on data from one sample
roc_test <- function(y,score1, score2){
  roc1 <- roc(y, score1) 
  roc2 <- roc(y, score2) 
  roc.test(roc1,roc2)
  auc_ci1 <- round(ci.auc(roc1),3)
  auc_res1 <- paste0(auc_ci1[2], "(", auc_ci1[1],", ",auc_ci1[3],")")
  auc_ci2 <- round(ci.auc(roc2),3)
  auc_res2 <- paste0(auc_ci2[2],"(",auc_ci2[1],", ",auc_ci2[3],")")
  result <- list(auc_res1 = auc_res1, auc_res2 =auc_res2, p_value = round(roc.test(roc1,roc2)$p.value,3))
  return(result)
}

## comparing two different diagnostic tests based on data from two independent samples
roc_test2 <- function(y1, y2, score1, score2){
  roc1 <- roc(y1, score1) 
  roc2 <- roc(y2, score2) 
  roc.test(roc1,roc2)
  auc_ci1 <- round(ci.auc(roc1),3)
  auc_res1 <- paste0(auc_ci1[2], "(", auc_ci1[1],", ",auc_ci1[3],")")
  auc_ci2 <- round(ci.auc(roc2),3)
  auc_res2 <- paste0(auc_ci2[2],"(",auc_ci2[1],", ",auc_ci2[3],")")
  result <- list(auc_res1 = auc_res1, auc_res2 =auc_res2, p_value = round(roc.test(roc1,roc2)$p.value,3))
  return(result)
}
