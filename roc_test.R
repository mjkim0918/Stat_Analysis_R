library("readxl");library(dplyr);library(tidyverse);library(RJafroc);library(WriteXLS);library(glue)
library("e1071");library(epiR);library(pROC);library(caret);

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
