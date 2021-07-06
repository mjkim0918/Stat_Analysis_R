library("readxl");library(dplyr);library(tidyverse);library(RJafroc);library(WriteXLS);library(glue)
library("e1071");library(epiR);library(pROC);library(caret);

roc_test <- function(y,score1, score2){
  roc1 <- roc(y, score1) 
  roc2 <- roc(y, score2) 
  roc.test(roc1,roc2)
  
  result <- round(c(roc.test(roc1,roc2)$estimate,roc.test(roc1,roc2)$p.value), 4) 
  names(result) <- c("AUC of roc1", "AUC of roc2", "p_value")
  return(result)
}
