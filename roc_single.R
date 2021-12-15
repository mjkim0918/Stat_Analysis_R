library("readxl");library(dplyr);library(tidyverse);library(RJafroc);library(WriteXLS);library(glue)
library("e1071");library(epiR);library(pROC);library(caret);

roc_single <- function(truth, score){
  roc <- pROC::roc(truth, score,plot=T)
  
  auc_ci <- round(ci.auc(roc),3)
  auc_ci_res <- paste0(auc_ci[2],"(",auc_ci[1],", ",auc_ci[3],")")
  auc_res <- list(auc_ci = auc_ci_res, auc = auc_ci[2], auc_LB = auc_ci[1], auc_UB = auc_ci[3])
  return(auc_res)
}
