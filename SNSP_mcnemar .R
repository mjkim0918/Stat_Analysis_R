library("readxl");library(dplyr);library(tidyverse);library(RJafroc);library(WriteXLS);library(glue)
library("e1071");library(epiR);library(pROC);library(caret);
library(geepack)

SNSP_mcnemar <- function(truth, pred1, pred2){
  T1_snsp <- SNSP(truth, pred1)
  T2_snsp <- SNSP(truth, pred2)
  data_sn <- as.data.frame(cbind(truth,pred1, pred2)) %>% filter(truth == 1)
  p_sn <- round(mcnemar.test(data_sn$pred1,data_sn$pred2)$p.value,3)
  data_sp <- as.data.frame(cbind(truth,pred1, pred2)) %>% filter(truth == 0)
  p_sp <- round(mcnemar.test(data_sp$pred1,data_sp$pred2)$p.value,3)
  result <- list(T1_snsp = T1_snsp, T2_snsp = T2_snsp, p_sn = p_sn, p_sp = p_sp)
  return(result)
} 
