library("readxl");library(dplyr);library(tidyverse);library(RJafroc);library(WriteXLS);library(glue)
library("e1071");library(epiR);library(pROC);library(caret);
library(geepack)

##comparing two diagnostic tests based on data from one sample
SNSP_mcnemar <- function(truth, pred1, pred2){
  #Sensitivity & Specificity for pred1
  T1_snsp <- SNSP(truth, pred1)
  
  #Sensitivity & Specificity for pred2
  T2_snsp <- SNSP(truth, pred2)
  
  #Mcnemar's test for Sensitivity
  data_sn <- as.data.frame(cbind(truth,pred1, pred2)) %>% filter(truth == 1)
  p_sn <- round(mcnemar.test(data_sn$pred1,data_sn$pred2)$p.value, 3)
  
  #Mcnemar's test for Specificity
  data_sp <- as.data.frame(cbind(truth,pred1, pred2)) %>% filter(truth == 0)
  p_sp <- round(mcnemar.test(data_sp$pred1,data_sp$pred2)$p.value, 3)
  
  result <- list(T1_snsp = T1_snsp, T2_snsp = T2_snsp, p_sn = p_sn, p_sp = p_sp)
  return(result)
} 


## comparing two different diagnostic tests based on data from two independent samples
SNSP_chisq <- function(truth1, truth2, pred1, pred2){
  snsp_df1 <- SNSP(truth1, pred1)
  snsp_df2 <- SNSP(truth2, pred2)
  tb1 <- confusionMatrix(data=factor(pred1),ref=factor(truth1),positive="1")$tab[2:1,2:1]
  tb2 <- confusionMatrix(data=factor(pred2),ref=factor(truth2),positive="1")$tab[2:1,2:1]
  tb_sen <- cbind(tb1[,1],tb2[,1])
  pvalue_sn <- round(chisq.test(tb_sen,correct = F)$p.value, 4)
  tb_spec <- cbind(tb1[,2],tb2[,2])
  pvalue_sp <- round(chisq.test(tb_spec,correct = F)$p.value,4)
  result <- list(T1_snsp = snsp_df1, T2_snsp = snsp_df2, p_sn = pvalue_sn, p_sp = pvalue_sp)
  return(result)
}
