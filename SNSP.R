library("readxl");library(dplyr);library(tidyverse);library(RJafroc);library(WriteXLS);library(glue)
library("e1071");library(epiR);library(pROC);library(caret);
library(geepack) 
SNSP<-function(truth,pred){
  #Sensitivity, specificity 95% CI
  tb<-confusionMatrix(data=factor(pred),ref=factor(truth),positive="1")$tab[2:1,2:1]
  
  sensitivity<-epi.tests(tb)$elements$sensitivity
  specificity<-epi.tests(tb)$elements$specificity
  
  snsp_result<-list(sensitivity=sensitivity,specificity=specificity)
  se_sp <- round(unlist(snsp_result),3)
  se <- se_sp[1]
  se_ci <- paste0(se_sp[1], "(", se_sp[2], ", ", se_sp[3], ")")
  sp <- se_sp[4]
  sp_ci <- paste0(se_sp[4], "(", se_sp[5], ", ", se_sp[6], ")")

  result <- list(sen_ci = se_ci, sp_ci = sp_ci, sen = se, sp = sp)
  return(result)
}
