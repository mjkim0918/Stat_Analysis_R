library("readxl");library(dplyr);library(tidyverse);library(RJafroc);library(WriteXLS);library(glue)
library("e1071");library(epiR);library(pROC);library(caret);
library(geepack)

snsp_gee <- function(Performance, Test, PatientID, GT, data){
  #Performance : reader's test result, test : without AI(0), with AI(1)
  #sensitivity
  data_sn <- data %>% filter(GT == 1)
  model_sn <- geeglm(Performance ~ Test, id = PatientID, data = data_sn, family = binomial(link = "identity"), corstr = "independence")
  p_sn <- summary(model_sn)$coefficients[2, "Pr(>|W|)"]
  
  #specificity
  data_sp <- data %>% filter(GT == 0)
  model_sp <- geeglm(Performance ~ Test, id = PatientID, data = data_sn, family = binomial(link = "identity"), corstr = "independence")
  p_sp <- summary(model_sp)$coefficients[2, "Pr(>|W|)"]
  
  #result
  result <- c(round(p_sn, 3), round(p_sp, 3))
  names(result) <- c("p_sn", "p_sp")
  
  return(result)
}
