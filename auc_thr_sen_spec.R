library(tidyverse);library(pROC);library(epiR);

auc_thr_sen <- function(y, pred, sen){
  roc <- pROC::roc(y,pred,plot=T)
  
  auc_ci <- round(ci.auc(roc),3)
  auc_res <- paste0(auc_ci[2],"(",auc_ci[1],", ",auc_ci[3],")")
  
  threshold_sen <- coords(roc) %>% 
    mutate(diff = sensitivity - sen) %>% 
    slice(which.min(abs(diff))) %>% 
    slice(which.max(specificity)) %>% 
    dplyr::select(-diff)
  
  table<-confusionMatrix(ref=factor(y),data=factor(ifelse(pred >= threshold_sen$threshold, 1, 0)),positive="1")
  n_positivie <- rowSums(table$table)[2] #cut off 적용결과 positivie 로 분류된 사람들
  n_true_positive <- table$table[2,2] # True positive
  Total_n_cancer <- colSums(table$table)[2]
  
  tb<-table$tab[2:1,2:1]
  sensitivity_sen <- round(epi.tests(tb)$elements$sensitivity,3)
  specificity_sen <- round(epi.tests(tb)$elements$specificity,3)
  threshold_sen <- threshold_sen %>%
    mutate(sensitivity = paste0(sensitivity_sen[1],"(",sensitivity_sen[2],", ",sensitivity_sen[3],")"),
           specificity = paste0(specificity_sen[1],"(",specificity_sen[2],", ",specificity_sen[3],")"),
           threshold = round(threshold, 3))
  
  
  result <- list(auc = auc_res, threshold_sen = threshold_sen, 
                 n_positivie =n_positivie, n_true_positive = n_true_positive, Total_n_cancer = Total_n_cancer)
  return(result)
}



auc_thr_spec <- function(y, pred, spec){
  roc <- roc(y,pred,plot=T)
  
  auc_ci <- round(ci.auc(roc),3)
  auc_res <- paste0(auc_ci[2],"(",auc_ci[1],", ",auc_ci[3],")")
  
  threshold_spec <- coords(roc) %>% 
    mutate(diff = specificity - spec) %>% 
    slice(which.min(abs(diff))) %>% 
    slice(which.max(sensitivity)) %>% 
    dplyr::select(-diff)
  
  table<-confusionMatrix(ref=factor(y),data=factor(ifelse(pred >= threshold_spec$threshold, 1, 0)),positive="1")
  n_positivie <- rowSums(table$table)[2] #cut off 적용결과 positivie 로 분류된 사람들
  n_true_positive <- table$table[2,2] # True positive
  Total_n_cancer <- colSums(table$table)[2]
  
  tb<-table$tab[2:1,2:1]
  sensitivity_spec <- round(epi.tests(tb)$elements$sensitivity,3)
  specificity_spec <- round(epi.tests(tb)$elements$specificity,3)
  threshold_spec <- threshold_spec %>%
    mutate(sensitivity = paste0(sensitivity_spec[1],"(",sensitivity_spec[2],", ",sensitivity_spec[3],")"),
           specificity = paste0(specificity_spec[1],"(",specificity_spec[2],", ",specificity_spec[3],")"),
           threshold = round(threshold, 3))
  
  
  result <- list(auc = auc_res, threshold_spec = threshold_spec, 
                 n_positivie =n_positivie, n_true_positive = n_true_positive, Total_n_cancer = Total_n_cancer)
  return(result)
}
