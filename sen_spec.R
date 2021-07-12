
SNSP<-function(truth,pred){
  #Sensitivity, specificity 95% CI
  tb<-confusionMatrix(data=factor(pred),ref=factor(truth),positive="1")$tab[2:1,2:1]
  
  sensitivity<-epi.tests(tb)$elements$sensitivity
  specificity<-epi.tests(tb)$elements$specificity
  
  result<-list(sensitivity=sensitivity,specificity=specificity)
  se_sp <- round(unlist(result),3)
  se_sp <- c(se_sp[1], paste0("(",se_sp[2],", ",se_sp[3],")"),
             se_sp[4], paste0("(",se_sp[5],", ",se_sp[6],")"))
  names(se_sp) <- c("Sensitivity", "95% CI_sen", "Specificity", "95% CI_spec") 
  return(se_sp)
}
