


accuracy<-function(truth,pred){
  #Accuracy 95% CI
  if(sum(dim(table(truth,pred))) == 4){
    tb<-confusionMatrix(data=factor(pred),ref=factor(truth),positive="1")$tab[2:1,2:1]
    acc<-epi.tests(tb)$elements$diag.acc
    accuracy <- round(as.numeric(acc[1]),4)
    accuracy_ci <- paste0("(", round(acc[2],4), ", ", round(acc[3],4), ")")
  } else if(sum(dim(table(truth,pred))) < 4){
    if(sum(truth) == 0){ #event 가 하나도 없는 데이터셋인 경우 커버됨 
      event <- sum(trut == pred)
      n <- length(truth)
      accuracy <- round(binom.test(event, n)$estimate, 4)
      accuracy_ci <- paste0("(", round(binom.test(event, n)$conf.int[1],4), ", ", round(binom.test(event, n)$conf.int[1],4), ")")
    }
  }
  
  result <- list(accuracy = accuracy, accuracy_ci = accuracy_ci)
  return(result)
}
