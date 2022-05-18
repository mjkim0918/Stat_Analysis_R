paired_ttest<-function(T1, T2){
  mean_sd1 <- paste(round(mean(T1, na.rm = T), 2), "±",round(sd(T1, na.rm = T), 2))
  mean_sd2 <- paste(round(mean(T2, na.rm = T), 2), "±",round(sd(T2, na.rm = T), 2))
  min_max1 <- paste0(min(T1), ", ", max(T1))
  min_max2 <- paste0(min(T2), ", ", max(T2))
  IQR1 <- paste0(round(quantile(T1, 0.25),4), ", ", round(quantile(T1, 0.75),4))
  IQR2 <- paste0(round(quantile(T2, 0.25),4), ", ", round(quantile(T2, 0.75),4))
  
  diff <- T1 - T2
  mean_sd_diff <- paste(round(mean(diff, na.rm = T), 2), "±",round(sd(diff, na.rm = T), 2))
  min_max_diff <- paste0(min(diff), ", ", max(diff))
  IQR_diff <- paste0(round(quantile(diff, 0.25),4), ", ", round(quantile(diff, 0.75),4))
  
  mean_sd <- c(mean_sd1, mean_sd2, mean_sd_diff)
  median <- c(median(T1), median(T2), median(diff))
  min_max <- c(min_max1, min_max2, min_max_diff)
  IQR <- c(IQR1, IQR2, IQR_diff)
  
  pvalue <- round(t.test(diff)$p.value, 4)
  
  result <- cbind(rbind(mean_sd, median, min_max, IQR), c(pvalue, "-", "-","-"))
  colnames(result) <- c("T1", "T2", "Difference", "P value")
  
  return(result)
}
