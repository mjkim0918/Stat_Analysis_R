
paired_ttest<-function(T1, T2){
  mean_var1 <- paste(round(mean(T1, na.rm = T), 2), "±",round(var(T1, na.rm = T), 2))
  mean_var2 <- paste(round(mean(T2, na.rm = T), 2), "±",round(var(T2, na.rm = T), 2))
  min_max1 <- paste0(min(T1), ", ", max(T1))
  min_max2 <- paste0(min(T2), ", ", max(T2))
  
  diff <- T1 - T2
  mean_var_diff <- paste(round(mean(diff, na.rm = T), 2), "±",round(var(diff, na.rm = T), 2))
  min_max_diff <- paste0(min(diff), ", ", max(diff))
  
  mean_var <- c(mean_var1, mean_var2, mean_var_diff)
  median <- c(median(T1), median(T2), median(diff))
  min_max <- c(min_max1, min_max2, min_max_diff)
  
  pvalue <- round(t.test(diff)$p.value, 4)
  
  result <- cbind(rbind(mean_var, median, min_max), c(pvalue, "-", "-"))
  colnames(result) <- c("T1", "T2", "Diffence", "P value")
  
  return(result)
}
