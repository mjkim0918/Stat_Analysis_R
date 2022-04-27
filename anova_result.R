library(dplyr);library(tidyverse);
anova_result <- function(y, group, data){
  anova <- aov(y ~ group, data = data)
  sum_anova <- summary(anova)
  p_value <- c(if_else(unlist(sum_anova)['Pr(>F)1']<0.0001,"< 0.0001", as.character(round(unlist(sum_anova)['Pr(>F)1']),4)),"-","-","-","-")
  names(p_value) <- "p_value"
  
  
  sum_tb <- data %>% 
    group_by(group) %>% 
    summarise(n = n(), 
              mean_sd = paste(round(mean(y, na.rm = T), 2), "±",round(sd(y, na.rm = T), 2)),
              median = round(median(y),4), min_max = paste0(round(min(y),4), ", ", round(max(y),4)),
              IQR = paste0(round(quantile(y, 0.75),4), ", ", round(quantile(y, 0.25),4)))
  
  sum_tb_t <- t(sum_tb)
  colnames(sum_tb_t) <- names(table(data$group))
  sum_tb_t <- sum_tb_t[-1,]
  sum_tb_t <- rownames_to_column(as.data.frame(sum_tb_t))
  result <- bind_cols(sum_tb_t, p_value)
  colnames(result) <- c("_", names(table(data$group)), "p_value")
  return(result)
}

###example
anova_result(df_ABC$cancer_score, df_ABC$group, df_ABC) ##y, group format should be -> data$y, data$group
