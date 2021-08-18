
library(pROC);library(tidyverse)


# function for drawing more than 1 ROC curve at once with point of reader's performances(sensitivity, specificity)
roc_multi_point <- function(roc.list, ci, point_data){ 
  # roc.list : roc object list, roc with pROC::roc
  # ci : with ci -> ci = TRUE, without ci -> ci = FALSE
  # point_data : data frame with column names "Test", "Reader", "sensitivity", "specificity"
  ci.list <- lapply(roc_list, ci.se, specificities = seq(0, 1, l = 25))
  
  dat.ci.list <- lapply(ci.list, function(ciobj) 
    data.frame(x = as.numeric(rownames(ciobj)),
               lower = ciobj[, 1],
               upper = ciobj[, 3]))
  
  p <- ggroc(roc.list) + theme_minimal() + geom_abline(slope=1, intercept = 1, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal()
  
  
  for(i in 1:length(roc.list)) {
    ci_label <- paste(str_sub(capture.output(roc.list[i])[1],2),capture.output(roc.list[i])[8])
    p <- p + annotate(geom = "text", label = ci_label, x = 0.25, y = (0.25 + 0.05*(1-i)), size = 4)+
      geom_point(data = point_data, aes(x = specificity, y = sensitivity, shape = Reader, color = Test)) 
  } 
  

  
  if(ci == TRUE){
    for(i in 1:length(roc.list)) {
      p <- p + geom_ribbon(
        data = dat.ci.list[[i]],
        aes(x = x, ymin = lower, ymax = upper),
        fill = i + 1,
        alpha = 0.2,
        inherit.aes = F)
    }
  }
  return(p)
}

#####################
##example
#####################

point_data <- data.frame(Test = c(rep("without AI",3), rep("with AI", 3)), Reader = c("Reader 1", "Reader 2", "Reader 3"), 
                         specificity = c(0.5, 0.6 ,0.7,0.7, 0.8, 0.8), 
                         sensitivity = c(0.6, 0.7, 0.8,0.7, 0.8, 0.9))
roc_multi_point(roc.list, ci = F, point_data)
roc_multi_point(roc.list, ci = T, point_data)
