library(pROC);library(tidyverse)

# function for drawing more than 1 ROC curve at once


roc_multi <- function(roc.list, ci){ 
  # roc.list : roc object list, roc with pROC::roc
  # ci : with ci -> ci = TRUE, without ci -> ci = FALSE
  ci.list <- lapply(roc.list, ci.se, specificities = seq(0, 1, l = 25))
  
  dat.ci.list <- lapply(ci.list, function(ciobj) 
    data.frame(x = as.numeric(rownames(ciobj)),
               lower = ciobj[, 1],
               upper = ciobj[, 3]))
  
  p <- ggroc(roc.list) + theme_minimal() + geom_abline(slope=1, intercept = 1, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal()
  
  
  for(i in 1:length(roc.list)) {
    ci_label <- paste(str_sub(capture.output(roc.list[i])[1],2),capture.output(roc.list[i])[8])
    p <- p + annotate(geom = "text", label = ci_label, x = 0.25, y = (0.25 + 0.05*(1-i)), size = 4)
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

#############
##example
#############

roc.list <- list(BaselineScore = roc1, SRMScore = roc2)
roc_multi(roc.list, ci = F)
roc_multi(roc.list, ci = T)

