library(pROC);library(tidyverse)


roc_single_point <- function(obj, ci, point_data) {

  ciobj <- ci.se(obj, specificities = seq(0, 1, l = 25))
  dat.ci <- data.frame(x = as.numeric(rownames(ciobj)),
                       lower = ciobj[, 1],
                       upper = ciobj[, 3])
  
  p <- ggroc(obj) +
    theme_minimal() +
    geom_abline(
      slope = 1,
      intercept = 1,
      linetype = "dashed",
      alpha = 0.7,
      color = "grey"
    ) + coord_equal() + ggtitle(capture.output(obj$ci))+
    geom_point(data = point_data, aes(x = specificity, y = sensitivity, shape = Reader))
  
  if(ci == TRUE) {
    p <- p +
      geom_ribbon(
        data = dat.ci,
        aes(x = x, ymin = lower, ymax = upper),
        fill = "steelblue",
        alpha = 0.2
      ) 
  }
  return(p)
}

################
##example
################

point_data <- data.frame(Reader = c("Reader 1", "Reader 2", "Reader 3"), 
                         specificity = c(0.5, 0.6 ,0.7), 
                         sensitivity = c(0.6, 0.7, 0.8))

roc_single_point(roc1, ci = F, point_data)
roc_single_point(roc1, ci = T, point_data)
