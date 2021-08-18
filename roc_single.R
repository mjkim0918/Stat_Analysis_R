library(pROC);library(tidyverse)

# single ROC curve

roc_single <- function(obj, ci) {
  # obj : roc object with pROC::roc
  # ci : with ci -> ci = TRUE, without ci -> ci = FALSE
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
    ) + coord_equal() + ggtitle(capture.output(obj$ci))
  
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




#####################
##example
#####################


data <- read.csv('/Users/lunit/Documents/Statistical Analysis/RSNA/RSNA 2021/SRM/data/vega_dbt.csv')

head(data)

actual <- ifelse(data$Label == "cancer", 1, 0)
BaselineScore <- data$BaselineScore
SRMScore <- data$SRMScore



roc1 <- roc(actual, BaselineScore, ci = T, plot = F)
roc2 <- roc(actual, SRMScore, ci = T, plot = F)

roc_single(roc1, ci = F)
roc_single(roc1, ci = T)



