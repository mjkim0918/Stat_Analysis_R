

confidence_interval_calc <- function(alpha, avg, number_of_data) {
  z_value <- qnorm ((1-alpha/2))
  confidence_interval <- z_value * sqrt((avg * (1 - avg))/number_of_data)
  upper <- round(avg + confidence_interval, 3)
  lower <- round(avg - confidence_interval, 3)
  return(paste0("(", lower, ", ", upper, ")"))
}

average_snsp <- function(alpha, snsp, number_of_data){
  #snsp : vector of Sensitivity or Specificity
  avg <- mean(snsp)
  CI <- confidence_interval_calc(alpha, avg, number_of_data)
  ave_ci <- paste0(round(avg,3), CI)
  result <- list(ave_ci = ave_ci, average = round(avg,3), CI = CI)
  return(result)
}
