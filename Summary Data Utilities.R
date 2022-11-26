t.test.from.summary.data <- function(mean1, sd1, n1, mean2, sd2, n2, ...) {
  data1 <- scale(1:n1)*sd1 + mean1
  data2 <- scale(1:n2)*sd2 + mean2
  t.test(data1, data2, ...)
}


Data_from_summary_Stats <- function(mean1, sd1, n1) {
  
  data1<- scale(1:n1)*sd1 + mean1
  return(data1)
  
  
}