meanarm <- function(x, na.rm = FALSE) {
  # Returns the armonic mean of x. 
  return(1/mean(1/x, na.rm = na.rm))
}
