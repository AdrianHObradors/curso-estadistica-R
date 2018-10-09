meanarm <- function(x, na.rm = FALSE) {
  return(1/mean(1/x, na.rm = na.rm))
}