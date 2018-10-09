skew <- function(x) {
  # Gives the Fisher's skewnewss of a dataset
  #
  # Returns skewness and uncertainty
  x <- x[!is.na(x)]
  m3 <- sum((x - mean(x))^3)/length(x)
  g1 <- m3/sd(x)^3
  dg1 <- sqrt(6/length(x))
  if (g1 >0) {
    skewdto <- "right"
  } else {
    skewdto <- "left"
  }
  if (abs(g1) > dg1) {
    print(paste("The dataset is skewed to the", skewdto))
  } else {
    print("The skewness is imperceptible")
  }
  return(c("Skew" = g1, "+-" = dg1))
}