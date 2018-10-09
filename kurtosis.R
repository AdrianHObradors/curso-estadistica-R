kurtosis <- function(x) {
  # Gives the kurtosis of a dataset
  #
  # Returns:
  #   Kurtosisness and uncertainty of x
  #
  # Removing NA from vector
  x <- x[!is.na(x)]
  # Calculating kurtosis and uncertainty
  m4 <- sum((x - mean(x))^4)/length(x)
  g2 <- m4/sd(x)^4
  dg2 <- sqrt(24/length(x))
  # Checks the kurtosis and prints accordingly
  if (g2 >3) {
    kurtosism <- "leptocurtic"
  } else {
    kurtosism <- "platocurtic"
  }
  if (abs(g2-3) > dg2) {
    print(paste("The dataset has a", kurtosism, "shape"))
  } else {
    print("The dataset has a mesocurtic shape")
  }
  return(c("Kurtosis" = g2, "+-" = dg2))
}