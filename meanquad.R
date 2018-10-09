meanquad <- function(x, na.rm = FALSE) {
  return(sign(sum(x, na.rm = TRUE))*mean(x^2, na.rm = na.rm)^(1/2))
}