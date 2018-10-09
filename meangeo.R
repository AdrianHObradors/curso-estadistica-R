meangeo <- function(x, na.rm = FALSE) {
  # Devuelve la media geométrica de x
  
  # Eliminamos NA
  if (sum(is.na(x)>0)){
    if (na.rm == TRUE) {
      x <- x[!is.na(x)]
    } else {
      return(NA)
    }
  }
  # media geométrica
  if (prod(x) > 0) {
    return(prod(x)^(1/(length(x))))
  } else {
    if (length(x) %% 2 == 1) {
      -prod(abs(x))^(1/(length(x)))
    } else {
      NaN
    }
  }
}
