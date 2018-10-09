CV <- function(x, na.rm = FALSE){
  # Calcula el coeficiente de variación de x
  #
  # Args: 
  #   x: Vector sobre el cual se va a calcular el coeficiente de variación
  #   na.rm: If FALSE, it returns NA if x contains NA. If TRUE, it removes it.
  #
  # Returns: 
  #   CV: Coeficiente de Variación
  CV <- sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
  return(CV)
}
