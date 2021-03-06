probcumple <- function(n) {
  # Calcula la probabilidad de que haya una coincidencia de cumplea�os en un grupo
  # de 'n' personas. 
  #
  # Args: 
  #   n: N�mero de personas para las que calcula la probabilidad.
  #
  # Returns: 
  #   La probabilidad de que haya coincidencia de cumplea�os. 
  # La probabilidad siempre ser� 1 para n > 365. As� evitamos errores: 
  if (n>365){
    return(1)
  }
  # Error handling (esta funci�n solo acepta valores positivos)
  if (n < 0) {
    stop("El argumento tiene que ser positivo, y el actual es: ", n)
  }
  days <- 1:365/365
  gente <- numeric(365)
  gente[1:(365-n)] <- 1:(365-n)/365
  days[days==gente] <- 1
  prob <- prod(days)
  return(1-prob)
}