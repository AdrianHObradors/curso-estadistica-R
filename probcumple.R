probcumple <- function(n) {
  # Calcula la probabilidad de que haya una coincidencia de cumpleaños en un grupo
  # de 'n' personas. 
  #
  # Args: 
  #   n: Número de personas para las que calcula la probabilidad.
  #
  # Returns: 
  #   La probabilidad de que haya coincidencia de cumpleaños. 
  # La probabilidad siempre será 1 para n > 365. Así evitamos errores: 
  if (n>365){
    return(1)
  }
  # Error handling (esta función solo acepta valores positivos)
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