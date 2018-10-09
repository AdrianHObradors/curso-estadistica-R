# Ejercicio 4 - Adri�n H. Obradors
correl.pares <- function(df) {
  # Checks for correlation between different columns of a data set.
  num <- 0
  for (i in 1:(ncol(df)-1)) {
    for (j in (i + 1):ncol(df)) {
       correl <- cor.test(df[[i]], df[[j]], method = "spearman")
       if (correl$p.value < 0.01) {
         cat("Se ha encontrado correlaci�n entre", names(df[i]), "y", names(df[j]), 
             "con un nivel de significaci�n de", correl$p.value, "\n")
         num <- num + 1
       }
    }
  }
  return(num)
}