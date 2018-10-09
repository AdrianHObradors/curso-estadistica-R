# codigo_afirmacion1.R, por Adrian H. Obradors
# source("simplehist.R")
afirm1 <- function(n, nsimul, mu = 0, sigma = 1, plotmuestra = FALSE) {
  # Calculates the standard score Z of a simulated sample and returns a 
  # data frame with the sample and the mean and Z for each sample. 
  # Args: 
  #   n: Number of samples to be taken in each simulation
  #   nsimul: Number of simulations to be realized 
  #   mu: The mean of the population from which the samples are taken. Default is 0
  #   sigma: The standard deviation of the population. Default is 1
  #   plotmuestra: If TRUE, a graphic of the samples is printed, showing also the 
  #                individual mean and total mean of the samples. 
  #
  # Returns: 
  #   A dataframe w/ the sample, mean and standard score Z for each simulation. 
  muestra <- as.list(rep(sigma, nsimul))  # Creamos lista de sigmas de largo nsimul. 
  muestra <- lapply(muestra, rnorm, n = n, mean = mu) # Y la convertimos en muestras. 
  df <- data.frame(cbind(muestra))  # Añadimos la lista muestra a un df
  df$media <- vapply(muestra, mean, numeric(1))  # Añadimos al df la media
  
  # Lo siguiente es para visualizar los datos de las simulaciones (not necessary)
  if (plotmuestra) {
    # Plot de la media de cada valor, con los valores extremos de las muestras como 
    # límites. 
    plot(df$media, ylim = c(min(unlist(df$muestra)) - 1, max(unlist(df$muestra)) + 1),
         type = "l", lwd = 2) 
    # Añadimos los puntos individuales de las muestras, en rojo y semitransparentes. 
    for (i in 1:length(df$muestra)) {
      points(y = unlist(df$muestra[i]), x = rep(i, n), col = rgb(1,0,0,0.5), pch = 20,
             cex = 0.8)
    }
    lines(df$media, lwd= 2) # Rerepresentamos la línea para que esté encima
    # Y ponemos una horizontal para la media total. 
    abline(mean(df$media), 0, col = "blue", lty = 2, lwd = 1.5)
  }
  df$Z <- (df$media - mu) / (sigma / sqrt(n)) # Cálculo del valor Z
  
  # Devolvemos el df invisible para cuando queremos solo visualizar los datos. 
  if (plotmuestra) {
    return(invisible(df))
  } else {
    return(df)
  }
}

# A continuación hacemos varias simulaciones para distintos tamaños de muestra, 
# y los representamos en un histograma. 
par(mfrow = c(1,1))
# Queremos demostrar que Z sigue una distrubución N(1,0) para cualquier población,  
# lo siguiente pregunta al humano un valor de mu 
mu <- as.numeric(readline("Elige la media de la población: "))
sigma <- as.numeric(readline("Elige la desviación típica de la población: "))
if (sigma < 0) {
  warning("La desviación típica ha de ser positiva. Se ha tomado su valor absoluto.")
  sigma <- abs(sigma)
} else if (sigma == 0) {
  warning("La desviación típica ha de ser distanta a cero.
  Se prosigue con el valor sigma = 1 ")
  sigma <- 1
}

n3 <- afirm1(3, 10000, mu, sigma)
n6 <- afirm1(6, 10000, mu, sigma)
n11 <- afirm1(11, 10000, mu, sigma)

xmin <- min(n3$Z)
xmax <- max(n3$Z)

simplehist(n3$Z, nbins = 50, col = 3, xmin = xmin, xmax = xmax, xlab = "Z")
simplehist(n6$Z, nbins = 50, col = 2, add = TRUE, xmin = xmin, xmax = xmax)
simplehist(n11$Z, nbins = 50, col = 1, add = TRUE, xmin = xmin, xmax = xmax)

curve(dnorm(x), add = TRUE, lwd = 1.5, col = "red", lty = 2)

legend(xmax - 1.2, 0.42, legend = c("n = 3", "n = 6", "n = 11"),
       fill = 3:1, cex = 0.7, y.intersp = 0.7, bty = "n")
legend(xmax - 1.4, 0.3, legend = c("N(0,1)"),
       lty = 2, col = "red", cex = 0.65, bty = "n")