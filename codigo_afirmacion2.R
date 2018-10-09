# codigo_afirmacion2.R, por Adrian H. Obradors
source("simplehist.R")
afirm2 <- function(nsimul, n, mu = 0, sigma = 1) {
  muestra <- as.list(rep(sigma, nsimul))  # Creamos lista de sigmas de largo nsimul. 
  # Y la convertimos en muestras dentro de un data.frame.  
  df <- data.frame(muestra = cbind(lapply(muestra, rnorm, n = n, mean = mu)))
  df$media <- vapply(df$muestra, mean, numeric(1))  # We add the mean. 
  
  # Lo siguiente es la aplicación de la función sd(), hasta que me di cuenta 
  # de que sd()... Las dos son lo mismo, pero la de R es más clara visualmente. 
  #
  # df$s <- sqrt(mapply(function(x, y) {sum((x-y)^2) / (n-1)},
  #                     x = df$muestra, y = df$media))
  
  df$s <- vapply(df$muestra, sd, numeric(1))
  df$t <- (df$media - mu) / (df$s / sqrt(n))  # Y terminamos con t. 
  return(df)
}
# Es un poco rollo preguntar siempre por mu y sigma así que lo siguiente
# elige uno al azar.
mu <- rnorm(1, 0, 100)
sigma <- abs(rnorm(1, 0, 100))

n3 <- afirm2(10000, 3, mu, sigma)
n6 <- afirm2(10000, 6, mu, sigma)
n11 <- afirm2(10000, 11, mu, sigma)

simplehist(n3$t, nbins = 40, xmin = -5, xmax = 5, xlab = "t, n = 3")
curve(dt(x, df = 2), add = TRUE, col = "red")
legend(2, 0.3, legend = expression(t[2]), lty = 1, col = 2, cex = 0.8, bty = "n")

simplehist(n6$t, nbins = 40, xmin = -5, xmax = 5, xlab = "t, n = 6") 
curve(dt(x, df = 5), add = TRUE, col = "red")
legend(2, 0.3, legend = expression(t[5]), lty = 1, col = 2, cex = 0.8, bty = "n")

simplehist(n11$t, nbins = 40, xmin = -5, xmax = 5, xlab = "t, n = 11") 
curve(dt(x, df = 10), add = TRUE, col = "red")
legend(2, 0.3, legend = expression(t[10]), lty = 1, col = 2, cex = 0.8, bty = "n")