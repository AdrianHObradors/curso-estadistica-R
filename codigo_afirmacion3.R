# codigo_afirmacion3.R, por Adrian H. Obradors
source("simplehist.R")
afirm3 <- function(nsimul, n, mu = 0, sigma = 1) {
  muestra <- as.list(rep(sigma, nsimul))  # Creamos lista de sigmas de largo nsimul.
  # Y la convertimos en muestras dentro de un data.frame.  
  df <- data.frame(muestra = cbind(lapply(muestra, rnorm, n = n, mean = mu)))
  df$media <- vapply(df$muestra, mean, numeric(1))  # We add the mean. 
  df$var <- vapply(df$muestra, var, numeric(1))  # The variance. 
  df$chisq <- (n - 1) * (df$var / (sigma ^ 2))  # And chi squared. 
  return(df)
}
# Aquí ya directamente trabajamos los default de la función
n3 <- afirm3(10000, 3)
n6 <- afirm3(10000, 6)
n11 <- afirm3(10000, 11)
# Lo siguiente crea una para representar hasta los extremos, para cada caso. 
xmin <- sapply(list(n3$chisq, n6$chisq, n11$chisq), min) - 1
xmax <- sapply(list(n3$chisq, n6$chisq, n11$chisq), max) + 1

simplehist(n3$chisq, nbins = 60, xmin = xmin[1], xmax = xmax[1],
           xlab = expression(paste(chi^2, " , n = 3")))
curve(dchisq(x, df = 2), add = TRUE, lwd = 1.5, col = "red", lty = 2)
legend(5, 0.4, legend = expression(chi[2]^2), lty = 2, col = "red", cex = 0.8,
       bty = "n")

simplehist(n6$chisq, nbins = 60, xmin = xmin[2], xmax = xmax[2],
           xlab = expression(paste(chi^2, " , n = 6")))
curve(dchisq(x, df = 5), add = TRUE, lwd = 1.5, col = "red", lty = 2)
legend(10, 0.14, legend = expression(chi[5]^2),
       lty = 2, col = "red", cex = 0.8, bty = "n")

simplehist(n11$chisq, nbins = 60, xmin = xmin[3], xmax = xmax[3],
           xlab = expression(paste(chi^2, " , n = 11")))
curve(dchisq(x, df = 10), add = TRUE, lwd = 1.5, col = "red", lty = 2)
legend(15, 0.085, legend = expression(chi[10]^2),
       lty = 2, col = "red", cex = 0.8, bty = "n")