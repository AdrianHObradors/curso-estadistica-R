simulation2 <- function(npeople, nsimulation = 10000) {
  nmatch <- c()
  for (i in 1:nsimulation) {
    gente <- sample(365, size = npeople, replace = TRUE)
    nmatch[i] <- sum(duplicated(gente))
  }
  return(table(nmatch))
}

# Lo siguiente crea una lista con tablas del largo de la mayor

lalista <- list(a = simulation2(15), b = simulation2(25), c = simulation2(75))
largo <- max(unlist(lapply(lalista,length)))
for (i in 1:length(lalista)) {       # Intenté con "for (i in lalista)", pero no me funcionaba...
  corto <- length(lalista[[i]])
  if (corto != largo) {
    lalista[[i]][(corto + 1):largo] <- 0
    names(lalista[[i]])[(corto + 1):largo] <- (corto + 1):largo
  }
}
tablafinal <- rbind(lalista$a, lalista$b, lalista$c)
barplot(100*tablafinal/10000, ylab = "Porcentaje (%)",  # Falta ponerla bonita
        xlab = "Coincidencias", beside = TRUE)