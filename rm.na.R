rm.na <- function(..., rm.cola = TRUE){
  # Limpia data frames, listas, o casi cualquier otra cosa de valores NA, y 
  # quita las filas asociadas.
  #
  # Acepta vectores individuales o una lista de vectores.
  # Si los vectores no tienen la misma longitud,  y rm.cola = TRUE, se quita la cola de los más largos
  vectors <- list(...)
  # Comprobación de si lo introducido es una lista (y deslistamiento en tal caso)
  remember <- FALSE
  remember2 <- FALSE
  if (is.data.frame(vectors[[1]])) {remember <- TRUE}
  if (length(vectors) == 1 && is.list(vectors[[1]])){
    vectors <- vectors[[1]]
  } else if (length(vectors) == 1) {
    remember2 <- TRUE
  }
  # Comprobación de longitud de los vectores y eliminación de cola
  long <- sapply(vectors, length)
  if (!rm.cola && max(long) != min(long)) {
    stop("Los vectores han de tener la misma longitud")
  } else {
    for (i in 1:length(vectors)) {
      vectors[[i]] <- vectors[[i]][1:min(long)]
    }
  }
  
  # Vectores que tienen NA
  rem <- numeric(length(vectors[[1]]))
  for (i in 1:length(vectors)){
    rem <- rem + is.na(vectors[[i]])
  }
  # Y selección final
  sinna <- list()
  for (i in 1:length(vectors)){
    sinna[[i]] <- vectors[[i]][rem == 0]
  }
  if (remember) {sinna <- data.frame(sinna)}
  if (remember2) {sinna <- unlist(sinna)}
  names(sinna) <- names(vectors)
  return(sinna)
}