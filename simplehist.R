#------------------------------------------------------------------------------
# Genera un histograma del vector "x" entre "xmin" y "xmax", normalizando el
# área del histograma a la unidad para poder superponer fácilmente funciones de
# densidad.
#------------------------------------------------------------------------------
# Parámetros de la función:
# x.....: vector cuyo histograma queremos calcular; es el único parámetro
#         estrictamente indispensable para invocar esta función
# nbins.: número de intervalos (bins)
# colour: color
# xmin, xmax: valores entre los que se calculará el histograma y se dibujará la
#         gráfica en el eje X
# ymin, ymax: valores entre los que se dibujará la gráfica en el eje Y
# xlab, ylab: etiquetas para los ejes X e Y
# main..: etiqueta para la gráfica (parte superior)
# log...: si es igual a "y" utiliza escala logarítmica en el eje Y; cualquier
#         otro valor es ignorado
# add...: si es igual a TRUE, el histograma se dibuja superpuesto a una gráfica
#         ya existente
#------------------------------------------------------------------------------
simplehist <- function(x, nbins=10, colour="black", 
                       xmin=NULL, xmax=NULL, 
                       ymin=NULL, ymax=NULL, 
                       xlab=NULL, ylab=NULL, main=NULL,
                       log=NULL, add=FALSE){
  #----------------------------------------------------------------------------
  # COMPROBACIONES INICIALES
  # Si x no es un vector, imprime mensaje de error y retorna
  if (!is.vector(x)) {
    stop("argument \"x\" is not a vector.")
  }
  # Si x no es un vector numérico, imprime mensaje de error y retorna
  if (!is.numeric(x)) {
    stop("argument \"x\" is not numeric.")
  }
  #----------------------------------------------------------------------------
  # Si no se especifican, determinamos xmin y xmax
  if (is.null(xmin)) {
    xmin <- min(x)
  }
  if (is.null(xmax)) {
    xmax <- max(x)
  }
  if (xmax == xmin){
    cat("WARNING: xmin == xmax. Modifying limits.\n")
    xmin <- xmin-1
    xmax <- xmax+1
  }
  if (xmax < xmin) {
    stop("xmin is greater than xmax.\n")
  }
  # Si no se especifica, la etiqueta del eje X será el nombre de la variable
  if (is.null(xlab)) {
    xlab <- deparse(substitute(x))
  }
  # Si no es especifica, se establece una etiqueta por defecto para el eje Y
  if (is.null(ylab)) {
    ylab <- "Densidad de probabilidad"
  }
  # Se establece si la escala en el eje Y debe ser logarítmica (base 10)
  if(is.null(log)){
    ylog10 <- FALSE
  } else {
    if (log == "y") {
      ylog10 <- TRUE
    } else {
      cat("WARNING: unexpected \"log=",log,"\". Ignoring parameter.\n", sep="")
      ylog10 <- FALSE
    }
  }
  # Anchura de los intervalos (igual para todos)
  mywidth <- (xmax-xmin)/nbins
  # Límites de los intervalos
  mybreaks <- seq(xmin,xmax,by=mywidth)  
  # Centros de los intervalos (marcas de clase)
  mycenters <- seq(xmin+mywidth/2,xmax-mywidth/2,length.out=nbins)
  # Determinamos el número de elementos en cada intervalo
  fx <- cut(x, breaks=mybreaks)
  tx <- table(fx)
  # Normalizamos para que el área total del histograma sea la unidad
  txnorm <- tx/length(x)/mywidth
  # Determinamos límites para la gráfica en el eje Y
  if (ylog10){
    tx_ok <- txnorm[txnorm > 0]  # evitamos tomar logaritmos de cero
    if (is.null(ymin)) ymin <- min(tx_ok)*0.95
    if (is.null(ymax)) ymax <- max(tx_ok)*1.05
  } else {
    if (is.null(ymin)) ymin <- min(txnorm)
    if (is.null(ymax)) ymax <- max(txnorm)*1.05
  }
  # Si add=FALSE, dibujamos una gráfica nueva
  if (!add){
    plot.new()
    if (ylog10) {
      plot.window(xlim=c(xmin,xmax), ylim=c(ymin,ymax),log="y")
    } else {
      plot.window(xlim=c(xmin,xmax), ylim=c(ymin,ymax))
    }
    axis(1)
    axis(2)
    box()
    title(xlab=xlab)
    title(ylab=ylab)
    if(!is.null(main)) title(main=main)
  }
  # Dibujamos cada "bin" del histograma
  for (i in 1:nbins){
    x1 <-  mycenters[i]-mywidth/2
    x2 <-  mycenters[i]+mywidth/2
    if (ylog10) {
      y1 <- ymin 
    } else {
      y1 <- 0
    }
    y2 <- txnorm[i]
    if(y2 > y1) lines(c(x1,x1,x2,x2),c(y1,y2,y2,y1),col=colour)
  }
  # Generamos una lista imitando los valores que devuelve el comando "hist"
  output <- list(breaks=mybreaks, 
                 counts=as.numeric(tx),
                 intensities=as.numeric(txnorm),
                 density=as.numeric(txnorm),
                 mids=mycenters,
                 xname=deparse(substitute(x)),
                 equidist=TRUE)
  # Retornamos la lista en modo invisible: no se imprime en pantalla pero si se
  # define una variable como resultado de la ejecución de esta función, dicha
  # variable será una lista con el contenido anterior.
  return(invisible(output))
}