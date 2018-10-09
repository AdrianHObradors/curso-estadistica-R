newton <- function(fun, x0 = 0, maxiter = 100, name = "x", tol = 10^-6,
                   tol2 = NULL, plot = FALSE, from = x0 - 5, to = x0 + 5, ylim = NULL) {
  # Usa el método de newton para encontrar los ceros de una función
  # Se podría trabajar más, pero está chulo
  # 
  # Args:
  #   fun: Function you want to find zeros for, inside " "
  #   x0: Where the method starts
  #   maxiter: If the number of iterations reaches this number, the method will stop
  #   name: Name of variable
  #   tol: If the x changes less than tol after using the method, it stops
  #   tol2: If the value reaches lower than tol2, method stops
  #   plot: If TRUE, it draws a nice graph with the method
  #    from: From where you want to draw the graph
  #    to: until where you want to draw the graph
  #    ylim: The ylim for the graph drawing
  #
  #
  # Returns:
  #   The x value where the method stops, the value of the function (hopefully zero)
  #   and how many iterations it took the method.
  #   Also if plot=TRUE it will draw the graph with the iterations.
  fun <- parse(text = fun)  # Necessary to derivate the function next
  fun.dx <- D(fun, name = name)
  if (plot) {
    curve(eval(fun, envir = list(x = x)), from = from, to = to, ylim = ylim)
    abline(h = 0, lty = 2, col = "red")
  }
  # The actual method follows:
  for (n in 1:maxiter) {
    value <- eval(fun, list(x=x0))
    value.dx <- eval(fun.dx, list(x=x0))
    x1 <- x0 - value/value.dx
    # cat("x = ", x0, ", value = ", value, ", n = ", n, "\n", sep = "")
    if (abs(x1 - x0) <= tol) {
      break
    }
    if (plot) {
      abline(value - value.dx * x0, value.dx, col = n)
      #abline(v = x0, col = n)
      Sys.sleep(0.5)
    }
    x0 <- x1
    if (!is.null(tol2) && abs(value) < tol2){
      break
    }
  }
  if (n == maxiter){
    warning("The maximum number of iterations was reached.")
  }
  if (plot){
    legend("topright", legend = 1:n, lty = 1, col = 1:n, cex = 0.6)
  }
  return(c("x1" = x1, "F(x1)" = value, "n" = n))
}