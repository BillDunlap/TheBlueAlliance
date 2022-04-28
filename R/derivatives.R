magnitude <- function(xyt) {
  multiple <- !is.data.frame(xyt) && is.list(xyt)  && all(vapply(xyt, is.data.frame, NA))
  if (multiple) {
    lapply(xyt, magnitude)
  } else {
    stopifnot(is.data.frame(xyt), "x" %in% names(xyt), "y" %in% names(xyt))
    sqrt(xyt$x^2 + xyt$y^2)
  }
}

derivViaLocalPolynomial <- function(x, deltaT, halfWidth, maxDeriv=2) {
  # estimate derivatives 0 through maxDeriv with respect to time
  # by fitting a polynomial to a running window of x.
  stopifnot(deltaT>0, maxDeriv>=0) # check halfWidth too
  width <- halfWidth * 2 + 1
  t <- seq(-halfWidth, halfWidth) * deltaT
  d <- 0:maxDeriv
  names(d) <- paste0("d", d)
  stopifnot(length(t)==width)
  A <- outer(t, d, function(t,d)t^d/factorial(d))
  stopifnot(nrow(A)==width, ncol(A)==maxDeriv+1)
  fMat <- solve( crossprod(A), t(A) ) # (A'A)^-1 * t(A)
  stopifnot(nrow(fMat)==maxDeriv+1, ncol(fMat)==width)
  fMat <- fMat[,width:1,drop=FALSE] # reverse columns for convolutional filtering
  lapply(d, function(d)as.vector(filter(x, fMat[d+1,])))
}
ddt <- function(xyt, deriv=1, deltaT = diff(xyt$time[1:2]), halfWidth=3, maxDeriv=2) {
  # estimate derivative with respect to time.
  stopifnot(deriv <= maxDeriv)
  multiple <- !is.data.frame(xyt) && is.list(xyt)  && all(vapply(xyt, is.data.frame, NA))
  if (multiple) {
    lapply(xyt, ddt, deltaT = deltaT, halfWidth = halfWidth, maxDeriv=maxDeriv)
  } else {
    stopifnot(is.data.frame(xyt), identical(names(xyt), c("x", "y", "time")))
    stopifnot(diff(range(diff(xyt$time))) < 1e-10)
    data.frame(
        x = derivViaLocalPolynomial(xyt$x, deltaT = deltaT, halfWidth = halfWidth, maxDeriv=maxDeriv)[[deriv+1]],
        y = derivViaLocalPolynomial(xyt$y, deltaT = deltaT, halfWidth = halfWidth, maxDeriv=maxDeriv)[[deriv+1]],
        time = xyt$time)
  }
}
