plotMagnitudes <- function(xytList)
{
   mag <- magnitude(xytList)
   ylim <- range(unlist(mag), na.rm=TRUE)
   xlim <- do.call(range, lapply(xytList, function(xyt)xyt$time))
   plot(0, 0, type="n", xlim=xlim, ylim=ylim, xlab="", ylab="")
   for(i in seq_along(mag)) {
     points(xytList[[i]]$time, mag[[i]], pch=i-1L, col=i)
   }
}

plotSpeed <- function(xytList, ...)
{
   #opar <- par(cex=0.5, mar=c(4.5,4.5,0.5,0.5))
   #on.exit(par(opar))
   d1 <- ddt(xytList, deriv=1, ...)
   plotMagnitudes(d1)
   title(xlab="Time (s)", ylab="Speed (ft/s)")
}
plotAcceleration <- function(xytList, ...)
{
   #opar <- par(cex=0.5, mar=c(4.5,4.5,0.5,0.5))
   #on.exit(par(opar))
   d2 <- ddt(xytList, deriv=2, ...)
   plotMagnitudes(d2)
   title(xlab="Time (s)", ylab="Acceleration (ft/s/s)")
}
