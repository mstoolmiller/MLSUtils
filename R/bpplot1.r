bpplot <- function (..., at = NULL, name = TRUE, xlab = "", ylab = "", srtx = 0, boxwex=1, xlim=NULL, plot.it = T, ylim=NULL, yaxt="s", tck=par("tck"), cex.axis=1.5*par("cex.axis"), 
	cex.lab=1.75*par("cex.lab"), points=T, point.size=2.5*par("cex"), point.color="black", point.pch=1, grid=F, median.width=2, quartile.width=2, tercile.width=2) 
{
    all.x <- list(...)
    nam <- character(0)
    if (is.list(all.x[[1]])) {
        all.x <- all.x[[1]]
        if (is.logical(name) && name) name <- names(...)
    }
    n <- length(all.x)
    if (is.null(at)) centers <- seq(from = 0, by = 1.2, length = n)
    else centers <- at
    if (is.null(ylim)) ylim <- range(unlist(all.x), na.rm=T)
    if (is.null(xlim)) xlim <- c(-0.5, max(centers) + 0.5)
    # set up the plot
    plot(c(0, 0), c(0, 0), xlim=xlim, ylim=ylim, type = "n", xlab = "", ylab = ylab, xaxt = "n", yaxt=yaxt, cex.axis=cex.axis, cex.lab=cex.lab, tck=tck)
    # add a grid
    if (grid) grid(nx=0, ny=NULL, col="lightsalmon", lty=1)
    # for each vector to be plotted, i = 1 to n
    for (i in 1:n) {
        # this doesn't work so well because the horizontal jitter distance increases as more vectors are added from right to left across the plot 
        # if (points) points(jitter(rep(centers[i], length(all.x[[i]])), amount = .25), all.x[[i]], cex=point.size, col=point.color, pch=point.pch)
        if (plot.it == T & length(unlist(all.x[[i]])) > 1) {
        	plot.values <- bpx(all.x[[i]], centers[i], boxwex)
	        if (points) points(x = runif(length(all.x[[i]]), plot.values$q10.x[1], plot.values$q10.x[2]), y = all.x[[i]], cex = point.size, col = point.color, pch = point.pch)
        	lines(plot.values$x1, plot.values$y1, lwd=2)
        	lines(plot.values$x2, plot.values$y2, lwd=2)
        	lines(plot.values$q10.x, plot.values$q10.y, col="blue", lwd=tercile.width)
        	lines(plot.values$q90.x, plot.values$q90.y, col="blue", lwd=tercile.width)
        	lines(plot.values$q1.x, plot.values$q1.y, col="green", lwd=quartile.width)
        	lines(plot.values$q3.x, plot.values$q3.y, col="green", lwd=quartile.width)
        	lines(plot.values$med.x, plot.values$med.y, col="red", lwd=median.width)
        }
    }
    if (is.logical(name)) {
        if (name) 
            mgp.axis(1, centers, sapply(substitute(list(...)), deparse)[2:(n + 1)], cex=cex.lab, srt = srtx, adj = 
            	if (srtx == 0) 0.5 else 1, axistitle = xlab)
    }
    else mgp.axis(1, centers, name, cex.axis=cex.lab, srt = srtx, adj =if (srtx == 0) 0.5  else 1, axistitle = xlab)
    invisible(centers)
}
