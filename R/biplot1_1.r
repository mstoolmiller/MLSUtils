biplot1 <- function(xdata, ydata = NULL, ..., points = T, point.lwd = 1, point.size = 2.5 * par("cex"), point.color = 1, pch = 1, jit = F, id = F, id.label = row.names(xdata), id.cex = par("cex"),
	xvar.labels = NULL, yvar.labels = NULL, text.size=par("cex"), grid = "T", grid.nx = NULL, grid.ny = grid.nx, grid.col = "lightsalmon", grid.lwd = 1, grid.lty = 1, stats=T, digits = 2, stat.text.size = par("cex"),
	smooth = T, smooth.line = 1, smooth.line.color = 3, smooth.line.lwd = 3, straight = T, straight.line = 1, straight.line.color = 2, straight.line.lwd = 2, qplot = T, qscale = F,
	mfrow = c(ncol(xdata), ncol(xdata)), mar = c(3, 3, 2, 1), mgp = c(1.75, 0.4, 0), tck = 0.005, oma = c(0, 0, 3, 1))
{
if(!is.data.frame(xdata)) xdata <- data.frame(xdata)
rows <- ncol(xdata)
if(is.null(xvar.labels)) xvar.labels <- names(xdata)
if(!is.null(mfrow)) {
#	par.old <- par()
#	invisible(on.exit(par(par.old)))
	invisible(par(mfrow = mfrow))
}
par(oma = oma, mar = mar, mgp = mgp, tck = tck)
if(is.null(ydata)) {
	if(qscale == T)
	qscale.val <- c(min(xdata, na.rm = T), max(xdata, na.rm = T))
	tmp <- array(dim = c(5, rows, rows), dimnames = list(c("r", "b", "t", "p", "N"), xvar.labels, xvar.labels))
	for(indvar in 1:rows) {
		for(depvar in 1:rows) {
			if(depvar == indvar) {
				if(qplot == F)	next
				else {
					if(qscale == F) qscale.val <- c(min(xdata[, indvar], na.rm = T), max(xdata[, indvar], na.rm = T))
					qqnorm1(xdata[, indvar], ylab = xvar.labels[indvar], ylim = qscale.val, text.size = text.size, id = id,
						id.label = id.label, point.size=point.size, pch = 1, stats=stats, ...)
				}
			}
			else {
				tmp[1:5, depvar, indvar] <- plot1(xdata[, indvar], xdata[, depvar],
					xlab = xvar.labels[indvar], ylab=xvar.labels[depvar],	id = id, id.label = id.label, id.cex=id.cex, jit = jit,
					points=points, point.size=point.size, point.lwd=point.lwd, point.color=point.color,
					stats=stats, stat.text.size = stat.text.size,
					smooth = smooth, smooth.line = smooth.line, smooth.line.color = smooth.line.color,
					smooth.line.lwd = smooth.line.lwd,
					straight = straight, straight.line = straight.line,
					straight.line.color = straight.line.color, straight.line.lwd = straight.line.lwd, ...)[1, c("corr", "slope", "slope_se", "slope_p", "N")]
				tmp[3, depvar, indvar] <- tmp[2, depvar, indvar]/tmp[3, depvar, indvar]

			}
		}
	}
}
else {
	if(!is.data.frame(ydata)) ydata <- data.frame(ydata)
	if(is.null(yvar.labels)) yvar.labels <- names(ydata)
	cols <- ncol(ydata)
	tmp <- array(dim = c(5, cols, rows), dimnames = list(c("r", "b", "t", "p", "N"), yvar.labels, xvar.labels))
	for(indvar in 1:rows) {
		for(depvar in 1:cols) {
			tmp[1:5, depvar, indvar] <- plot1(xdata[, indvar], ydata[, depvar],
				xlab = xvar.labels[indvar], ylab=yvar.labels[depvar], id = id, id.label = id.label, id.cex=id.cex, jit = jit,
				points=points, point.size=point.size, point.lwd=point.lwd, point.color=point.color,
				stats=stats, stat.text.size = stat.text.size,
				smooth = smooth, smooth.line = smooth.line,
				smooth.line.color = smooth.line.color, smooth.line.lwd = smooth.line.lwd,
				straight = straight, straight.line = straight.line, straight.line.color = straight.line.color,
				straight.line.lwd = straight.line.lwd, ...)[1, c("corr", "slope", "slope_se", "slope_p", "N")]
			tmp[3, depvar, indvar] <- tmp[2, depvar, indvar]/tmp[3, depvar, indvar]
		}
	}
}
invisible(return(aperm(tmp, c(3, 1, 2))))
}
