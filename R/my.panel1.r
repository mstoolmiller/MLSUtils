my.panel <- function(x, y, straight=T, smooth=T) {panel.grid(h = -1, v = -1, col = "lightsalmon"); panel.xyplot(x, y, cex = 1.25)
	if (straight & length(x) == 2) lpoints(x, y, type = "l", col = 2, lwd = 3)
	if (straight & length(x) >= 3) panel.loess(x, y, col = 2, lwd = 3, span = 5, family = "gaussian")
	if (smooth & length(x) >= 5) panel.loess(x, y, col = 3, lwd = 3, family = "gaussian")
}
