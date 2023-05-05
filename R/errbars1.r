errbars <- function(x, lo.lim, hi.lim, line.weight=2, bar.col="lightgray", cap.width.multiplier = .01, cap.width = NULL, orientation = "vertical") {
# make error bars for confidence intervals. Vertical orientation puts error bars through estimates vertically. Horizontal puts error bars through estimates horizontally.
# cap width is a proportion of the corresponding plot area unless non-null.
	if (orientation == "vertical") {
		if (is.null(cap.width)) cap.width <- diff(par("usr")[1:2])* cap.width.multiplier
		segments(x, hi.lim, x, lo.lim, col=bar.col, lwd=line.weight)
		segments(x-cap.width, hi.lim, x+cap.width, hi.lim, col=bar.col, lwd=line.weight)
		segments(x-cap.width, lo.lim, x+cap.width, lo.lim, col=bar.col, lwd=line.weight)
	}
	else {
		if (is.null(cap.width)) cap.width <- diff(par("usr")[3:4])* cap.width.multiplier
		segments(hi.lim, x, lo.lim, x, col=bar.col, lwd=line.weight)
		segments(hi.lim, x+cap.width, hi.lim, x-cap.width, col=bar.col, lwd=line.weight)
		segments(lo.lim, x+cap.width, lo.lim, x-cap.width, col=bar.col, lwd=line.weight)
	}
}
