qqplot1 <- function (x, y, id = FALSE, point.size = 2.5*par("cex"),
    id.text = 0.5, plot.it = TRUE, xlab = NULL, ylab = NULL,
    digits = 2, quantiles = T, t.stats = T, paired.t = F, text.size = 1.25*par("cex"),
	 cex.axis=1.5*par("cex"), cex.lab=2*par("cex"),
    square = F, common.scale = NULL, ...)
{
    if (is.null(xlab))
        xlab <- deparse(substitute(x))
    if (is.null(ylab))
        ylab <- deparse(substitute(y))
    if (t.stats) {
        tstats <- t.test(x, y, paired = paired.t)[c(1, 2, 3)]
        tstatsvalue <- paste(c("t=", "df=", "p="), round(c(tstats$statistic,
            tstats$parameter, tstats$p.value), digits), sep = "",
            collapse = ", ")
    }
    if (quantiles)
        quants <- matrix(c(quantile(x, c(0.25, 0.5, 0.75), na.rm = T),
            quantile(y, c(0.25, 0.5, 0.75), na.rm = T)), ncol = 2)
    if (!is.null(dimnames(x)))
        xnames <- dimnames(x)[[1]]
    else xnames <- as.character(1:length(x))
    if (!is.null(dimnames(y)))
        ynames <- dimnames(y)[[1]]
    else ynames <- as.character(1:length(y))
    if (is.data.frame(x))
        x <- x[, 1]
    if (is.data.frame(y))
        y <- y[, 1]
    o.x <- order(x, na.last = NA)
    o.y <- order(y, na.last = NA)
    x <- x[o.x]
    xnames <- xnames[o.x]
    y <- y[o.y]
    ynames <- ynames[o.y]
    nx <- length(x)
    ny <- length(y)
    if (nx > ny) {
        x <- approx(1:nx, x, n = ny)$y
        xnames <- xnames[ceiling(nx/ny * ((1:ny) - 1/2) + 1/2)]
    }
    if (ny > nx) {
        y <- approx(1:ny, y, n = nx)$y
        ynames <- ynames[ceiling(ny/nx * ((1:nx) - 1/2) + 1/2)]
    }
    if (plot.it) {
        if (square) {
            ylimits <- c(min(c(x, y)), max(c(x, y)))
            xlimits <- ylimits
        }
        if (!is.null(common.scale)) {
            ylimits <- common.scale
            xlimits <- common.scale
        }
        else {
            ylimits <- c(min(y), max(y))
            xlimits <- c(min(x), max(x))
        }
        plot(x, y, type = "n", xlab = xlab, ylab = ylab, ylim = ylimits,
            xlim = xlimits, cex.axis=cex.axis, cex.lab=cex.lab, ...)
        points(x, y, cex = point.size)
        abline(c(0, 1))
        if (quantiles) {
            for (k in 1:3) {
                if (k == 2)
                  color <- 2
                else color <- 3
                segments(x0 = quants[k, 1], y0 = par("usr")[3],
                  x1 = quants[k, 1], y1 = quants[k, 2], col = color)
                segments(x0 = quants[k, 1], y0 = quants[k, 2],
                  x1 = par("usr")[1], y1 = quants[k, 2], col = color)
            }
        }
        if (t.stats)
            mtext(tstatsvalue, side = 3, line = 0.1, cex = text.size)
    }
    if (id)
        identify(x, y, labels = paste(xnames, ynames), cex = id.text)
    invisible(list(x = x, y = y))
}
