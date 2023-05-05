plot1 <- function (x, y, ..., points = T, point.lwd = 1, point.size = 2 * par("cex"), point.color = 1, pch = 1, jit = F, id = F, id.label = NULL, id.cex = par("cex"),
	plot.label = NULL, xlab = NULL, ylab = NULL, cex.axis=1.5*par("cex"), cex.lab=2*par("cex"), grid = "B", grid.nx = NULL, grid.ny = grid.nx, grid.col = "lightsalmon",
	grid.lwd = 1, grid.lty = 1, smooth = T, smooth.iter = 3, smooth.line = 1, smooth.line.color = 3, smooth.line.lwd = 3, straight = T, straight.line = 1,
	straight.line.color = 2, straight.line.lwd = 2, stats = T, digits = 2, stat.text.size = par("cex"), se = F, ci = 0.95, new = F)
{
    if (id == T && is.null(id.label)) {
        id.label <- as.character(1:length(x))
        tmp.data <- na.omit(data.frame(x, y, id.label))
    }
    else if (id == T) tmp.data <- na.omit(data.frame(x, y, id.label))
    else tmp.data <- na.omit(data.frame(x, y))
    if (is.null(xlab)) xlab <- deparse(substitute(x))
    if (is.null(ylab)) ylab <- deparse(substitute(y))
    xlimits <- range(tmp.data[, 1])
    ylimits <- range(tmp.data[, 2])
    if (new == F) {
        if (jit) {
            plot(jitter(tmp.data[, 1]), jitter(tmp.data[, 2]), xlab = xlab, ylab = ylab, cex.axis = cex.axis, cex.lab = cex.lab, type = "n", ...)
            if (grid == "B") grid(nx = grid.nx, ny = grid.ny, col = grid.col, lwd = grid.lwd, lty = grid.lty)
            box()
            if (points) points(jitter(tmp.data[, 1]), jitter(tmp.data[, 2]), pch = pch, cex = point.size, lwd = point.lwd, col=point.color)
        }
        else {
            plot(tmp.data[, 1], tmp.data[, 2], xlab = xlab, ylab = ylab, cex.axis = cex.axis, cex.lab = cex.lab, type = "n", ...)
            if (grid == "B") grid(nx = grid.nx, ny = grid.ny, col = grid.col, lwd = grid.lwd, lty = grid.lty)
            box()
            if (points) points(tmp.data[, 1], tmp.data[, 2], pch = pch, cex = point.size, lwd = point.lwd, col=point.color)
        }
    }
    else {
        if (jit) {
            if (grid == "B") grid(nx = grid.nx, ny = grid.ny, col = grid.col, lwd = grid.lwd, lty = grid.lty)
            box()
            if (points) points(jitter(tmp.data[, 1]), jitter(tmp.data[, 2]), cex = point.size, lwd = point.lwd, col=point.color)
        }
        else {
            if (grid == "B") grid(nx = grid.nx, ny = grid.ny, col = grid.col, lwd = grid.lwd, lty = grid.lty)
            box()
            if (points) points(tmp.data[, 1], tmp.data[, 2], cex = point.size, lwd = point.lwd, col=point.color)
        }
    }
    if (!is.null(plot.label)) title(plot.label)
    if (stats == T | straight == T) {
		treg <- lm(y ~ x, data = tmp.data)
		B <- as.vector(coef(treg))
	}
    if (stats == T) {
        stderrs <- summary(treg)$coefficients[3:4]
        tvalues <- summary(treg)$coefficients[5:6]
        ps <- summary(treg)$coefficients[7:8]
        r <- cor(tmp.data[, 1], tmp.data[, 2])
        N <- nrow(tmp.data)
        statvalue <- paste(c("r = ", "B = ", "t = ", "p = ", "N = "), c(format(round(c(r, B[2], tvalues[2], ps[2]), 2),
        digits = 3, justify = "n"), N), sep = "", collapse = ", ")
        mtext(statvalue, line = 0.25, side = 3, cex = stat.text.size)
    }
    if (grid == "T") grid(nx = grid.nx, ny = grid.ny, col = grid.col, lwd = grid.lwd, lty = grid.lty)
    box()
    if (straight == T) segments(xlimits[1], xlimits[1] * B[2] + B[1], xlimits[2], xlimits[2] * B[2] + B[1], lty = straight.line, col = straight.line.color, lwd = straight.line.lwd)
    if (smooth) lines(sort(tmp.data$x), predict(loess(y~x, data=tmp.data[order(tmp.data$x), ])), lty = smooth.line, col = smooth.line.color, lwd = smooth.line.lwd)
    if (id == T) identify(tmp.data[, 1], tmp.data[, 2], labels = tmp.data$id.label, cex = id.cex)
    if (se == T) {
        x <- seq(xlimits[1], xlimits[2], length = 100)
        fit <- predict(treg, se.fit = T, newdata = data.frame(x))
        lines(x, qt((1-ci)/2, fit$df)*fit$se.fit+fit$fit, lty=3, col=straight.line.color, lwd=straight.line.lwd)
        lines(x, qt((1+ci)/2, fit$df)*fit$se.fit+fit$fit, lty=3, col=straight.line.color, lwd=straight.line.lwd)
    }
    if (stats) return(matrix(c(corr=r, intercept=B[1], slope=B[2], int_se = stderrs[1], slope_se = stderrs[2], int_p = ps[1], slope_p = ps[2], N=N), nrow=1, byrow=T,
    	dimnames=list(paste0(ylab, " ~ ", xlab), c("corr", "intercept", "slope", "int_se", "slope_se", "int_p", "slope_p", "N"))))
    # else return(c(corr=NA, intercept=NA, slope=NA, slope.t=NA, slope.p=NA, N=NA))
}
