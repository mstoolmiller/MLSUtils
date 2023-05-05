qqnorm1 <- function(data, id = F, point.size=2.5*par("cex"), id.label = names(data), ylab = NULL, grid = FALSE, cex.axis=1.25*par("cex"), cex.lab=1.75*par("cex"), stats = T, text.size = par("cex"), ...)
{
	if(is.null(ylab)) ylab <- deparse(substitute(data))
        o <- order(data, na.last = NA)
        data <- data[o]
        statistics <- descriptives(data, format = T)
        if(id == T && is.null(id.label)) id.label <- as.character(c(1:length(data)))
        else if(id == T) id.label <- id.label[o]
        norm.qtiles <- qnorm(ppoints(1.:length(data)))
        plot(norm.qtiles, data, ylab = paste(ylab, statistics[[2]], sep = ", "), xlab = "Std. Normal Quantiles", cex.lab=cex.lab, cex.axis=cex.axis, cex = point.size, ...)
        if(grid) grid(nx=-1, ny=NULL, col="lightsalmon", lty=1)
        qqline1(data)
        equants <- quantile(data, c(0.25, 0.5, 0.75))
        for(j in 1:3) {
                if(j == 2) color <- 2
                else color <- 3
                lines(c(par("usr")[1], par("usr")[2]), rep(equants[j], 2), col = color, lty = 2)
        }
        if(id == T) identify(norm.qtiles, data, labels = id.label, cex = text.size)
        if(stats == T) mtext(statistics[[1]], side = 3, line = 0.2, cex = text.size)
        tmp.dat <- data.frame(norm.qtiles, data)
        names(tmp.dat) <- c("norm_qtiles", ylab)
        invisible(tmp.dat)
}
