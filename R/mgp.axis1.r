mgp.axis <- function (side, at = NULL, ..., tck=par("tck"), mgp = mgp.axis.labels(type = if (side == 1 | side == 3) "x" else "y"), axistitle = NULL) 
{
    mfrow <- par("mfrow")
    nr <- mfrow[1]
    nc <- mfrow[2]
    w <- list(side = side)
    w <- c(w, list(...))
    if (length(at)) 
        w$at <- at
    if (side == 1 || side == 3) {
        w$mgp <- mgp/nr
        # if (.R.) w$tcl <- -0.4/nr
        w$tcl <- -0.4/nr
		  w$tck <- tck
        if (side == 1 && length(axistitle)) 
            title(xlab = axistitle, mgp = mgp/min(2.25, nr))
    }
    else {
        w$mgp <- mgp/nc
        # if (.R.) w$tcl <- -0.4/nc
        w$tcl <- -0.4/nc
        las <- par("las")
        w$srt <- 90 * (las == 0)
        w$adj <- if (las == 0) 0.5
        else 1
        if (side == 2 && length(axistitle)) 
            title(ylab = axistitle, mgp = mgp/min(2.25, nc))
    }
    do.call("axis", w)
    invisible()
}

