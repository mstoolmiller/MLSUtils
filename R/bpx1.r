bpx <- function (y, offset, boxwex) 
{
    y <- y[!is.na(y)]
    n <- length(y)
    delta <- 1/(n + 1)
    prob <- seq(delta, 1 - delta, delta)
    quan <- sort(y)
    med <- median(y)
    q1 <- quantile(y, prob = 0.25)
    q3 <- quantile(y, prob = 0.75)
    q10 <- quantile(y, prob = 0.1)
    q90 <- quantile(y, prob = 0.9)
    first.half.p <- prob[quan <= med]
    second.half.p <- 1 - prob[quan > med]
    plotx <- c(first.half.p, second.half.p)
    qx <- approx(quan, plotx, xout = q1)$y
    q1.x <- c(-qx, qx) * boxwex + offset
    qx <- approx(quan, plotx, xout = q3)$y
    q3.x <- c(-qx, qx) * boxwex + offset
    qx <- approx(quan, plotx, xout = q10)$y
    q10.x <- c(-qx, qx) * boxwex + offset
    qx <- approx(quan, plotx, xout = q90)$y
    q90.x <- c(-qx, qx) * boxwex + offset
    q1.y <- c(q1, q1)
    q3.y <- c(q3, q3)
    q10.y <- c(q10, q10)
    q90.y <- c(q90, q90)
    med.x <- c(-max(first.half.p), max(first.half.p)) * boxwex + 
        offset
    med.y <- c(med, med)
    return(list(x1 = (-plotx) * boxwex + offset, y1 = quan, x2 = plotx * 
        boxwex + offset, y2 = quan, q1.y = q1.y, q1.x = q1.x, 
        q3.y = q3.y, q3.x = q3.x, med.y = med.y, med.x = med.x, 
        q10.y = q10.y, q10.x = q10.x, q90.y = q90.y, q90.x = q90.x))
}

