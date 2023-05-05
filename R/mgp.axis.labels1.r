mgp.axis.labels <- function (value, type = c("xy", "x", "y", "x and y")) 
{
    type <- match.arg(type)
    if (missing(value)) {
        value <- .Options$mgp.axis.labels
        pr <- par(c("mgp", "las"))
        mgp <- pr$mgp
        if (!length(value)) 
            value <- c(0.7, 0.7)
        return(switch(type, xy = value, x = c(mgp[1], value[1], 
            mgp[3]), y = c(mgp[1], value[2], mgp[3]), `x and y` = list(x = c(mgp[1], 
            value[1], mgp[3]), y = c(mgp[1], value[2], mgp[3]))))
    }
    if (value[1] == "default") 
        value <- c(0.7, 0.7)
    options(mgp.axis.labels = value, TEMPORARY = FALSE)
    invisible()
}
