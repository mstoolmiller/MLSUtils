setdiff1 <- function (x, y)
{
    tmp <- vector("list", 2)
    names(tmp) <- c(paste("not in", deparse(substitute(y))), paste("not in", deparse(substitute(x))))
    tmp[[1]] <- setdiff(x, y)
    attr(tmp[[1]], "length") <- length(tmp[[1]])
    tmp[[2]] <- setdiff(y, x)
    attr(tmp[[2]], "length") <- length(tmp[[2]])
    return(tmp)
}
