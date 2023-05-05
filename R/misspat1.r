misspat <- function (x, sort = "r") 
{
    if (!is.data.frame(x)) {
        x <- data.frame(x)
        if (ncol(x) == 1 && names(x) == "x" && is.name(substitute(x))) names(x) <- substitute(x)
    }
    n <- nrow(x)
    p <- ncol(x)
    ina <- is.na(x)
    var.mis <- colSums(ina)
    sort.r <- sort.c <- sort
    if (is.character(sort)) {
        sort.r <- length(grep("r", sort))
        if (length(grep("R", sort))) sort.r <- 2
        sort.c <- length(grep("c", sort))
        sort <- T
    }
    if (sort.c) {
        maxBits <- trunc(-logb(.Machine$double.eps, 2))
        oC <- do.call("order", c(list(var.mis), lapply(seq(length = ceiling(n/maxBits)), 
           function(j, M, v, k) colSums(M * (0.5)^pmax(0, v - (j - 1) * k)), M = ina, v = rank(rowSums(ina)), k = maxBits)))
        ina <- ina[, oC, drop = F]
        var.mis <- var.mis[oC]
    }
    else oC <- 1:p
    dimnames(ina) <- list(NULL, dimnames(ina)[[2]])
    key <- do.call("paste", c(split(ina, col(ina)), list(sep = "")))
    w <- !duplicated(key)
    keyw <- key[w]
    pattern <- ina[w, , drop = F]
    if (sort.r) {
        key.order <- if (sort.r == 2) order(keyw)
        else order(rowSums(pattern), keyw)
        pattern <- pattern[key.order, , drop = F]
        okey <- c(ordered(key, levels = keyw[key.order]))
    }
    else okey <- c(ordered(key, levels = keyw))
    rep.pattern <- tabulate(okey)
    oR <- order(okey)
    result <- c(list(n = n, var.mis = var.mis, pattern = pattern, rep.pattern = rep.pattern), row.order = list(oR), if (sort.c) list(column.order = oC))
    oldClass(result) <- "miss"
    if (result$n > 0) data.frame(Nobs = result$rep.pattern, Nmiss = apply(result$pattern, 1, sum), 
	 	matrix(as.integer(result$pattern), ncol = ncol(result$pattern), dimnames = list(NULL, dimnames(result$pattern)[[2]])))
	 else print("No data selected")
}

