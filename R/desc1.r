desc <- function (data, groupvar = NULL, maxcat = 30, print = TRUE, digits = 3)
{
    if (is.null(dim(data))) data <- as.data.frame(matrix(data, ncol = 1, dimnames = list(NULL, deparse(substitute(data)))))
    non.numerics <- sapply(data, function(x) !is.numeric(x))
    if (any(non.numerics)) cat(paste0("Removing Non-numeric variables: ", dimnames(data)[[2]][non.numerics], "\n"))
    stat.names <- c("Mean", "Sd", "Skew", "Kurt", "Min", "Q25", "Q50", "Q75", "Max", "Min%", "Max%", "Uvals", "N")
    variable.names <- dimnames(data[, sapply(data, is.numeric), drop = F])[[2]]
    vn <- sum(sapply(data, is.numeric))
    if (is.null(groupvar)) {
        stats <- t(apply(data[, sapply(data, is.numeric), drop = F], 2, descriptives))
        uvals <- apply(data[, sapply(data, is.numeric), drop = F], 2, table, exclude = NULL)
        continuous.vars <- (1:length(uvals))[sapply(uvals, length) > maxcat]
        tables <- matrix(NA, ncol = maxcat, nrow = vn)
        for (i in (1:vn)[-continuous.vars]) {
            nvals <- length(uvals[[i]])
            if (is.na(names(uvals[[i]])[nvals])) {
                if (nvals == 1) tables[i, ] <- c(rep(NA, maxcat - nvals), uvals[[i]][nvals])
                else tables[i, ] <- c(uvals[[i]][1:(nvals - 1)], rep(NA, maxcat - nvals), uvals[[i]][nvals])
            }
            else tables[i, ] <- c(uvals[[i]], rep(NA, maxcat - nvals))
        }
        dimnames(tables) <- list(variable.names, c(paste0(1:(maxcat - 1)), "NA"))
        dimnames(stats) <- list(dimnames(stats)[[1]], stat.names)
        if (print) print(stats, digits=digits)
        invisible(list(stats = stats, tables = tables))
    }
    else {
        if (!is.factor(groupvar)) groupvar <- factor(groupvar)
        group.names <- levels(groupvar)
        gn <- length(group.names)
        v.by.g.names <- paste(rep(variable.names, rep(gn, vn)), rep(group.names, vn))
        tmp <- apply(data[, sapply(data, is.numeric), drop = F], 2, function(x, groupvar) tapply(x, groupvar, descriptives), groupvar = groupvar)
        tmp <- matrix(unlist(tmp), ncol = 13, byrow = T, dimnames = list(v.by.g.names, stat.names))
        if (print) print(tmp, digits=digits)
        invisible(tmp)
    }
}
