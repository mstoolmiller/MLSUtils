waldtest <- function (fit, cmat, var = vcov(fit), multi = F) 
{
    if (is.vector(fit)) beta <- fit
    else beta <- coef(fit)
    nvar <- length(beta)
    missing <- is.na(beta)
    if (ncol(var) != nrow(var) || ncol(var) != nvar) stop("Mismatched coefficient and variance matrices")
    if (any(missing)) {
        beta <- beta[!missing]
        var <- var[!missing, !missing]
    }
    if (!is.matrix(cmat)) {
        if (length(cmat) == nvar) cmat <- as.matrix(cmat)
        else {
            whichvar <- floor(cmat)
            if (any(whichvar < 1 | whichvar > nvar)) stop("Coefficient number out of range")
            cmat <- 0 * diag(nvar)
            cmat[cbind(whichvar, whichvar)] <- 1
        }
    }
    if (nrow(cmat) != nvar) stop("Wrong number of rows in cmat")
    if (any(missing)) cmat <- cmat[!missing, ]
    est <- t(cmat) %*% beta
	 R <- t(cmat) %*% var %*% cmat
    se.est <- sqrt(diag(R))
    z <- est/se.est
    z[se.est <= 0] <- as.numeric(NA)
    p <- 2 * (1 - pnorm(abs(z)))
    p[is.na(z)] <- NA
    if (multi) {
    	test <- as.vector(t(est) %*% solve(R) %*% est) 
		df <- nrow(est)
	   pval <- 1 - pchisq(test, df = df)
	 }
    else df <- pval <- test <- NA
    list(individual.estimates = data.frame(est = est, se.est = se.est, z = z, p = p, row.names = dimnames(t(cmat))[[1]]), 
    	chisq = test, df = df, p = signif(pval, 4))
}
