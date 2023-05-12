diplot <- function (data, indvars, text.size = par("cex.lab"), var.labels = names(data), pch = 1, digits = 2, stats = T, point.size = par("cex"), new = T, smooth = T,
	straight = T, smooth.line.color = 2, straight.line.color = 3, smooth.line.type = 1, straight.line.type = 1, smooth.line.weight = 3, straight.line.weight = 3, smooth.se = T,
    straight.se = T, ...)
{
	 pods <- function(x) exp(x)/(1+exp(x))
	 #attach(data)  #katia
    if (is.character(indvars)) {
        indvars <- match(indvars, names(data), nomatch = 0)
        if (any(indvars) == 0) stop("Independent variable name not found in variable names of input data")
    }
    for (j in indvars) {
        for (i in 1:ncol(data)) {
            if (any(i == indvars)) next
            o <- order(data[, j])
            pnts <- sum(!is.na(unique(data[, j])))
            if (pnts > 3 & smooth) {
                tmp.gam <- gam(data[, i] ~ s(data[, j]), na.action = na.exclude, family = binomial)
                fgam <- predict(tmp.gam, se = smooth.se)
                fgam$fit <- fgam$fit[o]
                fgam$se.fit <- fgam$se.fit[o]
                fgam.ci <- matrix(c(pods(-1.96 * fgam$se.fit + fgam$fit), pods(1.96 * fgam$se.fit + fgam$fit)), ncol = 2, dimnames = list(NULL, c("fgam.lower", "fgam.upper")))
					 fgam$fit <- pods(fgam$fit)
            }
            else fgam <- fgam.ci <- NULL
            if (straight) {
                tmp.glm <- glm(data[, i] ~ data[, j], na.action = na.exclude, family = binomial)
                fglm <- predict(tmp.glm, se = straight.se)
                fglm$fit <- fglm$fit[o]
                fglm$se.fit <- fglm$se.fit[o]
                fglm.ci <- matrix(c(pods(-1.96 * fglm$se.fit + fglm$fit), pods(1.96 * fglm$se.fit + fglm$fit)), ncol = 2, dimnames = list(NULL, c("fglm.lower", "fglm.upper")))
					 fglm$fit <- pods(fglm$fit)
            }
            else fglm <- fglm.ci <- NULL
            tmp <- data.frame(na.omit(cbind(data = data[o, j], fglm.fit = fglm$fit, fgam.fit = fgam$fit, fglm.ci, fgam.ci)))
            if (new)
                plot(data[, j], jitter(data[, i], amount = 0), pch = pch, xlab = var.labels[j], ylab = var.labels[i], cex = point.size, ...)
            if (straight) {
                points(tmp[, c("data", "fglm.fit")], type = "l", lwd = straight.line.weight, lty = straight.line.type, col = straight.line.color)
                if (straight.se)
                  matpoints(tmp[, "data"], tmp[, c("fglm.lower", "fglm.upper")], type = "l", lty = 2, col = straight.line.color)
            }
            if (pnts > 3 & smooth) {
                points(tmp[, c("data", "fgam.fit")], type = "l", lwd = smooth.line.weight, lty = smooth.line.type, col = smooth.line.color)
                if (smooth.se) matpoints(tmp[, "data"], tmp[, c("fgam.lower", "fgam.upper")], type = "l", lty = 2, col = smooth.line.color)
            }
            if (stats) {
                if (pnts > 3 & smooth) {
                  chi.table <- anova(tmp.glm, tmp.gam)
                  chi <- format(round(chi.table$Deviance[2], digits))
                  df <- format(round(chi.table$Df[2], digits))
                  p.chi <- format(round(1 - pchisq(chi.table$Deviance[2], chi.table$Df[2]), digits))
                }
                else chi <- df <- p.chi <- "NA"
                if (straight) {
                  B <- format(round(coef(tmp.glm)[2], digits))
                  tvalue <- format(round(summary(tmp.glm)$coefficients[2, 3], digits))
                  p.t <- format(round(2 - 2 * pt(abs(summary(tmp.glm)$coefficients[2, 3]), tmp.glm$df.residual), digits))
                }
                else B <- tvalue <- p.t <- NULL
                statvalue <- paste(c("B=", "t=", "p=", "Chi=", "df=", "p="), c(B, tvalue, p.t, chi, df, p.chi), sep = "", collapse = ", ")
                mtext(statvalue, line = 0.2, side = 3, cex = text.size)
            }
        }
    }
    invisible(detach(2))
}
