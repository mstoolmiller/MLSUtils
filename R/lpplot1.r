##########                                     HELPFUL FUNCTIONS
lpplot <- function(mobj.out, pseudo=T, fitted=T, path=NULL, inset = -.05, scale = T, center = T, return_pclasses = F, reverse = c(-1, 1, 1, -1, -1, -1, -1, -1),
	relabel = c("Calm1", "QOL1", "Wages1", "ASE1", "Hope1", "Esteem1", "ISEL1", "PsySOC1")) {
## Latent profile plot. mobj.out is an output object as returned from MplusAutomation. The rest of the arguments are options for the plot.
	nclasses <- max(mobj.out$class_counts$modelEstimated$class)
	if (!is.null(path)) path <- gsub("(/|\\\\)$", "", path)

	## get model fitted latent class tajectories
	raw_means <- subset(mobj.out$parameters$unstandardized, paramHeader == "Means" & LatentClass != "Categorical.Latent.Variables", c(est, param, LatentClass))
	class.means <- reshape(raw_means, direction = "wide", idvar = "param", timevar = "LatentClass", v.names = "est", sep="")
	nindicators <- nrow(class.means)

	## get raw data used in model, class probs and pseudo-class assignment.
	mobj.out.cprob <- data.frame(lapply(read.table(paste0(path, "/", mobj.out$savedata_info$fileName)), as.numeric))
	names(mobj.out.cprob) <- tolower(mobj.out$savedata_info$fileVarNames)
	names(mobj.out.cprob)[match("c", names(mobj.out.cprob))] <- "pseudo.class"

	# pseudo-class means of latent class indicators. The 1:nindicators may not always work but OK for now.
	pclass_means <- t(apply(mobj.out.cprob[, 1:nindicators], 2, function(x) tapply(scale(x, scale = scale, center = center), mobj.out.cprob$pseudo.class, mean, na.rm=T)))

	## standardize model estimated means
	if (scale) indicator_sd <- t(apply(mobj.out.cprob[, 1:nindicators], 2, sd, na.rm=T)) else indicator_sd <- rep(1, nindicators)
	if (center) indicator_mean <- t(apply(mobj.out.cprob[, 1:nindicators], 2, mean, na.rm=T)) else indicator_mean <- rep(0, nindicators)
	class.means[, -1] <- (class.means[, -1] - indicator_mean)/indicator_sd

	## reverse the direction of the negative variables
	if (!is.null(reverse)) {
		reverse <- matrix(reverse, ncol = nclasses, nrow=nindicators)
		pclass_means <- pclass_means*reverse
		class.means[, -1] <- class.means[, -1]*reverse
	}
	pclass_means <- data.frame(param = dimnames(pclass_means)[[1]], pclass_means, row.names = NULL)

	## new labels for variables.
	if (!is.null(relabel)) indicator_labels <- relabel

	# plot trajectories
	if (pseudo & fitted) matplot(1:nrow(class.means), cbind(class.means[, -1], pclass_means[, -1]), type= "l", lty = rep(c(1, 2), each = nclasses),
		col = rep(1:nclasses, 2), lwd = 5, xaxt = "n", ylab = "Class Means of Standardized Indicators", xlab="")
	else if (pseudo & !fitted) matplot(1:nrow(class.means), pclass_means[, -1], type= "l", lty=1, lwd = 5, xaxt = "n",
		ylab = "Class Means of Standardized Indicators", xlab="")
	else if (!pseudo & fitted) matplot(1:nrow(class.means), class.means[, -1], type= "l", lty=1, lwd = 5, xaxt = "n",
		ylab = "Class Means of Standardized Indicators", xlab="")
	axis(side = 1, labels = indicator_labels, at = 1:nrow(class.means), las = 2, tck = .01, mgp=c(1.25, .25, 0))
	grid(ny = NA, col = "lightsalmon")
	legend.text <- apply(cbind(x=paste0("c", 1:nclasses), round(mobj.out$class_counts$modelEstimated$proportion, 2)), 1, paste0, collapse=" = ")
	legend("top", legend.text, inset=inset, xpd=NA, ncol=nclasses, bty="n", cex=1, lwd=3, col=1:nclasses, lty=1)
	if (return_pclasses) mobj.out.cprob else NULL
}
