weighted_var <- function(m, v, w, labels = NULL, var_correct = TRUE) {
# Weighted variance from subgroup N's (w), means (m), and variances (v). Labels for subgroups can be included.
# var_correct argument is for un-doing the bias correction usually applied to the input variances (divide by N-1 rather than N).
# The output pooled variance is bias corrected if the input variances are bias corrected.
	wp <- sum(w)
	sdev <- sqrt(v)
	if (var_correct) v <- v*(w-1)/w
	mp <- weighted.mean(m, w);
	ms <- m^2+v;
	msp = weighted.mean(ms, w);
	vp <- if (var_correct) (msp - mp^2)* (wp/(wp-1)) else msp - mp^2
	sdevp <- sqrt(vp)
	pmat <- matrix(c(w, m, sdev, v, ms), ncol = 5, dimnames = list(labels, c("N", "Mean", "Sd", "Var", "MnSq")))
	pmat <- rbind(pmat, c(wp, mp, sdevp, vp, msp))
	dimnames(pmat)[[1]][nrow(pmat)] <- "Pooled"
	pmat
}
