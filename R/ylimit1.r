ylimit <- function(est, lo=0.95, hi=0.95, type="optimal", null=0) {
  #ylimit <- function(est=est, lo=lo95, hi=hi95, type="optimal", null=0) {

# For plots that include parameter estimates and confidence intervals:
# possible ylimit options include: 1) scale plots optimally for parameters, CIs and null value, 2) scale plots to include null value in all plots,
# 3) scale plots to include full CIs in all plots, or 4) scale plots to only include parameter estimates.
	data <- na.omit(data.frame(est, lo, hi))
	with(data, {
	if (type == "optimal") return(if (all(est >= null)) c(max(null, min(lo)), max(est)) else if (all(est < null)) c(min(est), min(null, max(hi))) else range(est))
	if (type == "null") return(if (all(est >= null)) c(null, max(est)) else if (all(est < null)) c(min(est), null) else range(est))
	if (type == "CI") return(range(c(lo, hi)))
	if (type == "estimates") return(range(est))
	})
}
