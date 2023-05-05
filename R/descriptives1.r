descriptives <- function(data, plevel = 0.05, digits = 2, format = F)
{
unique.vals <- length(unique(data[!is.na(data)]))
if (unique.vals == 0) return(c(rep(NA, 11), 0, 0))
data.min <- min(data, na.rm=T)
data.max <- max(data, na.rm=T)
N <- sum(!is.na(data))
stats <- c(mean(data, na.rm=T), sd(data, na.rm=T), skew(data), kurt(data), data.min, quantile(data, prob=c(.25, .50, .75), na.rm=T), data.max,
	(100 * sum(!is.na(data) & data == data.min))/N, (100 * sum(!is.na(data) & data == data.max))/N, unique.vals, N)

if(format == T) {
	stats <- format(round(stats[1:4], digits))
	z.skew <- as.numeric(stats[3]) * sqrt(((N + 1) * (N + 3))/6/(N - 2))
	z.kurt <- (as.numeric(stats[4]) + 6/(N + 1)) * sqrt(((N + 1)^2 * (N + 3) * (N + 5))/(24 * N * (N - 2) * (N - 3)))
	prob.skew <- pnorm(z.skew)
	prob.kurt <- pnorm(z.kurt)
	if(!is.na(prob.skew) & (prob.skew > 1 - plevel/2 | prob.skew < plevel/2)) stats[3] <- paste(stats[3], "*", sep = "")
	if(!is.na(prob.kurt) & (prob.kurt > 1 - plevel/2 | prob.kurt < plevel/2)) stats[4] <- paste(stats[4], "*", sep = "")
	return(list(paste(c("M=", "Sd=", "Sk=", "K="), stats, sep = "", collapse = ","), paste("N=", N, sep = "")))
	}
else {
	names(stats) <- c("Mean", "Sd", "Skew", "Kurt", "Min", "Q25", "Q50", "Q75", "Max", "Min%", "Max%", "Uvals", "N")
	return(stats)
	}
}
