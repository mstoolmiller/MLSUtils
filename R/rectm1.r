rectm <- function (x, lower = 1-upper, upper = 1-lower, Trim = T, Recode=F)
{
  x.quant <- quantile(x, prob = c(lower, upper), na.rm = T)
  if (Recode) {
    x <- (x - x.quant[1])/(x.quant[2] - x.quant[1])
  }
  if (Trim) {
    x <- ifelse(x <= x.quant[2] & x >= x.quant[1], x, ifelse(x >
      x.quant[2], x.quant[2], x.quant[1]))
  }
  return(x)
}
