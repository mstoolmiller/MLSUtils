qqline1 <- function (x)
{
    data.quartiles <- quantile(x, c(0.25, 0.75), na.rm = T)
    norm.quartiles <- qnorm(c(0.25, 0.75))
    b <- (data.quartiles[2] - data.quartiles[1])/(norm.quartiles[2] - norm.quartiles[1])
    a <- data.quartiles[1] - norm.quartiles[1] * b
    abline(a, b)
}
