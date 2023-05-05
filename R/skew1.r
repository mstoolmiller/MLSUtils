skew <- function (x, na.rm = T, method = "fisher") 
{
    method <- char.expand(method, c("fisher", "moment"), stop("argument 'method' must match either \"fisher\" or \"moment\""))
    if (na.rm) {
        wnas <- seq(along = x)[is.na(x)]
        if (length(wnas)) x <- x[-wnas]
    }
    else if (length(seq(along = x)[is.na(x)])) return(NA)
    n <- as.double(length(x))
    if (method == "fisher" && n < 3) return(NA)
    x <- x - mean(x)
    if (method == "moment") (sum(x^3)/n)/(sum(x^2)/n)^1.5
    else ((sqrt(n * (n - 1))/(n - 2)) * (sum(x^3)/n))/((sum(x^2)/n)^1.5)
}
