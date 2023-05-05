kurt <- function (x, na.rm = T, method = "fisher") 
{
    method <- char.expand(method, c("fisher", "moment"), stop("argument 'method' must match either \"fisher\" or \"moment\""))
    if (na.rm) {
        wnas <- seq(along = x)[is.na(x)]
        if (length(wnas)) x <- x[-wnas]
    }
    else if (length(seq(along = x)[is.na(x)])) return(NA)
    n <- as.double(length(x))
    if (method == "fisher" && n < 4) return(NA)
    x <- x - mean(x)
    if (method == "moment") (sum(x^4)/n)/(sum(x^2)/n)^2 - 3
    else ((n + 1) * (n - 1) * ((sum(x^4)/n)/(sum(x^2)/n)^2 - (3 * (n - 1))/(n + 1)))/((n - 2) * (n - 3))
}
