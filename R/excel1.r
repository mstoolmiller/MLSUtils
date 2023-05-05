excel <- function (data, file = paste0(deparse(substitute(data)), ".csv"), path = NULL, na = " ", row.names = FALSE, ...)
{
    if (is.null(path))
        path <- paste0(getwd(), "/")
    write.csv(data, file = paste0(path, file), na = na, row.names=row.names, ...)
    print(paste0("Data exported to: ", paste0(path, file), " as Excel CSV"))
}
