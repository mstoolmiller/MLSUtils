runit <- function(file="Rout.r", append = FALSE, ...) {
    sink(file = file, append = append, ...)
    source("clipboard", echo = TRUE, max.deparse.length = 100000)
    on.exit(sink())
}
