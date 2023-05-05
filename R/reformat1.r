reformat <- function (d3array, digits=4)
{
    nivars <- dim(d3array)[1]
    nstats <- dim(d3array)[2]
    ndvars <- dim(d3array)[3]
    ivarlab <- dimnames(d3array)[[1]]
    statlab <- dimnames(d3array)[[2]]
    dvarlab <- dimnames(d3array)[[3]]
    data.frame(stat = rep(statlab, rep(nivars, nstats)), variable = rep(ivarlab,
                                                                        nstats), round(matrix(d3array, ncol = ndvars, nrow = nivars *
                                                                                                  nstats, dimnames = list(NULL, dvarlab)), digits=digits))
}
