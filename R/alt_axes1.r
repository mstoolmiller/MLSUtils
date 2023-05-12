#' Alternates ticks so they do not crowd each other
#'
#' These lines provide a verbose description.
#' This could be multiple lines
#'
#' @param i An integer value. a subplot counter, from 1 to the total number of subplots = prod(par("mfrow"))
#'
#' @examples
#' # Some code
#'
alt_axes <- function(i) {
  # i is a subplot counter, from 1 to the total number of subplots = prod(par("mfrow"))
	# for horizontal axis
	# if subplot number is odd and row is last row, put tck labels on bottom axis
	if (i %in% seq(prod(par("mfrow"))- par("mfrow")[2] + 1, prod(par("mfrow")), 2)) axis(side=1)
	# if subplot number is even and row is first row, put tck labels on top axis
	if (i %in% seq(2, par("mfrow")[2], 2)) axis(side=2)
	# for vertical axis
	# if subplot number is odd and column is first column, put tck labels on left axis
	if (i %in% seq(1, prod(par("mfrow")), 2*par("mfrow")[2])) axis(side=2)
	# if suplot number is even and column is last column, put tck labels on right axis
	if (i %in% seq(2*par("mfrow")[2], prod(par("mfrow")), 2*par("mfrow")[2])) axis(side=4)
}
