nestcheck <- function(data, idvars, check_missing = TRUE) {
# idvars are the character strings for names of the numeric id variables. They will be sorted according to the number of unique values to check nesting status.
# Variables with larger numbers of unique levels will be checked to see if they are nested under variables with smaller numbers of unique levels.
# This should work well with datasets that supposed to be strictly nested and actually are mostly nested.
# A list with one named vector for each ID that has observations that violate nesting will be returned. Each element of the vector
# is a count of the number of violations and each element is named with the ID number.
	if (!is.data.frame(data)) {data <- as.data.frame(data); warning("Coercing data to a data.frame")}
  data_name <- deparse(substitute(data))
	if (!grepl("[[", data_name, fixed=T)) cat(paste0("\n", data_name, "\n"))

	# count the number of non-missing, unique values of each ID variable in data and the total observations in data.
	# Put counts in a table in increasing order
	tmp1 <- data.frame(idvar = idvars, count = as.numeric(NA))
	for (i in tmp1$idvar) tmp1[tmp1$idvar == i, "count"] <- length(na.omit(unique(data[, i])))
	tmp1 <- dplyr::arrange(tmp1, dplyr::desc(tmp1$count))
	tmp1 <- rbind(tmp1, data.frame(idvar = "observations", count = nrow(data)))
	idvars <- tmp1$idvar[-nrow(tmp1)]

	if (check_missing) {tmp0 <- misspat(data[, idvars, drop = F]);
		if (nrow(tmp0) > 1) warning("Missing ID variables. Nesting report conditional on non-missing ID variables")
	} else tmp0 <- NULL

	# loop through ID variables and check to see for each pair of id vars if id var with more observations is nested within id var with fewer observations.
	tmp1 <- cbind(tmp1, matrix(NA, nrow=nrow(tmp1), ncol = nrow(tmp1)-1, dimnames=list(NULL, paste0(idvars, "_nested_in"))))
	non_nested <- list()
	for (i in 1:length(idvars)) {
		for (j in 1:length(idvars)) {
			if (j <= i) next
			tmp.dat <- na.omit(data[, idvars[c(i, j)]])
			tmp1[j, paste0(idvars[i], "_nested_in")] <- lme4::isNested(tmp.dat[, idvars[i]], tmp.dat[, idvars[j]])
			# if not nested, identify those observations that are not nested.
			if (!tmp1[j, paste0(idvars[i], "_nested_in")]) {
				# make a big cross-table of idvar[j] by idvar[i]. Sum non-zero counts in columns (idvar[i]). Pick out those columns that have more than 1 non-zero count.
				tmp <- table(data[, idvars[j]], data[, idvars[i]]); tmp <- apply(tmp, 2, function(x) sum(x > 0)); tmp <- tmp[tmp > 1];
				attr(tmp, "length") <- length(tmp)
				non_nested[[paste0(idvars[i], "_not_nested_in_", idvars[j])]] <- tmp
			}
		}
	}
	return(list(missing_idvars = tmp0, idvars_nesting_status = tmp1, non_nested = non_nested))
}
