#' Capitalize the first letter of a word/sentence.
#' 
#' This implementation is inspired from the help of the \code{toupper} function.
#' @param x Character vector to capitalize
#' @param onlyFirst Logical, if TRUE (by default)
#' capitalize the first letter of the first word only.
#' Otherwise, capitalize the first letters of all words of the sentence.
#' See also \code{link[tools]{toTitleCase}} for a more syntax-friendly implementation.
#' @param rev Logical, if TRUE (FALSE by default), set first letter to lower case (otherwise upper case)
#' @return Character vector with first letter capitalized
#' @author author of the 'toupper' function?
#' @seealso \code{link[tools]{toTitleCase}}
#' @examples 
#' # capitalize only the first word of the sentence
#' simpleCap(x = "this is the caption of my figure.")
#' # capitalize all words
#' simpleCap(x = "this is the caption of my figure.", onlyFirst = FALSE)
#' # opposite: set the first letter of the first word to lower case
#' simpleCap(x = "This is the caption of my figure.", rev = TRUE)
#' @export
simpleCap <- function(x, onlyFirst = TRUE, rev = FALSE) {
	paste0c <- function(...) paste(..., sep = "", collapse = " ")
	fctToUse <- get(ifelse(rev, "tolower", "toupper"))
	simpleCap1 <- function(s) paste0c(fctToUse(substring(s, 1, 1)), substring(s, 2))
	res <- sapply(x, function(x){	
		if(!is.na(x) && x != ""){
			s <- strsplit(x, " ")[[1]]
			if(onlyFirst)	paste0c(c(simpleCap1(s[1]), s[-1]))	else	simpleCap1(s)
		}else x
	})
	return(res)
}

#' Function for reordering columns
#' 
#' @param data a data.frame
#' @param vars named vector indicating the position in the data frame of the specified variable
#' @export 
#' @examples
#' someData <- data.frame(
#'		"Col1" = c(1, 2),
#'		"Col2" = c(2, 3),
#'		"Col3" = c(3, 4)
#'		)
#' reorderColumns(
#'		data = someData,
#'		vars = c("Col3" = 1)
#'		)
reorderColumns <- function(data, vars){
	
	# Sort out inputs
	dataNames <- names(data)
	numberVars <- length(dataNames)
	varNames <- names(vars)
	if(any(! varNames %in% dataNames)) stop("One or more vars is not available in the data")
	varPosition <- vars
	# Prepare output
	outPosition <- character(numberVars)
	outPosition[varPosition]  <- varNames
	outPosition[-varPosition] <- dataNames[ !(dataNames %in% varNames) ]
	# Re-arrange vars by position
	data <- data[ , outPosition]
	return(data)
}
