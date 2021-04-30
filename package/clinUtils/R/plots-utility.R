#' Format parameter variable to be displayed 
#' in the labels of a plot
#' 
#' The following workflow is used:
#' \enumerate{
#' \item{format the variable as a factor}
#' \item{wrap it across multiple lines if needed}
#' \item{sort (its levels) according to a grouping variable}
#' }
#' @param data data.frame with data
#' @param paramVar string, variable of \code{data} with parameter
#' @param paramGroupVar (optional) character vector with variable(s) of \code{data} with grouping.
#' If specified, the parameters will be grouped by this(these) variable(s) in the y-axis.
#' @param revert logical, if TRUE revert the order of the levels of the variable
#' @param width max number of characters in the code{paramVar} parameter.
#' @return vector with re-formatted \code{paramVar}, NULL if empty
#' @author Laure Cougnaud
#' library(ggplot2)
#' data(dataADaMCDISCP01)
#' dataAE <- dataADaMCDISCP01$ADAE
#' 
#' # by default, groups are sorted alphabetically in ggplot2 (from bottom to top for an histogram)
#' ggplot(data = dataAE, aes(y = AEDECOD, fill = AEBODSYS)) + geom_histogram(stat="count")
#' 
#' # by default: labels are set to a new line if more than 20 characters:
#' dataAE$AEDECOD <- formatVarForPlotLabel(data = dataAE, paramVar = "AEDECOD")
#' levels(dataAE$AEDECOD)
#' ggplot(data = dataAE, aes(y = AEDECOD, fill = AEBODSYS)) + geom_histogram(stat="count")
#' 
#' # revert order of the variable
#' dataAE$AEDECOD <- formatVarForPlotLabel(data = dataAE, paramVar = "AEDECOD", revert = TRUE)
#' levels(dataAE$AEDECOD)
#' ggplot(data = dataAE, aes(y = AEDECOD, fill = AEBODSYS)) + geom_histogram(stat="count")
#' 
#' # group based on body system
#' dataAE$AEDECOD <- formatVarForPlotLabel(data = dataAE, paramVar = "AEDECOD", paramGroupVar = "AEBODSYS")
#' ggplot(data = dataAE, aes(y = AEDECOD, fill = AEBODSYS)) + geom_histogram(stat="count")
#' @export
formatVarForPlotLabel <- function(data, 
	paramVar = NULL, paramGroupVar = NULL, 
	revert = FALSE,
	width = 20){
	
	res <- if(!is.null(paramVar)){
		
		paramVarVect <- data[, paramVar]
		if(!is.factor(paramVarVect))
			paramVarVect <- as.factor(paramVarVect)
		
		# cut too long labels
		paramVarLevels <- formatLongLabel(x = levels(paramVarVect), width = width)
		
		# convert paramVar
		# don't use directly ( labels to avoid error: duplicated factor levels
		paramVarVectNew <- paramVarLevels[as.character(paramVarVect)]
		paramVarVect <- factor(paramVarVectNew,	levels = unique(paramVarLevels))
		
		# if paramGroupVar is specified: change order levels of 'variable'
		if(!is.null(paramGroupVar)){
			
			paramGroupVarInData <- paramGroupVar %in% names(data)
			
			if(any(!paramGroupVarInData))
				warning("The variable(s): ", toString(shQuote(paramGroupVar[!paramGroupVarInData])),
						" used for grouping are not used because they are not available in the data.")
			
			if(any(paramGroupVarInData)){
				
				paramGroupVar <- paramGroupVar[paramGroupVarInData]
					
				convertToFactor <- function(x){
					if(!is.factor(x)){
						factor(x)
					}else	x
				}
				data[, paramGroupVar] <- colwise(convertToFactor)(data[, paramGroupVar, drop = FALSE])
				groupVariable <- interaction(data[, paramGroupVar, drop = FALSE], lex.order = TRUE)
		
				# order based on specified grouping variable
				# and secondly based on the paramVar 
				# (if multiple records have the same group var)
				idxOrder <- order(groupVariable, paramVarVect)
				paramVarLevels <- unique(as.character(paramVarVect[idxOrder]))
				paramVarVect <- factor(paramVarVect, levels = paramVarLevels)
				
			}
		}
		
		if(revert){
			factor(paramVarVect, levels = rev(levels(paramVarVect)))
		}else	paramVarVect
		
	}
	
	return(res)
	
}

#' Format a variable with long labels
#' 
#' This function formats a variable with long labels by wrapping
#' its elements into multiple lines.
#' @param x character vector with labels to format
#' @param width target maximum size. Note: a word
#' longer that this width won't be split (see \code{\link{strwrap}}).
#' @return vector with formatted labels
#' @author Laure Cougnaud
#' longLabel <- "This is a very long description of the variable in the dataset"
#' cat(longLabel)
#' cat(formatLongLabel(longLabel))
#' @export
formatLongLabel <- function(x, width = 20){
	
	xRF <- vapply(x, function(x1)
		xRF <- paste(strwrap(x1, width = width), collapse = "\n"),
		FUN.VALUE = character(1)
	)
	if(length(x) != length(xRF))
		stop("Reformatting of labels failed.")
	
	return(xRF)
	
}