#'@importFrom utils globalVariables
utils::globalVariables(c("shapePaletteNRIND", "colorPaletteNRIND"))

#' Shape palette for a standard CDISC Normal/Reference
#' Range Indicator.
#' 
#' These symbols should be supported in Windows and Linux.
#' @format A named character vector with shape symbol for 
#' typical Normal Reference Range Indicator variable:
#' \itemize{
#' \item "LOW": filled down-pointing arrow (\code{25})
#' \item "NORMAL": filled circle (21)
#' \item "HIGH": filled up-pointing arrow (\code{24})
#' \item "ABNORMAL": diamond (\code{18})
#' \item "UNKNOWN" or 'NA': cross (\code{3})
#' \item "NA": cross (\code{3})
#' }
"shapePaletteNRIND"

#' Color palette for a standard CDISC Normal/Reference
#' Range Indicator.
#' @format A named character vector with color for 
#' typical Normal Reference Range Indicator variable:
#' \itemize{
#' \item "LOW": orange
#' \item "NORMAL": green4
#' \item "HIGH": orange
#' \item "ABNORMAL": red
#' \item "UNKNOWN" or 'NA': grey
#' \item "NA": grey
#' }
"colorPaletteNRIND"

#' Get standard palette for typical CDISC variables.
#' 
#' The extraction of the palette elements is case-insensitive.
#' 
#' The order of the palette depends on the type of the input
#' variable (\code{x}):
#' \itemize{
#' \item if a factor is specified, the palette is ordered based
#' on its levels
#' \item if a character vector is specified,
#' the elements from the internal standard palette are used first,
#' the remaining elements are then sorted alphabetically.
#' }
#' @param x Character vector of factor with
#' variable to consider.
#' The palette is built based on the unique elements
#' of this vector, or levels if \code{x} is a factor.
#' @param var String with type of variable, among:
#' \itemize{
#' \item 'NRIND': Normal Reference Range Indicator
#' }
#' @param type String with type of palette:
#' \itemize{
#' \item 'shape': shape/symbol palette
#' \item 'color': color palette
#' }
#' @param palette (optional) Named vector
#' with extra palette, e.g. to specify elements
#' for non-standard categories.
#' This palette is combined with the standard palette.
#' @return Named vector with palette.
#' @examples 
#' 
#' ## palette for reference range indicator variables
#' 
#' xRIND <- c("LOW", "HIGH", "NORMAL", "NORMAL", "NORMAL", "ABNORMAL")
#' 
#' # get standard palette
#' getPaletteCDISC(x = xRIND, var = "NRIND", type = "shape")
#' getPaletteCDISC(x = xRIND, var = "NRIND", type = "color")
#' 
#' # in case extra categories are specified:
#' xRIND <- c(xRIND, "High Panic")
#' # the symbols are set to numeric symbols
#' getPaletteCDISC(xRIND, var = "NRIND", type = "shape")
#' # use shapePalette to specify symbols for extra categories
#' getPaletteCDISC(xRIND, var = "NRIND", type = "shape", palette = c("High Panic" = "\u2666"))
#' 
#' # palette is case-insensitive
#' xRIND <- c("Low", "High", "Normal", "Normal", "Normal")
#' getPaletteCDISC(xRIND, var = "NRIND", type = "shape")
#' @author Laure Cougnaud
#' @export
getPaletteCDISC <- function(x, 
	var, type, 
	palette = NULL){
	
	var <- match.arg(var, choices = "NRIND")
	type <- match.arg(type, choices = c("shape", "color"))

	if(!is.null(palette) && is.null(names(palette)))
		stop("'palette' should be named.")
	
	standardPalette <- switch(type,
		shape = switch(var, 
			NRIND = shapePaletteNRIND
		),
		color = switch(var, 
			NRIND = colorPaletteNRIND
		)
	)
	
	palette <- c(palette, standardPalette)
	palette <- palette[!duplicated(names(palette))]
	
	if(missing(x))
		return(palette)
	
	# extract unique elements
	xCat <- if(is.factor(x))	levels(x)	else unique(x)
	
	# extract symbols from standard palette
	xPaletteName <- sapply(xCat, function(cat){
		catPalette <- grep(
			pattern = paste0("^", cat, "$"),
			x = names(palette), 
			ignore.case = TRUE,
			value = TRUE
		)
		if(length(catPalette) == 0){
			NA_character_
		}else	catPalette
	})
	
	xPalette <- palette[xPaletteName]
	names(xPalette) <- xCat
	
	# add symbols for non-standard categories
	xExtra <- names(which(is.na(xPalette)))
	if(length(xExtra) > 0){
		# use 'match' in case x contains empty string ('')
		xPalette[match(xExtra, names(xPalette))] <- seq_along(xExtra)
	}
	
	if(!is.factor(x)){
		xPalette <- xPalette[order(
			match(xPaletteName, names(standardPalette)),
			xPaletteName
		)]
	}
	
	return(xPalette)	
	
}