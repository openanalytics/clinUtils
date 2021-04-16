#' @importFrom viridisLite viridis
clinColors <- viridisLite::viridis

# see: ggplot2:::translate_shape_string
pch_table <- c(
	"square open"           = 0,
	"circle open"           = 1,
	"triangle open"         = 2,
	"plus"                  = 3,
	"cross"                 = 4, 
	"diamond open"          = 5,
	"triangle down open"    = 6,
	"square cross"          = 7,
	"asterisk"              = 8,
	"diamond plus"          = 9,
	"circle plus"           = 10,
	"star"                  = 11,
	"square plus"           = 12,
	"circle cross"          = 13,
	"square triangle"       = 14,
#	"triangle square"       = 14,
	"square"                = 15, 
	"circle small"          = 16, # remove it: similar to 'circle'
	"triangle"              = 17,
	"diamond"               = 18,
	"circle"                = 19,
	"bullet"                = 20, # remove it: similar to 'circle'
	# filled symbols
	"circle filled"         = 21,
	"square filled"         = 22,
	"diamond filled"        = 23,
	"triangle filled"       = 24,
	"triangle down filled"  = 25
)

clinShapes <- c(
	# filled symbols
	seq(from = 21, to = 25), 
	# other symbols
	setdiff(seq(from = 0, to = 19), 16)
)
clinShapesText <- names(pch_table)[match(clinShapes, pch_table)]
		
clinLinetypes <- c(
    "solid", "dashed", "dotdash",
    "twodash", "dotted", "longdash"
) 

#' Parameters for all patient profiles visualization palette functions.
#' @param n Integer of length 1, number of elements in palette.
#' @param x Vector with elements used for palette.
#' If factor, the levels are used, otherwise the unique elements of the vector.
#' Missing values are automatically removed, excepted if
#' \code{includeNA} is set to TRUE.
#' @param includeNA Logical (FALSE by default), 
#' should NA elements be retained in the palette in case
#' \code{x} is specified?
#' @name clinUtils-palette
NULL

#' Get a color palette for clinical
#' visualizations.
#' 
#' Get a color palette of specified length,
#' either from a vector of names for the palette, or
#' from a specified length.
#' @param palette A function or a vector, for custom colors.
#' Default is the the colorblind 
#' \code{\link[viridisLite]{viridis}} color palette.
#' @inheritParams clinUtils-palette
#' @return Vector of colors,
#' named with the elements in \code{x} if \code{x} is specified.
#' @author Laure Cougnaud and Michela Pasetto
#' @examples 
#' # extract longest palette available
#' getColorPalette(n = 11)
#' # extract palette for a vector
#' getColorPalette(x = paste('treatment', 1:4))
#' # possibility to include missing values:
#' getColorPalette(x = c(NA_character_, "group1"), includeNA = FALSE)
#' getColorPalette(x = c(NA_character_, "group1"), includeNA = TRUE)
#' # change default settings
#' getColorPalette(n = 3, palette = c("red", "green", "grey"))
#' @importFrom viridisLite viridis
#' @export
getColorPalette <- function(
    n = NULL, 
    x = NULL,
    includeNA = FALSE,
    palette = clinColors
)
{
  
  if(is.null(x) & is.null(n))
    stop("A vector ('x') or number of colors ('n') ",
        "should be specified.")
  
  if(!is.null(x)) {
    x <- if(is.factor(x))	levels(x)	else unique(x)
    if(includeNA){
      x[is.na(x)] <- 'NA'
    } else {
      x <- x[!is.na(x)]
    }
  } else	if(is.na(n) || length(n) != 1)
    stop("'n' should be a non missing integer of length 1.")
  
  if(is.null(n)) n <- length(x)
  
  palette <- evaluatePalette(palette = palette, n = n)
  
  if(!is.null(x)) {
	# set palette names to x
    names(palette) <- x
  } else {
    # remove names
    palette <- unname(palette)
  }
  
  return(palette)
  
}

evaluatePalette <- function(palette, n) {
  
  if(is.function(palette)) {
    paletteVect <- palette(n)
	if(length(paletteVect) != n)
		stop(paste("If 'palette' is specified as a function",
			"it should return a vector of length 'n'",
			"with n the input number."))
  } else if(is.vector(palette)){
    paletteVect <- rep(palette, length.out = n)
  }else	stop("'palette' should be a vector or a function.")
  
  return(paletteVect)
  
}

#' Get a shape palette for clinical
#' visualizations.
#' 
#' Get a shape palette of specified length,
#' either from a vector of names for the palette, or
#' from a specified length.
#' 
#' Note that 19 unique symbols are available at maximum
#' (replicated if necessary).
#' @param palette A function or a vector, for custom shapes.\cr
#' The vector should be a character if \code{asText} is set to TRUE.\cr
#' Default is the \code{\link{clinShapes}} shape palette,
#' or \code{\link{clinShapesText}} is \code{asText} is set to TRUE.
#' @param asText Logical (FALSE by default), should the palette
#' be expressed as integer (base R plot and ggplot2 compatible)
#' or in text format 
#' (e.g. required if combined with unicode symbols in ggplot2)?
#' @inheritParams clinUtils-palette
#' @return Vector of shapes,
#' named with the elements in \code{x} if \code{x} is specified.
#' @author Laure Cougnaud and Michela Pasetto
#' @examples
#' #' extract longest shape palette available
#' getShapePalette(n = 19)
#' # extract palette for a vector
#' getShapePalette(x = paste('treatment', 1:4))
#' # include missing
#' getShapePalette(x = c(NA_character_, "group1"), includeNA = TRUE)
#' getShapePalette(x = c(NA_character_, "group1"), includeNA = FALSE)
#' # change default settings
#' getShapePalette(x = paste('treatment', 1:3), palette = c("circle", "triangle"))
#' # get symbols as 'text' (e.g. to be combined with Unicode in ggplot2)
#' getShapePalette(x = paste('treatment', 1:4), asText = TRUE)
#' @export
getShapePalette <- function(
    n = NULL, 
    x = NULL,
    includeNA = FALSE,
    asText = FALSE,
    palette = if(asText){clinShapesText}else{clinShapes}
) {
  
  if(is.null(x) & is.null(n))
    stop("A vector ('x') or number of shapes ('n') ",
        "should be specified.")
  
  if(!is.null(x)){
    x <- if(is.factor(x))	levels(x)	else unique(x)
    if(includeNA){
      x[is.na(x)] <- 'NA'
    }else{
      x <- x[!is.na(x)]
    }
  }else	if(is.na(n) || length(n) != 1)
    stop("'n' should be a non missing integer of length 1.")
  
  if(is.null(n)) n <- length(x)
  
  # entire palette
  palette <- evaluatePalette(palette = palette, n = n)
  
  # shape as text (if specified)
  if(asText && !is.character(palette))
	stop(paste("'palette' should be a character vector",
		"if 'asText' is set to TRUE."))
  
  if(!is.null(x)) {
    # set palette names to x
    names(palette) <- x
  } else {
    # remove names
    palette <- unname(palette)
  }
  
  return(palette)
  
}

#' Get a linetype palette for clinical
#' visualizations.
#'
#' Get a linetype palette of specified length,
#' either from a vector of names for the palette, or
#' from a specified length.
#' 
#' Note that 7 unique symbols are available at maximum
#' (replicated if necessary).
#' @param palette A function or a vector, for custom linetypes.
#' Default is the \code{\link{clinLinetypes}} linetype palette.
#' @inheritParams clinUtils-palette
#' @return Vector with linetypes,
#' named with the elements in \code{x} if \code{x} is specified.
#' @author Laure Cougnaud and Michela Pasetto
#' @examples
#' # extract longest linetype palette available
#' getLinetypePalette(n = 6)
#' # extract palette for a vector
#' getLinetypePalette(x = paste('treatment', 1:4))
#' # include missing
#' getLinetypePalette(x = c(NA_character_, "group1"), includeNA = TRUE)
#' getLinetypePalette(x = c(NA_character_, "group1"), includeNA = FALSE)
#' # set custom linetypes
#' lty <- getColorPalette(n = 3, palette = c("twodash", "dashed"))
#' @export
getLinetypePalette <- function(
    n = NULL, 
    x = NULL,
    includeNA = FALSE,
    palette = clinLinetypes
) {
  
  if(is.null(x) & is.null(n))
    stop("A vector ('x') or number of linetypes ('n') ",
        "should be specified.")
  
  if(!is.null(x)){
    x <- if(is.factor(x))	levels(x)	else unique(x)
    if(includeNA){
      x[is.na(x)] <- 'NA'
    }else{
      x <- x[!is.na(x)]
    }
  }else	if(is.na(n) || length(n) != 1)
    stop("'n' should be a non missing integer of length 1.")
  
  if(is.null(n)) n <- length(x)
  
  palette <- evaluatePalette(palette = palette, n = n)
  
  if(!is.null(x)) {
    # set palette names to x
    names(palette) <- x
  } else {
	# remove names  
    palette <- unname(palette)
  }
  
  return(palette)
  
}