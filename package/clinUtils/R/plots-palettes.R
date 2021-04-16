#' @importFrom viridisLite viridis
clinColors <- viridisLite::viridis

# see: ggplot2:::translate_shape_string
clinShapes <- c(
    "circle filled", 		# 21
    "square filled", 		# 22
    "diamond filled", 		# 23
    "triangle filled", 		# 24
    "triangle down filled", # 25
    "square open", 			# 0
    "circle open", 			# 1
    "triangle open", 		# 2
    "plus", 				# 3
    "cross", 				# 4
    "diamond open" , 		# 5
    "triangle down open", 	# 6
    "square cross", 		# 7
    "asterisk", 			# 8
    "diamond plus", 		# 9
    "circle plus", 			# 10
    "star", 				# 11
    "square plus", 			# 12
    "circle cross", 		# 13
    "square triangle", 		# 14
#	"triangle square" = 14
    "square", 				# 15 (comparator)
#	"circle small" = 16 # remove it: similar to 'circle'
    "triangle", 			# 17
    "diamond", 				# 18
    "circle" 				# 19
#	"bullet"                = 20 # remove it: similar to 'circle'
)

clinLinetypes <- c(
    "solid", "dashed", "dotdash",
    "twodash", "dotted", "longdash"
) 

#' Get a color palette for clinical
#' visualizations.
#' 
#' Get a color palette of specified length,
#' either from a vector of names for the palette, or
#' from a specified length.
#' @param n Integer of length 1, number of elements in palette.
#' @param x Vector with elements used for palette.
#' If factor, the levels are used, otherwise the unique elements of the vector.
#' Missing values are automatically removed, excepted if
#' \code{includeNA} is set to TRUE.
#' @param includeNA Logical (FALSE by default), 
#' should NA elements be retained in the palette in case
#' \code{x} is specified.
#' @param defaultSettings A function or a vector, for custom colors.
#' Default is the the colorblind 
#' \code{\link[viridisLite]{viridis}} color palette.
#' @return Vector of colors,
#' named with the elements in \code{x} if \code{x} is specified.
#' @author Laure Cougnaud and Michela Pasetto
#' @examples 
#' # extract longest palette available
#' getColorPalette(n = 11)
#' # extract palette for a vector
#' getColorPalette(x = paste('treatment', 1:4))
#' # possibility to include missing values:
#' getColorPalette(x = c(NA_character_, "group1"), includeNA = TRUE)
#' # change default settings
#' getColorPalette(n = 3, defaultSettings = c("red", "green", "grey"))
#' @importFrom viridisLite viridis
#' @export
getColorPalette <- function(
    n = NULL, 
    x = NULL,
    includeNA = FALSE,
    defaultSettings = clinColors
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
  
  palette <- evaluatePalette(palette = defaultSettings, n = n)
  
  if(!is.null(x)) {
    
    # extract palette names (based on x)
    namesX <- rep(NA_character_, length = n)
    
    namesX[which(is.na(namesX))] <- setdiff(x, namesX)
    
    # set palette names to x
    names(palette) <- namesX
    palette <- palette[match(x, names(palette))]
    
  } else {
    
    # remove names
    palette <- unname(palette)
    
  }
  
  return(palette)
  
}

evaluatePalette <- function(palette, n) {
  
  if(is.function(palette)) {
    paletteVect <- palette(n)
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
#' @param defaultSettings A function or a vector, for custom shapes.
#' Default is the \code{\link{clinShapes}} shape palette.
#' @param asText Logical (FALSE by default), should the palette
#' be expressed as integer (base R plot and ggplot2 compatible)
#' or in text format (e.g. required if combined with unicode symbols in ggplot2)?
#' @inheritParams getColorPalette
#' @return vector of shape values
#' @author Laure Cougnaud and Michela Pasetto
#' @examples
#' #' extract longest shape palette available
#' getShapePalette(n = 19)
#' # extract palette for a vector
#' getShapePalette(x = paste('treatment', 1:4))
#' # include missing
#' getShapePalette(x = c(NA_character_, "group1"), includeNA = TRUE)
#' # change default settings
#' getShapePalette(x = paste('treatment', 1:2), defaultSettings = c("circle", "triangle"))
#' # get symbols as 'text' (e.g. to be combined with Unicode in ggplot2)
#' getShapePalette(x = paste('treatment', 1:4), asText = TRUE)
#' @export
getShapePalette <- function(
    n = NULL, 
    x = NULL,
    includeNA = FALSE,
    asText = FALSE,
    defaultSettings = clinShapes
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
  palette <- evaluatePalette(palette = defaultSettings, n = n)
  
  if(!is.null(x)) {
    
    # extract palette names (based on x)
    namesX <- rep(NA_character_, length = n)
    namesX[which(is.na(namesX))] <- setdiff(x, namesX)
    
    # shape as text (if specified)
    if(asText)	names(palette) <- palette # palette <- names(palette)
    
    # set palette names to x
    names(palette) <- namesX
    palette <- palette[match(x, names(palette))]
    
  } else {
    
    # shape as text (if specified)
    if(asText)	names(palette) <- palette # palette <- names(palette)
    
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
#' @inheritParams getColorPalette
#' @param defaultSettings A function or a vector, for custom linetypes.
#' Default is the \code{\link{clinLinetypes}} linetype palette.
#' @return character vector values with linetype
#' @author Laure Cougnaud and Michela Pasetto
#' @examples
#' # extract longest linetype palette available
#' getLinetypePalette(n = 6)
#' # extract palette for a vector
#' getLinetypePalette(x = paste('treatment', 1:4))
#' # include missing
#' getLinetypePalette(x = c(NA_character_, "group1"), includeNA = TRUE)
#' # set custom linetypes
#' getColorPalette(n = 2, defaultSettings = c("twodash", "dashed"))
#' @export
getLinetypePalette <- function(
    n = NULL, 
    x = NULL,
    includeNA = FALSE,
    defaultSettings = clinLinetypes
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
  
  palette <- evaluatePalette(palette = defaultSettings, n = n)
  
  if(!is.null(x)) {
    
    # extract palette names (based on x)
    namesX <- rep(NA_character_, length = n)
    
    namesX[which(is.na(namesX))] <- setdiff(x, namesX)
    
    # set palette names to x
    names(palette) <- namesX
    palette <- palette[match(x, names(palette))]
    
  } else {
    palette <- unname(palette)
  }
  
  return(palette)
  
}