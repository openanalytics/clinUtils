#' @importFrom viridisLite viridis
clinColors <- viridisLite::viridis


#' Get a color blind palette
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
#' Default is the \code{\link[viridisLite]{viridis}} color palette.
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
  
  palette <- defaultSettings
  palette <- if(is.function(palette)) {
        palette(n)
      } else {
        rep(palette, length.out = n)
      }
  
  
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

#' Get a shape palette of specified length,
#' either from a vector of names for the palette, or
#' from a specified length.
#' 
#' Note that 19 unique symbols are available at maximum
#' (replicated if necessary).
#' @param xPlacebo (optional) String with element of \code{x}
#' containing placebo, for which 'plus' should be used.
#' @param xComparator (optional) String with element of \code{x}
#' containing comparator, for which 'square' should be used.
#' @param asText Logical (FALSE by default), should the palette
#' be expressed as integer (base R plot and ggplot2 compatible)
#' or in text format (e.g. required if combined with unicode symbols in ggplot2)?
#' @inheritParams getColorPalette
#' @return vector of shape values
#' @author Laure Cougnaud
#' @examples
#' #' extract longest shape palette available
#' getShapePalette(n = 19)
#' # extract palette for a vector
#' getShapePalette(x = paste('treatment', 1:4))
#' # include missing
#' getShapePalette(x = c(NA_character_, "group1"), includeNA = TRUE)
#' # get symbols as 'text' (e.g. to be combined with Unicode in ggplot2)
#' getShapePalette(x = paste('treatment', 1:4), asText = TRUE)
#' @export
getShapePalette <- function(
    n = NULL, 
    x = NULL, xPlacebo = NULL, xComparator = NULL,
    includeNA = FALSE,
    asText = FALSE
) {
  
  if(is.null(x) & is.null(n))
    stop("A vector ('x') or number of colors ('n') ",
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
  palette <- glpgShape()
  
  # if required, include correct palette for placebo and comparator
  paletteReq <- c(
      if(!is.null(xPlacebo)){
        if(xPlacebo %in% x){
          "plus"
        }else	warning("'xPlacebo' is not available in 'x'.")
      },
      if(!is.null(xComparator)){
        if(xComparator %in% x){
          "square"
        }else	warning("'xComparator' is not available in 'x'.")
      }
  )
  palette <- c(palette[paletteReq], palette[which(!names(palette) %in% paletteReq)])
  palette <- rep(palette, length.out = n)
  
  if(!is.null(x)){
    
    # extract palette names (based on x)
    namesX <- rep(NA_character_, length = n)
    
    if(!is.null(xPlacebo) && !is.na(xPlacebo) && xPlacebo %in% x){
      namesX[match("plus", names(palette))] <- xPlacebo
    }	
    if(!is.null(xComparator) && !is.na(xComparator) && xComparator %in% x){
      namesX[match("square", names(palette))] <- xComparator
    }
    namesX[which(is.na(namesX))] <- setdiff(x, namesX)
    
    # shape as text (if specified)
    if(asText)	palette <- names(palette)
    
    # set palette names to x
    names(palette) <- namesX
    palette <- palette[match(x, names(palette))]
    
  }else{
    
    # shape as text (if specified)
    if(asText)	palette <- names(palette)
    
    # remove names
    palette <- unname(palette)
    
  }
  
  return(palette)
  
}

#' Get a linetype palette of specified length,
#' either from a vector of names for the palette, or
#' from a specified length.
#' 
#' Note that 7 unique symbols are available at maximum
#' (replicated if necessary).
#' @param xPlacebo (optional) String with element of \code{x}
#' containing placebo, for which 'longdash' should be used.
#' @param xComparator (optional) String with element of \code{x}
#' containing comparator, for which 'dotted' should be used.
#' @inheritParams getColorPalette
#' @return character vector values with linetype
#' @author Laure Cougnaud
#' @examples
#' # extract longest linetype palette available
#' getLinetypePalette(n = 6)
#' # extract palette for a vector
#' getLinetypePalette(x = paste('treatment', 1:4))
#' # include missing
#' getLinetypePalette(x = c(NA_character_, "group1"), includeNA = TRUE)
#' @export
getLinetypePalette <- function(
    n = NULL, 
    x = NULL, xPlacebo = NULL, xComparator = NULL,
    includeNA = FALSE
) {
  
  if(is.null(x) & is.null(n))
    stop("A vector ('x') or number of colors ('n') ",
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
  
  palette <- c("solid", "dashed", "dotdash", "twodash", "dotted", "longdash") 
  
  # if required, include correct palette for placebo and comparator
  if(!is.null(x)){
    paletteReq <- c(
        if(!is.null(xPlacebo)){
          if(xPlacebo %in% x){
            "longdash"
          }else	warning("'xPlacebo' is not available in 'x'.")
        },
        if(!is.null(xComparator)){
          if(xComparator %in% x){
            "dotted"
          }else	warning("'xComparator' is not available in 'x'.")
        }
    )
    palette <- c(paletteReq, setdiff(palette, paletteReq))
  }
  
  palette <- rep(palette, length.out = n)
  
  if(!is.null(x)){
    
    # extract palette names (based on x)
    namesX <- rep(NA_character_, length = n)
    
    if(!is.null(xPlacebo) && !is.na(xPlacebo) && xPlacebo %in% x){
      namesX[match("longdash", palette)] <- xPlacebo
    }	
    if(!is.null(xComparator) && !is.na(xComparator) && xComparator %in% x){
      namesX[match("dotted", palette)] <- xComparator
    }
    namesX[which(is.na(namesX))] <- setdiff(x, namesX)
    
    # set palette names to x
    names(palette) <- namesX
    palette <- palette[match(x, names(palette))]
    
  }else{
    palette <- unname(palette)
  }
  
  return(palette)
  
}