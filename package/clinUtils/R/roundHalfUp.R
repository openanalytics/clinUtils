#' Round a number with 'round-up' strategy in text format.
#' 
#' This function rounds numbers with a 'round-up' strategy 
#' for a specific number of digits,
#' and format number to a: 'xxx.xxx' text.
#' 
#' The following workflow is used:
#' \enumerate{
#' \item{numbers are rounded with the \code{\link{roundHalfUp}}
#' function, see the \code{? roundHalfUp} for more details
#' on the rounding strategy}
#' \item{round numbers are formatted to character in
#' the format: 'xxx.xxx' with pads leading zeros}
#' }
#' @param x Numeric vector to round.
#' @param digits Integer with number of digits to consider, 0 by default.
#' @return A character vector with the rounded number.
#' NA values are returned as 'NA' as string.
#' @author Laure Cougnaud and Michela Pasetto
#' @seealso \link{roundHalfUp} for the rounding customization.
#' @examples 
#' # number of digits higher than number of decimal
#' roundHalfUpTextFormat(x = c(0.345, 0.567, -0.98), digits = 2)
#' # number of digits lower than number of decimal
#' roundHalfUpTextFormat(x = c(0.345, 0.567, -0.98), digits = 0)
#' # by default, 'digits' is 0!
#' roundHalfUpTextFormat(x = c(0.345, 0.567, -0.98))
#' # padding zeros
#' roundHalfUpTextFormat(1.23, 10)
#' @export
roundHalfUpTextFormat <- function(x, digits = 0) {
  
  z <- roundHalfUp(x = x, digits = digits)
  res <- formatC(z, digits = digits, format = "f", flag = "0")
  
  return(res)
  
}

#' Custom round function, with 'rounding up' strategy 
#' for rounding off a 5.
#' 
#' This function rounds a number for a specified number of digits.
#' It rounds off to the highest number for a 5.
#' The default R \code{\link{round}} function rounds to the
#' 'even digit' in case of rounding off a 5 
#' (see 'Details' section in \code{? round}).
#' This function instead rounds up to the nearest number for a 5. 
#' It mimics a similar rounding strategy used in SAS.
#' See examples for the difference between \code{\link{round}} and
#' 'roundHalfUp' below.
#' @param x Numeric vector to round.
#' @param digits Integer with number of digits to consider, 0 by default.
#' @return Rounded numeric vector.
#' @author stackoverflow question 6461209
#' @examples
#' # numbers are rounded to the closest even number in case of .5 
#' # with the round 'base' function
#' round(0.45, 1)
#' # 'roundHalfUp' always round to the next highest number in case of .5
#' roundHalfUp(0.45, 1)
#' # rounding is the same for uneven number:
#' round(0.55, 1)
#' roundHalfUp(0.55)
#' # other examples
#' round(1.456e-2, digits = 3)
#' round(1.456e-2, digits = 2)
#' round(1.456e-2, digits = 1)
#' @export
roundHalfUp <- function(x, digits = 0) {
  
  x <- x + abs(x) * sign(x) * .Machine$double.eps
  z <- round(x, digits = digits)
  
  return(z)
  
}
