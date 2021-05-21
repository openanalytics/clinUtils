#' Include a list of plots in a knitr document
#' 
#' This function includes a list of plots with different figure height, 
#' size, label in a knitr document.
#' The reason is that the chunck options 
#' \code{fig.height} and \code{fig.width} are not vectorized.
#' 
#' Note: the chunk in which this function is called should be 
#' set with the following option: \strong{\code{results = 'asis'}}.\cr
#' Note that a (one-level) list of \code{plotly} plots can also be included directly via
#' \code{htmltools::tagList(listPlots)}, but without the possibility to specify
#' title and/or change chunk option(s).
#' Each plot will be included in a separated chunk.\cr
#' @param plotsList list of plots, e.g. \code{ggplot} objects
#' from the \code{ggplot2} package or from the \code{plotly} packages.
#' @param generalLabel general label for the chunks, used to build the \code{labels}.
#' The labels are constructed as '\code{generalLabel}[i]',
#' with i the plot number (from sequence spanning the length of \code{plotsList}).
#' Only use if \code{labels} is not specified.
#' @param type string with plot type: 'ggplot2' or 'plotly'
#' @param ... Additional parameters for the \code{\link{knitPrintListObjects}} function.
#' @inherit knitPrintListObjects return
#' @author Laure Cougnaud
#' @example inst/examples/knitPrintListPlots-example.R
#' @export
knitPrintListPlots <- function(
	plotsList, 
	generalLabel = "plotsList",
	type = c("ggplot2", "plotly"),
	...){
	
	type <- match.arg(type)

	knitPrintListObjects(
		xList = plotsList, 
		generalLabel = generalLabel,
		printObject = (type == "ggplot2"),
		...		
	)
	
	
}