#' Include a list of plots in a knitr document
#' 
#' Each plot is included (internally) in a separated chunk,
#' so different chunk options can be set for each plot.\cr
#' For example, plots can be created with different
#' figure height or width (see examples).
#' 
#' This function should be called within a chunk
#' with the following option: \strong{\code{results = 'asis'}}.\cr
#' Note that a (one-level) list of \code{plotly} plots can 
#' also be included directly via
#' \code{htmltools::tagList(listPlots)}, but without the possibility to 
#' have different chunk option for each plot.
#' @param plotsList list of plots, e.g. \code{ggplot} objects
#' from the \code{ggplot2} package or from the \code{plotly} packages.
#' @param generalLabel general label for the chunks, used to build the \code{labels}.
#' The labels are constructed as '\code{generalLabel}[i]',
#' with i the plot number (from sequence spanning the length of \code{plotsList}).
#' Only use if \code{labels} is not specified.
#' @param type string with plot type: 'ggplot2' or 'plotly'
#' @inheritDotParams knitPrintListObjects -xList -printObject
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