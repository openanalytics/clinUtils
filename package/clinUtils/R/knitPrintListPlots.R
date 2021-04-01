#' Include a list of plots with different figure height, 
#' size, label in a knitr document.
#' Reason: fig.height and fig.width options are not vectorized.
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
#' @examples 
#' # Note: the following code should be included within a chunk of a knitr (e.g. RMarkdown) document
#' # to include/export a list of figures in the Rmarkdown output
#' data(iris)
#' 
#' # include a list of static plots
#' library(ggplot2)
#' plotsListStatic <- list(
#' 	point = ggplot(data = cars, aes(x = speed, y = dist)) + geom_point(),
#' 	line = ggplot(data = cars, aes(x = speed, y = dist)) + geom_line()
#' )
#' # with general label (used to name exported figure)
#' knitPrintListPlots(
#'   plotsList = plotsListStatic, 
#'   generalLabel = "scatter-cars"
#' )
#' # with label for each plot (used to name exported figure)
#' knitPrintListPlots(
#'   plotsList = plotsListStatic, 
#'   labels = names(plotsListStatic)
#' )
#' # with section header (header of level 1 in Markdown)
#' knitPrintListPlots(
#'   plotsList = plotsListStatic, 
#'   titles = names(plotsListStatic), 
#'   titleLevel = 3
#' )
#' # specify caption and dimension for each figure (figure height is replicated)
#' knitPrintListPlots(
#'   plotsList = plotsListStatic, 
#'   fig.cap = names(plotsListStatic), 
#'   fig.width = 3*seq_along(plotsListStatic), 
#'   fig.height = 6
#' )
#' 
#' # include a list of interactive plots
#' library(plotly)
#' plotsListInteractive <- list(
#' 	point = plot_ly(data = cars, x = ~speed, y = ~dist, type = "scatter", mode = "marker"),
#' 	line = plot_ly(data = cars, x = ~speed, y = ~dist, type = "scatter", mode = "line")
#' )
#' knitPrintListPlots(
#'  plotsList = plotsListInteractive, 
#'   type = "plotly", 
#'   titles = names(plotsListInteractive), 
#'   titleLevel = 3
#' )
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

#' Include a list of objects with possibly different chunk options
#' in a knitr document (e.g. \code{flextable} object).
#' 
#' Note: the chunk in which this function is called should be 
#' set with the following option: \strong{\code{results = 'asis'}}.
#' @param xList List of objects to print.
#' @param generalLabel String with general label for the chunks, 
#' used to build the \code{labels}.
#' The labels are constructed as '\code{generalLabel}[i]',
#' with i the list index.
#' Only use if \code{labels} is not specified.
#' @param labels Character vector with labels,
#' one for each chunk.
#' @param titles Character vector with section titles,
#' one for each chunk.
#' @param titleLevel Integer with level for section header,
#' 1 for top-level section header.
#' @param printObject Logical, if TRUE (FALSE by default),
#' each object within \code{xList} is explicitely printed 
#' with the \code{\link{print}} function.
#' @param ... any chunk parameters, will be replicated if necessary,
#' at the exception of 'results', set to 'asis' and 'echo' set to FALSE
#' internally.
#' See \code{\link{knitr}[opts_chunk]} for further details on available options.
#' @return no returned value, a text is printed with chunk content
#' @author Laure Cougnaud
#' @importFrom knitr knit_expand knit
#' @examples 
#' \dontrun{
#' # Note: the following code should be included within a chunk of a knitr (e.g. RMarkdown) document
#' # to include/export a list of figures in the Rmarkdown output
#' 
#' # list of flextable objects
#' library(flextable)
#' listTables <- list(flextable(iris), flextable(cars))
#' knitPrintListObjects(xList = listTables)
#' }
#' @export
knitPrintListObjects <- function(
	xList, 
	generalLabel = "objectsList",
	labels = paste0(generalLabel, seq_along(xList)), 
	titles = NULL, titleLevel = 2,
	printObject = FALSE,
	...){

	# based on chunks
	
	# escape any non alnum character in the chunk label
	labels <- paste0("'", labels, "'")
	
	# additional chunk arguments
	argsChunk <- list(...)
	if("fig.cap" %in% names(argsChunk)){
		argsChunk[["fig.cap"]] <- paste0("'", argsChunk[["fig.cap"]], "'")
	}
	argsChunkTxt <- if(length(argsChunk) > 0)
		paste0(names(argsChunk), "=", "{{", names(argsChunk), "}}"
		)
	
	# chunk general template
	# seems that plot object cannot be passed as argument to knit_expand?
	chunkTemplate <- paste0(
		"```{r {{label}}", 
		if(!is.null(argsChunkTxt)) paste0(", ", toString(argsChunkTxt)), ", results = 'asis', echo = FALSE}\n",
		if(!is.null(titles))	
			paste0('cat("\\n", paste(rep("#", titleLevel), collapse = ""), " {{title}}\\n", sep = "")\n'),
		if(printObject)	"print(", 
		"xList[[{{i}}]]",
		if(printObject)	")", 
		"\n",
		"```\n"
	)
	
	# vectorize over plots
	argsKnitExpand <- c(
		list(
			FUN = knit_expand, 
			text = chunkTemplate,
			i = seq_along(xList), 
			label = labels
		),
		if(!is.null(titles))	list(title = titles),
		argsChunk
	)
	chunkTxt <- do.call(mapply, argsKnitExpand)
	
	# run chunks
	cat(knit(text = paste(chunkTxt, collapse = "\n"), quiet = TRUE))
	
}

