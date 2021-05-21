#' Include a list of objects in a knitr document
#' 
#' This functions includes a list of objects in a knitr document 
#' allowing different chunk options
#' (e.g. \code{flextable} object).
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
#' @example inst/examples/knitPrintListPlots-example.R
#' @return no returned value, a text is printed with chunk content
#' @author Laure Cougnaud
#' @importFrom knitr knit_expand knit
#' @example inst/examples/knitPrintListObjects-example.R
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
	
	if(length(argsChunk) > 0){
		
		idxArgsChunkCharac <- which(sapply(argsChunk, is.character))
		
		if(length(idxArgsChunkCharac) > 0){
			argsChunk[idxArgsChunkCharac] <- lapply(argsChunk[idxArgsChunkCharac], function(x){
				paste0("'", x, "'")	
			})
		}
		
		argsChunkTxt <- paste0(names(argsChunk), "=", "{{", names(argsChunk), "}}")
		
	}else	argsChunkTxt <- NULL
	
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

