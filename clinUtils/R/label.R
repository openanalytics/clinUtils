#' Concatenate and format text strings to a label of a table
#' 
#' This function concatenates and formats 
#' text strings to a label of a table for \code{bookdown} package
#' @param ... string to be concatenated to form label
#' @return String with chunk label
#' @author Laure Cougnaud
#' @export
formatTableLabel <- function(...){
	
	label <- paste0("(\\#tab:", formatLabel(...), ")")
	return(label)
	
}

#' Concatenate and format text strings to a chunk label
#' 
#' @param ... string to be concatenated to form chunk label
#' @return String with chunk label
#' @author Laure Cougnaud
#' @export
formatLabelChunk <- function(...)
	formatLabel(...)

#' Concatenate and format text strings to a label
#' 
#' This function concatenates and formats 
#' text strings to a label e.g to use for chunk and table/figures
#' @param ... string(s) to be concatenated to form label
#' or data.frame with only one row.
#' If an unique data.frame is specified, the
#' different columns are collapsed to form one label.
#' @return String with chunk label
#' @author Laure Cougnaud
#' @export
formatLabel <- function(...){
	
	args <- list(...)
	args <- args[!is.null(args)]
	args <- sapply(args, function(x){
		if(is.data.frame(x)){
			if(nrow(x) > 1)
				stop("Dataframe should contain only one record (row).")
			paste(sapply(x, as.character), collapse = "-")
		}else	x
	})
	argsRF <- sapply(args, function(x) 
		gsub(":|\\_| |\\.|%|>|<|/", "", x), 
	simplify = FALSE)
	
	label <- do.call(paste, c(argsRF, sep = "-"))
	return(label)
	
}