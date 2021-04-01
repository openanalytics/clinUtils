#' Load data from ADaM/SDTM file(s).
#' 
#' This converts also date/time variable to R date/time class 
#' (see \code{\link{convertToDateTime}}) function.
#' @param files Character vector with path to ADaM or SDTM file(s).
#' Currently only import of files with extension: 'sas7bdat' or 'xpt' are supported.
#' @param convertToDate logical, if TRUE columns with date/time are converted to 
#' \code{\link{POSIXct}} format, which stores calendar date/time in R.
#' Please note that most of the time this is not necessary, as date variables
#' are automatically imported via the \code{haven} package if encoded correctly in the dataset.
#' @param dateVars vector of columns in \code{data} containing date/time,
#' or pattern for this columns.
#' By default all columns ending with 'DTC' are used (dateVars is: 'DTC$').
#' @param verbose logical, if TRUE (by default) progress messages are printed during execution.
#' @param encoding String with encoding, only used if \code{files} is of extension: 'sas7bdat',
#' 'UTF-8' by default.
#' @param ... Additional parameters for the \code{\link[haven]{read_sas}} or
#' \code{\link[haven]{read_xpt}} functions, depending on the input file type.
#' @return list of data.frame with data of each ADAM file (if not empty),
#' with special attributes 'labelVars': named vector with label of the variables.
#' Each data.frame contains an additional column: 'dataset' specifying the name of the 
#' \code{files} it was read from.
#' @author Laure Cougnaud
#' @importFrom tools file_path_sans_ext
#' @importFrom haven read_sas read_xpt
#' @importFrom plyr colwise
#' @importFrom tools file_ext
#' @examples
#' \dontrun{
#' dataFromSAS7bdat <- loadDataADaMSDTM(files = "ae.sas7bdat")
#'  attr(dataFromSAS7bdat, "labelVars") # column labels
#' dataFromXpt <- loadDataADaMSDTM(files = c("ae.xpt", "dm.xpt"))
#' attr(dataFromXpt, "labelVars") # column labels
#' }
#' @export
loadDataADaMSDTM <- function(files, 
	convertToDate = FALSE, dateVars = "DTC$",
	verbose = TRUE, 
	encoding = "UTF-8", 
	...){
	
	# extract ADaM name
	names(files) <- toupper(file_path_sans_ext(basename(files)))
	
	idxDuplFiles <- duplicated(names(files))
	if(any(idxDuplFiles))
		warning(sum(idxDuplFiles), " duplicated file name. These files will have the same name in the 'dataset' column.")

	readFct <- function(file, ...)
		switch(file_ext(file),
			'sas7bdat' = read_sas(file, encoding = encoding, ...),
			'xpt' = read_xpt(file, ...),
			stop(paste("File with extension:", file_ext(file), "currently not supported."))
		)
	
	# import SAS dataset format into R
	dataList <- sapply(names(files), function(name){
				
		if(verbose)
			message("Import ", name, " dataset.")
				
		# read data
		
		data <- as.data.frame(readFct(files[name], ...))
		
		if(nrow(data) > 0){
		
			# save dataset name
			data <- cbind(data, DATASET = name)
			
			if(convertToDate){
				colsDate <- grep(dateVars, colnames(data), value = TRUE)
				data[, colsDate] <- lapply(colsDate, function(col){
					convertToDateTime(data[, col], colName = col)
				})
			}
			
			# column names in lower case for some datasets
			colnames(data) <- toupper(colnames(data))
			
		}else	if(verbose)	warning("Dataset ", name, " is empty.")
		
		data
	}, simplify = FALSE)

	# remove empty dataset(s)
	dataList <- dataList[!sapply(dataList, is.null)]
	
	# extract label variables
	labelVars <- c(getLabelVars(dataList), 'DATASET' = "Dataset Name")
	attr(dataList, "labelVars") <- labelVars
	
	return(dataList)

}

#' Convert character vector to date/time object
#' @param x character vector to convert to date/time
#' @param format string with possible format(s) of the input date/time in the ADaM dataset.
#' If multiple are specified, each format is tested successively, until at least one
#' element in the input vector is converted with the specified format
#' (non missing, following the approach described in
#' the \code{format} parameter of the \code{\link{strptime}} function).
#' See the 'Details' section of the help of the function,
#' for more information about this format.
#' @param colName string with name of column, used in message (if any).
#' @param verbose logical, if TRUE (by default) progress messages are printed during execution
#' @return vector of class \code{\link{POSIXct}}
#' @author Laure Cougnaud
#' @export
convertToDateTime <- function(x, format = c("%Y-%m-%dT%H:%M", "%Y-%m-%d"), 
	colName = NULL, verbose = TRUE){
	
	if(is.character(x)){
		isEmpty <- function(x) is.na(x) | x == ""
	}else{
		isEmpty <- function(x) is.na(x)
	}
	
	newTime <- .POSIXct(rep(NA_real_, length(x))) 
	for(formatI in format){
		idxMissingRecords <- which(is.na(newTime) & !isEmpty(x))
		newTime[idxMissingRecords] <- as.POSIXct(x[idxMissingRecords], format = formatI)
		if(all(!is.na(newTime[!isEmpty(x)])))
			break
	}
	
	if(any(is.na(newTime[!isEmpty(x)]))){
		warning(
			"Vector", if(!is.null(colName)) paste0(": ", colName), 
			" not of specified calendar date format, so is not converted to date/time format.", 
			immediate. = TRUE, call. = FALSE
		)
		newTime <- x
	}else if(verbose)	message("Convert vector", if(!is.null(colName)) paste0(": ", colName), 
		" to calendar date/time format.")
	
	return(newTime)
	
}

#' Get label for a variable of the dataset
#' 
#' The label is extracted either (in this order):
#' \enumerate{
#' \item{if \code{label} is specified: }{from this label based on names,
#' or directly from this label if \code{label} and \code{var} are of length 1 (if available)}
#' \item{if \code{labelVars} is specified: }{from the specified vector of labels,
#' based on names (if available)}
#' \item{if \code{data} is specified: }{from the 'label' attribute 
#' of the corresponding column in \code{data} (if available)}
#' } 
#' If the label is not available, the input variable is returned.
#' @param var Character vector with variables of interest.
#' @param data Data.frame with data.
#' @param labelVars Named character vector with variable labels (names are the variable code),
#' usually extracted from \code{data}.
#' @param label (Named) Character vector with user-specified label for \code{var}.
#' Label is extracted based on names if variable is available.
#' If \code{var} is of length 1, \code{label} can also be specified
#' as an unnamed character.
#' @return Named character vector with label, \code{var} is no label is available
#' @author Laure Cougnaud
#' @examples
#' 
#' data(ADaMDataPelican)
#' labelVarsADaM <- attr(ADaMDataPelican, "labelVars")
#' 
#' # upon reading the data: attributes are directly available in each column
#' 	getLabelVar(data = ADaMDataPelican$ADAE, var = "AREL")
#' 		
#' # but when data is subsetted, label is lost:
#' getLabelVar(data = subset(ADaMDataPelican$ADAE, AESEV == "MODERATE"), var = "AREL")
#' 	
#' # so better to use 'labelVars':
#' getLabelVar(var = "AREL", labelVars = labelVarsADaM)
#' @export
getLabelVar <- function(var, data = NULL, labelVars = NULL, label = NULL){
	
	res <- if(!is.null(var)){
		
		if(is.null(data) & is.null(labelVars) & is.null(label)){
			res <- var
			names(res) <- var
			res
		}else{
			
			var <- unname(var)
			res <- sapply(var, function(x){
				
				attrX <- if(!is.null(label)){
					if(!is.null(names(label)) && x %in% names(label)){
						label[x]
					}else	if(is.null(names(label)) && length(label) == 1 && length(var) == 1){
						label
					}
				}
				if((is.null(attrX) || is.na(attrX)) && !is.null(labelVars)){
					attrX <- labelVars[x]
				}
				
				if(is.null(attrX) || is.na(attrX))
					attrX <- attributes(data[[x]])$label
	
				attrX <- unname(attrX)
				ifelse(is.null(attrX) || is.na(attrX), x, attrX)
				
			})
		}
		
	}
	
	return(res)
}

#' Get label of the variables in SAS dataset(s)
#' @param dataList This parameter is deprecated, use \code{data} instead.
#' @param data Data.frame with SAS dataset(s) or list of those.
#' @param labelVars (optional) Named character vector with additional labels.
#' @return named vector with variable labels
#' @author Laure Cougnaud
#' @importFrom plyr ddply
#' @examples
#' data(ADaMDataPelican)
#' # by default extracted from the data (if available)
#' getLabelVars(ADaMDataPelican)
#' # extracted from specified labelVars, e.g. to specify custom label for specific variable(s)
#' labelVarsCustom <- getLabelVars(
#'   data = ADaMDataPelican, 
#'   labelVars = c(USUBJID = "Subject identifier for my study")
#' )
#' labelVarsCustom["USUBJID"]
#' @export
getLabelVars <- function(data, labelVars = NULL, dataList){
	
	if(!missing(dataList)){
		warning("'dataList' is deprecated, please use: 'data' instead.")
		data <- dataList
	}
	
	if(!missing(data)){
	
		if(!is.data.frame(data)){
			
			labelVarsFromDataList <- lapply(data, getLabelVars)
			labelVarsFromData <- unlist(labelVarsFromDataList)
			names(labelVarsFromData) <- unlist(lapply(labelVarsFromDataList, names))
			
			labelVars <- c(labelVars, labelVarsFromData)
			
			labelVars <- labelVars[!duplicated(names(labelVars))]
			
			return(labelVars)
			
		}
		
		# the variable description are stored in the 'label' attribute of each column
		labelVarsFromData <- unlist(
			sapply(colnames(data), function(col) 
				attributes(data[[col]])$label, 
				simplify = FALSE
			)
		)
		
		labelVars <- c(labelVars, labelVarsFromData)
		labelVars <- labelVars[!duplicated(names(labelVars))]
		
	}
	
	return(labelVars)
	
}


#' Get label for a parameter code (extracted from the 'PARAM' column)
#' @param paramcd Character vector with parameter code(s).
#' @param data Data.frame with data.
#' @param paramcdVar String with column containing the \code{paramcd} parameter,
#' 'PARAMCD' by default (for ADaM format).
#' @param paramVar String with column containing the \code{param} parameter,
#' 'PARAM' by default(for ADaM format).
#' @return Named character vector with label for parameter code or
#' \code{paramcd} if label is missing.
#' @author Laure Cougnaud
#' @examples
#' # for ADaM
#' data(ADaMDataPelican)
#' getLabelParamcd(paramcd = "CRP", data = ADaMDataPelican$ADLB)
#' # for SDTM
#' data(SDTMDataPelican)
#' getLabelParamcd(
#'   paramcd = "CRP", 
#'   data = SDTMDataPelican$LB, 
#'   paramcdVar = "LBTESTCD", 
#'   paramVar = "LBTEST"
#' )
#' @export
getLabelParamcd <- function(paramcd, data, paramcdVar = "PARAMCD", paramVar = "PARAM"){
	
	vars <- c(paramcdVar, paramVar)
	varsNotInData <- vars[which(!vars %in% colnames(data))]
	if(length(varsNotInData) > 0)
		stop(paste("Parameters:", toString(sQuote(varsNotInData)),
			"not available in 'data', you may need to adapt the",
			"parameters: 'paramVar'/'paramcdVar'."))
	
	label <- data[match(paramcd, data[, paramcdVar]), paramVar]
	names(label) <- paramcd
	res <- sapply(names(label), function(xName){
		x <- as.character(label[xName])
		ifelse(is.null(x) || is.na(x), xName, x)
	})
	return(res)
}
