#' Load data from ADaM/SDTM file(s).
#' 
#' Load data set in SAS format ('sas7bdat' or 'xpt') into R data.frames, from
#' \code{files} or raw vector \code{data}.
#' 
#' While creating the R data.frames, if date/time variables are present, 
#' those are converted into to R date/time class 
#' (see \code{\link{convertToDateTime}}) function.
#' 
#' The labels of the ADaM/SDTM data sets are attached as attributes 
#' of the R data.frame.
#' @param files Character vector with path to ADaM or SDTM file(s).\cr
#' Currently only import of files with extension: 'sas7bdat' or 'xpt' are supported.
#' @param data Named list with \link{raw} vector data 
#' (as supported by: \link[haven]{read_sas} and \link[haven]{read_xpt}).\cr
#' The list should be named with the file name (or full path)
#' the data has been imported from (e.g.: 'ae.xpt' or '/path/to/data.adsl.sas7bdat').
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
#' @return List of data.frame with data of each ADAM file (if not empty),
#' with special attributes:
#' \itemize{
#'  \item 'labelVars': named vector with label of the variables
#'  \item 'label': named vector with label of the datasets
#' }
#' Each data.frame contains an additional column called 'dataset' 
#' specifying the name of the \code{files} it was read from.
#' @author Laure Cougnaud
#' @importFrom tools file_path_sans_ext
#' @importFrom haven read_sas read_xpt
#' @importFrom plyr colwise
#' @importFrom tools file_ext
#' @examples
#' \dontrun{
#' dataFromSAS7bdat <- loadDataADaMSDTM(files = "ae.sas7bdat")
#' attr(dataFromSAS7bdat, "labelVars") # column labels
#' dataFromXpt <- loadDataADaMSDTM(files = c("ae.xpt", "dm.xpt"))
#' attr(dataFromXpt, "labelVars") # column labels
#' }
#' @export
loadDataADaMSDTM <- function(
  files, data,
	convertToDate = FALSE, dateVars = "DTC$",
	verbose = TRUE, 
	encoding = "UTF-8", 
  ...){
  
  
  isData <- (!missing(data))
  if(isData){
    
    if(!is.list(data) || (is.list(data) && is.null(names(data))))
      stop("'data' should be a named list.")
    
  }else{
  
    if(!is.character(files))
      stop("'files' should be a character vector")
    
  }

  # utility function to import data from xpt or sas7bdat file
	readFct <- function(file, input = file, ...){
  	switch(
  	  tools::file_ext(file),
  		'sas7bdat' = haven::read_sas(data_file = input, encoding = encoding, ...),
  		'xpt' = haven::read_xpt(file = input, ...),
  		stop(paste("File with extension:", file_ext(file), "currently not supported."))
  	)
	}
	
	getDataset <- function(file)
	  return(toupper(tools::file_path_sans_ext(basename(file))))
	
	# import SAS dataset format into R
	files <- if(isData){names(data)}else{files}
	dataList <- sapply(files, function(file){
	  
	  # extract dataset name
	  dataset <- getDataset(file = file)
				
		if(verbose)
		  message("Import ", dataset, " dataset.")
				
		# read data
		data <- readFct(
		  file = file, 
		  input = if(isData){data[[file]]}else{file}, 
		  ...
		)
		data <- as.data.frame(data)
		
		if(nrow(data) > 0){
		  
		  # extract label (not retained by 'cbind')
		  label <- attr(data, "label")
		
			# save dataset name
			data <- cbind(data, DATASET = dataset)
			
			if(convertToDate){
				colsDate <- grep(dateVars, colnames(data), value = TRUE)
				data[, colsDate] <- lapply(colsDate, function(col){
					convertToDateTime(data[, col], colName = col)
				})
			}
			
			# column names in lower case for some datasets
			colnames(data) <- toupper(colnames(data))
			
			# save the label
			attr(data, "label") <- label
			
		}else	if(verbose)	warning("Dataset ", dataset, " is empty.")
		
		return(data)
	}, simplify = FALSE)

	# remove empty dataset(s)
	dataList <- dataList[!sapply(dataList, is.null)]
	
	# set names as the dataset name:
	datasets <- getDataset(file = names(dataList))
	
	# check if there is no duplicated names
	idxDuplFiles <- duplicated(datasets)
	if(any(idxDuplFiles))
	  warning(sum(idxDuplFiles), " duplicated file name. ",
	    "These files will have the same name in the 'dataset' column.")
	
	# set the names of the list to the datasets
	names(dataList) <- datasets
	
	# extract label variables
	labelVars <- c(getLabelVars(dataList), 'DATASET' = "Dataset Name")
	attr(dataList, "labelVars") <- labelVars
	
	# extract dataset label
	labels <- lapply(dataList, attr, "label")
	labels <- labels[!sapply(labels, is.null)]
	attr(dataList, "label") <- unlist(labels)
	
	return(dataList)

}

#' Convert character vector to date/time object
#' 
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
#' @return Vector of class \code{\link{POSIXct}}
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
#' \item if \code{label} is specified: from this label based on names,
#' or directly from this label if \code{label} and \code{var} are of length 1 
#' (if available)
#' \item if \code{labelVars} is specified: from the specified vector of labels,
#' based on names (if available)
#' \item if \code{data} is specified: from the 'label' attribute 
#' of the corresponding column in \code{data} (if available)
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
#' data(dataADaMCDISCP01)
#' labelVars <- attr(dataADaMCDISCP01, "labelVars")
#' 
#' # (upon reading the data with haven: attributes should directly available in each column)
#' getLabelVar(data = dataADaMCDISCP01, var = "AEREL")
#' 		
#' # but if the data as data.frame is subsetted, label is lost 	
#' # so better to use 'labelVars':
#' getLabelVar(var = "AEREL", labelVars = labelVars)
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
#' 
#' @param data Data.frame with SAS dataset(s) or list of those.
#' @param labelVars (optional) Named character vector with additional labels.
#' @return Named vector with variable labels.
#' @author Laure Cougnaud
#' @importFrom plyr ddply
#' @examples
#' data(dataADaMCDISCP01)
#' labelVars <- attr(dataADaMCDISCP01, "labelVars")
#' 
#' # extract label for all variables from specified datasets:
#' getLabelVars(data = dataADaMCDISCP01[c("ADLBC", "ADVS")], labelVars = labelVars)
#' 
#' # extracted from specified labelVars, e.g. to specify custom label for specific variable(s)
#' labelVarsCustom <- getLabelVars(
#'   data = dataADaMCDISCP01, 
#'   labelVars = c(USUBJID = "Subject identifier for my study")
#' )
#' labelVarsCustom["USUBJID"]
#' @export
getLabelVars <- function(data, labelVars = NULL) {

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


#' Get label for a parameter code
#' 
#' This function gets the labele for a parameter code 
#' extracted from the 'PARAM' column.
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
#' data(dataADaMCDISCP01)
#' getLabelParamcd(paramcd = "CHOL", data = dataADaMCDISCP01$ADLBC)
#' # for SDTM
#' data(dataSDTMCDISCP01)
#' getLabelParamcd(
#'   paramcd = "ALB", 
#'   data = dataSDTMCDISCP01$LB, 
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
