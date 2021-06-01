#' Convert a data.frame into interactive table
#' 
#' This function converts a \code{data.frame} from R into 
#' \code{\link[DT]{datatable}} object
#' with sensitive defaults.
#' 
#' The defaults set in this function allow to create specific interactive tables 
#' where columns/cells of interest can be collapsed/expanded via
#' the \code{expandVar}/\code{expandIdx} parameters.
#' 
#' @param data Data.frame, matrix or \code{\link[crosstalk]{SharedData}}
#' object with input data for the table.
#' @param nonVisibleVar Character vector with column(s) in \code{data} to 
#' hide in the output table (column is hidden).
#' @param nonVisible This parameter is deprecated, use the new interface
#' with the \code{nonVisibleVar} parameter. 
#' Numeric vector with column(s)
#' in \code{data} to not display in the output table (column is hidden),
#' in \strong{Javascript unit: first column is 0}, second column is 1, ...
#' @param expandVar Character vector with expandable variables of \code{data}.
#' These columns won't be included in the table, but displayed
#' for each row when the '+' icon in the first column 
#' of the table will be clicked on.
#' @param expandIdx Matrix named with: 'row'/'column'
#' containing row/column indices to expand.
#' @param escape Column(s) to escape in the table
#' (e.g. containing raw HTML code), either character, numeric or logical of length 1.
#' See corresponding parameter in the \code{\link[DT]{datatable}} function.
#' @param percVar Character vector with percentage columns.
#' These columns should contain the percentage from 0 to 1.
#' The content of these colunms will be rounded to 2 digits.
#' @param filter String with position of the filter boxes
#' (\code{filter} parameter of the \code{\link[DT]{datatable}} function), 
#' 'top' by default. Set to 'none' to not included any filtering boxes.
#' @param searchBox Logical, if TRUE (FALSE by default)
#' a general search box is included.
#' @param pageLength Numeric with number of records to include in one page,
#' by default set to 10.
#' Set to Inf to include all records.
#' @param fixedColumns List with fixed columns, see corresponding
#' parameter in the \code{options} parameter of the \code{\link[DT]{datatable}} function.
#' @param rowGroupVar Character vector with colname(s) of \code{data}
#' containing variables to group rows by.
#' This creates row header containing this column.
#' Please note that the original row order in \code{data} is respected,
#' so you might want to order rows based on the grouping variables upfront.
#' @param rowGroup This parameter is deprecated, please use \code{rowGroup} instead.
#' @param options List with additional \code{\link[DT]{datatable}} options.
#' This parameter overwrites the default options set internally
#' in the function (an indicative message mentions it if that is the case).
#' @param columnsWidth Character vector with column width,
#' of length 1 (used for all columns) or of length: \code{ncol(data)}
#' @param vAlign String with vertical alignment for the cells,
#' 'top' by default.
#' @param callback String with custom Javascript callback function.
#' @param buttons DataTable buttons
#' (passed to the 'buttons' element of the \code{options} parameter of \code{\link[DT]{datatable}}).
#' To remove all buttons, set this parameter to NULL.
#' @param scrollX Logical, if TRUE (by default) a horizontal scrolling bar
#' is included. 
#' Note: this differs from the \code{\link[DT]{datatable}} default (FALSE),
#' because required for \code{bookdown::gitbook} output if table is too wide.
#' @param file (optional) String with name of html file to which the
#' created DT should be exported.
#' @param verbose Logical, if TRUE (by default) informative messages
#' are displayed, e.g. if specified \code{options} overwrite the
#' internal default.
#' @param ... Additional parameters for the \code{\link[DT]{datatable}} function,
#' e.g table width.
#' @inheritParams formatDTBarVar
#' @import DT
#' @importFrom htmlwidgets JS saveWidget
#' @importFrom stats na.omit
#' @importFrom crosstalk SharedData
#' @importFrom tools file_ext
#' @example inst/examples/getClinDT-example.R
#' @return \code{\link[DT]{datatable}} object.
#' @author Laure Cougnaud
#' @export
getClinDT <- function(data, 
	nonVisibleVar = NULL, nonVisible = NULL,
	percVar = NULL,
	barVar = NULL,
	barColorThr = NULL,
	barRange = NULL,
	filter = "top",
	searchBox = FALSE,
	pageLength,
	fixedColumns = NULL,
	columnsWidth = NULL,
	options = list(),
	expandVar = NULL, expandIdx = NULL,
	escape = TRUE,
	rowGroup = NULL, rowGroupVar = NULL,
	vAlign = "top",
	callback = NULL,
	buttons = c("copy", "csv", "excel", "pdf", "print"),
	scrollX = TRUE,
	file = NULL,
	verbose = TRUE,
	...){

	#searchColsGroup <- vector("list", length = ncol(tableMHGroup)-1)
	#searchColsGroup[[which(colnames(tableMHGroup) == "% subjects")-1]] <- list(search = "2 ... Inf")
	
	# extract arguments passed to the 'options'
	extraArgs <- list(...)
	
	# extract df is input is 'SharedData'
	isSharedData <- inherits(x = data, what = "SharedData")
	dataContent <- if(isSharedData){
		data$origData()
	}else	data

	# convert tibble to data.frame
	if(inherits(dataContent, "tbl_df")){
		dataContent <- as.data.frame(dataContent)
	}
	
	if(!inherits(dataContent, c("data.frame", "matrix")))
		stop("'data' should be a data.frame, a matrix, a tibble or a SharedData object.")
	
	# adjust colnames to only contains variables in data, otherwise issue:
	# Error in convertIdx(colnames, cn) : Some column names in the 'escape' argument not found in data
	colnames <- extraArgs$colnames
	if(!is.null(colnames)){
		colnames <- colnames[colnames %in% colnames(dataContent)]
		if(length(colnames) == 0){
			colnames <- NULL
			warning("'colnames' doesn't contain labels for columns in data. ",
				"Are you sure you have specified it correctly (c([newName] = [oldName], ...)?")
		}
		extraArgs$colnames <- colnames
	}

	# non-visible columns
	if(!is.null(nonVisible))
		warning("'nonVisible' is deprecated, please use: 'nonVisibleVar' instead.")		
	nonVisibleVar <- checkVarInData(var = nonVisibleVar, data = dataContent, label = "non-visible")
	if(!is.null(nonVisibleVar)){
		if(!is.null(nonVisible))
			warning("'nonVisible' or 'nonVisibleVar' should be specified, 'nonVisibleVar' is used")
		nonVisible <- match(nonVisibleVar, colnames(dataContent))-1
	}
	
	if(missing(pageLength)){
		pageLength <- ifelse(nrow(dataContent) <= 10, Inf, 10)
	}
	
	# row grouping
	if(!is.null(rowGroup)){
		warning("'rowGroup' is deprecated, please use: 'rowGroupVar' instead.")		
		rowGroupVar <- rowGroup
	}
	rowGroupVar <- checkVarInData(var = rowGroupVar, data = dataContent, label = "row group")
	if(!is.null(rowGroupVar)){
		rowGroup <- match(rowGroupVar, colnames(dataContent))-1
		if(length(rowGroup) == 0)	rowGroup <- NULL
	}else	rowGroup <- NULL
	
	if(is.logical(escape)){
		if(length(escape) != 1){
			stop("If escape is logical, it should be of length 1.")
		}else{
			if(escape){
				escape <- seq(from = 1, to = ncol(dataContent))
			}else{
				escape <- numeric()
			}
		}
	}else if(is.numeric(escape)){
		
		idxEscNotInData <- escape[!abs(escape) %in% seq_len(ncol(dataContent))]
		if(length(idxEscNotInData) > 0){
			stop("'Escape' contains columns not in data: ", toString(idxEscNotInData), ".")
		}
		
		if(any(escape < 0)){
			if(!all(escape < 0))
				stop("If 'escape' contains negative elements, they should all be negative.")
			escape <- setdiff(seq(from = 1, to = ncol(dataContent)), -escape)
		}
	}
	
	if(!is.null(rowGroup))
		nonVisible <- union(nonVisible, rowGroup)	
	
	## expand variables
	
	idxControl <- NULL
	expandVar <- checkVarInData(var = expandVar, data = dataContent, label = "expandable")
	isExpandIdxWrong <- 
		!is.null(expandIdx) && (
			(!is.matrix(expandIdx)) ||
			!all(c("row", "col") %in% colnames(expandIdx))
		)
	if(isExpandIdxWrong){
		stop("'expandIdx' should be a matrix with columns: ",
			"'row' and 'col'.")
	} 
	
	if(!is.null(expandVar) | !is.null(expandIdx)){
		
		if(!is.null(expandIdx)){
			
			idxExpandVar <- unique(expandIdx[, "col"])
			
			# for each column to expand
			for(iCol in seq_along(idxExpandVar)){
				
				# extract column index
				idxCol <- idxExpandVar[iCol]
				# a) extract new column index (in case previous columns have been added)
				idxColNew <- idxCol + iCol - 1
			
				# only consider indices for specified iCol
				expandIdxCol <- expandIdx[which(expandIdx[, "col"] %in% idxCol), , drop = FALSE]
				expandIdxCol[, "col"] <- idxColNew # change column index 
				
				# save values to expand in a new column
				expandRow <- rep(NA_character_, nrow(dataContent))
				expandRow[expandIdxCol[, "row"]] <- dataContent[expandIdxCol]
				
				# add a '+' in cells to be expanded
				dataContent[expandIdxCol] <- '&oplus;'
				
				# add the column with variables to expand after the specified column
				idxBefore <- seq_len(idxColNew)
				idxAfter <- setdiff(seq_len(ncol(dataContent)), idxBefore)
				dataContent <- cbind(
					dataContent[, idxBefore, drop = FALSE], 
					expandRow = expandRow, 
					dataContent[, idxAfter, drop = FALSE]
				)
				
			}
				
			# column -> column + 1 for each column added 
			newIdxForExpandVar <- idxExpandVar + seq_along(idxExpandVar)-1
			getCol <- function(x){x}		
			body(getCol) <- bquote({
				xNew <- sapply(x, function(xI){
					idxDiff <- xI-.(idxExpandVar)
					idxDiff <- idxDiff[idxDiff > 0]
					ifelse(length(idxDiff) > 0, xI + which.min(idxDiff), xI)
				})
		        return(xNew)
			})
			# formatStyle only works with index of column without addition!
			getColFormatStyle <- function(x){x}	
			body(getColFormatStyle) <- bquote(.(getCol)(x)-1)
			
			# column with: '+'
			idxControl <- getCol(idxExpandVar)-1 # JS notation (start at 0)
			escapeExpand <- getCol(idxExpandVar) # R notation (start at 1)
			
			# column with hidden content
			nonVisibleExpand <- getCol(idxExpandVar) # JS notation (start at 0): col + 1 -1
			
			expandJS <- paste0("' + d[iCol + 1]+ '") # JS notation (start at 0): iCol + 1 - 1
			
			callback <- JS(
				paste0("
					table.column(1).nodes().to$().css({cursor: 'pointer'});
					var format = function(d, iCol) {
						return '<div>",
						expandJS,
						"</div>';
					};
					table.on('click', 'td.details-control', function() {
						var td = $(this), row = table.row(td.closest('tr')), iCol = td[0]._DT_CellIndex['column'];
						if (row.child.isShown()) {
							row.child.hide();
							td.html('&oplus;');
						} else {
							oldVal = format(row.data(), iCol-1);
							if(oldVal === '<div>&oplus;</div>'){
								row.child(format(row.data(), iCol)).show();
								td.html('&CircleMinus;');
							}
						}
					});"
				),
				callback
			)
			
		}else if(!is.null(expandVar)){
			
			idxExpandVar <- which(colnames(dataContent) %in% expandVar)
			
			# column -> column + 1 (because one column added) 
			getCol <- function(x)	return(x+1)
			# formatStyle only works with index of column without addition!
			getColFormatStyle <- function(x)	return(x)
			
			# add '+' column
			dataContent <- cbind(' ' = '&oplus;', dataContent)
			
			# column with: '+'
			idxControl <- 0 # R notation (start at 1)
			escapeExpand <- 1 # JS notation (start at 0)
			
			# column(s) with hidden content
			nonVisibleExpand <- idxExpandVar # JS notation (start at 0)
			
			# build JS containing concatenation of columns
			expandJS <- paste(sapply(idxExpandVar, function(i){
				# JS notation (start at 0)
				# idxInit + 1 (new column) - 1 (JS notation)
				labelI <- colnames(dataContent)[getCol(i)] 
				if(!is.null(colnames)){
					labelCNI <- names(colnames)[match(labelI, colnames)]
					if(!is.na(labelCNI))	labelI <- labelCNI
				}
				paste0(labelI, ": ' + d[", i, "] + '")		
			}), collapse = "<br>")

			callback <- JS(
				paste0("
					table.column(1).nodes().to$().css({cursor: 'pointer'});
					var format = function(d) {
						return '<div>",
						expandJS,
						"</div>';
					};
					table.on('click', 'td.details-control', function() {
						var td = $(this), row = table.row(td.closest('tr'));
						if (row.child.isShown()) {
							row.child.hide();
							td.html('&oplus;');
						} else {
							row.child(format(row.data())).show();
							td.html('&CircleMinus;');
						}
					});"
				),
				callback
			)
			
		}
		
		escape <- setdiff(getCol(escape), escapeExpand)
		nonVisible <- union(getCol(nonVisible), nonVisibleExpand)
		
	}else{
		
		getColFormatStyle <- getCol <- function(x)	return(x)
		callback <- callback

	}
	
	if(any(nonVisible >= ncol(dataContent)))
		stop(paste(
			"'nonVisible' should contain indices of columns within data (< ncol(data)).",
			"Are you sure you are using Javascript indexing",
			"(0 for first column, 1 for second column and so on)?"
		))
	
	# specify properties for specific columns
	if(!is.null(options$columnDefs)){
		options$columnDefs <- sapply(options$columnDefs, function(x){
			if(is.list(x) && "targets" %in% names(x)){
				x[["targets"]] <- getCol(x[["targets"]])
			}
			x		
		}, simplify = FALSE)
	}
	columnDefs <- c(
		options$columnDefs,
		if(!is.null(columnsWidth)){
			list({
				columnsWidths <- rep(columnsWidth, length.out = ncol(dataContent))
				lapply(seq_along(columnsWidths), function(i)
					list(targets = getCol(i), columnsWidth = columnsWidths[i])
				)
			})
		},
		if(!is.null(nonVisible))	
			list(list(targets = nonVisible, visible = FALSE)),
		if(!is.null(idxControl))
			columnDefs <- list(list(orderable = FALSE, className = 'details-control', targets = idxControl))
	)
	
	## set options for the table
	# if specified by the user, they overwrite the default
	
	# check if options not already set
	isOptionAvailable <- function(options, label){
		isOptionAvailable <- !label %in% names(options)
		if(!isOptionAvailable & verbose){
			message("The", sQuote(label), " specified in 'options' overwrites the default.")
		}
		return(isOptionAvailable)
	}
	
	if(isOptionAvailable(options, "dom")){
		domDefault <- paste0(
			# Buttons for export of the table
			if(length(buttons) > 0)	'B', 
			# l: length changing input control
			if(pageLength < Inf) 	"l",
			# f: filtering input
			if(searchBox)	'f', 
			# r: processing display element
			# t: table
			'rt', 
			# i: table information summary
			# p: pagination control
			if(pageLength < Inf)	"ip"
		)
		options[["dom"]] <- domDefault
	}
	
	
	if(!is.null(fixedColumns)){
		
		idx <- which(names(fixedColumns) %in% c("leftColumns", "rightColumns"))
		if(length(idx) > 0)
			fixedColumns[idx] <- sapply(fixedColumns[idx], getCol, simplify = FALSE)
		
		if(isOptionAvailable(options, "fixedColumns")){
			options[["fixedColumns"]] <- fixedColumns
		}
	}
	
	if(isOptionAvailable(options, "fixedHeader")){
		options[["fixedHeader"]] <- if(is.null(fixedColumns)) TRUE else FALSE
	}
	
	if(isOptionAvailable(options, "buttons")){
		options[["buttons"]] <- buttons
	}
	
	if(isOptionAvailable(options, "searching")){
		options[["searching"]] <- TRUE
	}
	
	if(isOptionAvailable(options, "scrollX")){
		options[["scrollX"]] <- scrollX
	}
	
	if(isOptionAvailable(options, "autoWidth")){
		options[["autoWidth"]] <- (!is.null(columnsWidth))
	}
	
	if(isOptionAvailable(options, "pageLength")){
		options[["pageLength"]] <- ifelse(pageLength == Inf, nrow(dataContent), pageLength)
	}
	
	if(length(rowGroup) > 0 && isOptionAvailable(options, "rowGroup")){
		options[["rowGroup"]] <- list(dataSrc = getCol(rowGroup))
	}
	
	# columnsDefs are combined with input options (if specified)
	if(length(columnDefs) > 0){
		options[["columnDefs"]] <- columnDefs
	}
	if(length(options) == 0)	options <- NULL

	## JS extensions
	extensions <- c(
		# if table too narrow: certain columns automatically collapsed and hidden
		#Note: 'Responsive' and 'FixedColumns' extensions are not compatible
#		"Responsive", 
		if(!is.null(rowGroup))	"RowGroup", 
		if(length(buttons) > 0)	"Buttons", 
		if(!is.null(fixedColumns))	c("FixedColumns", "Scroller"),
		if(is.null(fixedColumns)) "FixedHeader"
	)
	
	# extract data for datatable
	dataDT <- if(isSharedData){
		if(nrow(dataContent) != length(data$key()))
			stop("Key vector is of different length than the number of records in the data.")
		# a public key() method exists, but returns extracted key variable
		# can leads to the error: 'Unknown key type' if key is a factor
		# private key() method returns user-provided 'key' parameter
		keySD <- data$.__enclos_env__$private$.key
		SharedData$new(data = dataContent, key = keySD, group = data$groupName())
	}else	dataContent

	## create DataTable
	
	# extract input parameters
	argsDT <- list(
		data = dataDT,
		# if rownames = FALSE, indices are col indices - 1 (e.g. escape)
		rownames = FALSE,
		filter = filter,
		# Add buttons to export the table
		extensions = extensions,
		options = options,
		escape = escape
	)
	if(!is.null(callback))	argsDT  <- c(argsDT, list(callback = callback))
	
	extraArgsSpec <- intersect(names(extraArgs), names(argsDT))
	if(length(extraArgsSpec) > 0){
		warning(paste("Extra parameter(s)", toString(sQuote(extraArgsSpec)),
			"are ignored because some internal defaults are set for these parameters."
		))
		extraArgs <- extraArgs[setdiff(names(extraArgs), extraArgsSpec)]
	}
	argsDT <- c(argsDT, extraArgs)
	
	# create datatable
	tableDT <- do.call(datatable, argsDT)
	
	## specify custom formatting functions for the columns
	
	if(!is.null(percVar))
		tableDT <- DT::formatPercentage(tableDT, columns = percVar, digits = 2)
	
	# add bar variable(s)
	tableDT <- formatDTBarVar(
		tableDT = tableDT,
		data = dataContent,
		barVar = barVar,
		barColorThr = barColorThr,
		barRange = barRange,
		getCol = getColFormatStyle
	)
	
	if(!is.null(vAlign)){
		tableDT <- tableDT %>% formatStyle(
			columns = seq_len(ncol(dataContent)),
			'vertical-align' = vAlign
		)
		
	}
	
	if(!is.null(file)){
		
		if(file_ext(file) != "html")
			stop("'file' should be of extension 'html'.")
		
		# 'saveWidget' only save in current directory, so move to specified directory
		wdInit <- getwd();on.exit(setwd(wdInit))
		setwd(dirname(file))
		htmlwidgets::saveWidget(widget = tableDT, file = basename(file))
		
	}

	return(tableDT)

}

#' Format a variable in a \code{\link[DT]{datatable}}
#' as a barplot.
#' @param tableDT \code{\link[DT]{datatable}} object
#' @param data Data.frame with content of \code{tableDT}.
#' @param barVar Character vector with numeric variable of \code{data}
#' which should be represented as bar in the table.
#' @param barRange (optional) range for the bars, either:
#' \itemize{
#' \item{a numeric vector of length 2, same range for all bars}
#' \item{list with range for each bar, named with the variable
#' in \code{barVar}}
#' }
#' If not specified, the range of each \code{barVar} variable
#' in \code{data} is used.
#' @param barColorThr Numeric vector with threshold to 
#' consider to color the bar, either:
#' \itemize{
#' \item{a numeric vector of length 1, same threshold for all bars}
#' \item{named vector with threshold for each bar, named with the variable
#' in \code{barVar}}
#' }
#' @param getCol Function, which for an index of a column
#' in \code{data} returns the index of the column to be passed to
#' \code{\link[DT]{formatStyle}}
#' @return Updated \code{tableDT}
#' @import DT
#' @importFrom viridisLite viridis
#' @author Laure Cougnaud
formatDTBarVar <- function(
	tableDT,
	data,
	barVar = NULL,
	barColorThr = NULL,
	barRange = NULL,
	getCol = function(x) x){
	
	# highlight records which occur in more than [] percentage
	
	# check if specified var as in the data
	barVar <- checkVarInData(var = barVar, data = data, label = "bar")
	
	if(!is.null(barVar)){
		
		# check if barVar is numeric
		barVarNotNum <- barVar[!sapply(data[, barVar, drop = FALSE], is.numeric)]
		if(length(barVarNotNum) > 0){
			warning(paste(toString(barVarNotNum), "variable(s)",
				"not represented as bar because they are not numeric.")
			)
			barVar <- setdiff(barVar, barVarNotNum)
		}
		
		# custom fct to extract value for spec var in parameter
		getElFromList <- function(param, var){
			if(!is.null(param)){
				if(!is.null(names(param))){
					if(var %in% names(param)){
						param[[var]]
					}
				}else	param
			}
		}
		
		for(var in barVar){
			
			# use index instead of the column names
			# (if new colnames are specified, should use new column names)
			idxVar <- getCol(match(var, colnames(data)))
			
			# extract threshold to specify intervals
			barColorThrVar <- getElFromList(param = barColorThr, var = var)
			
			# extract range to set the color
			barRangeVar <- getElFromList(param = barRange, var = var)
			if(is.null(barRangeVar))	barRangeVar <- range(as.numeric(data[, var]), na.rm = TRUE)
			# 'styleColorBar' create empty bar for minimum value
			# so set a lower value for the minimum: min - 1% of the range
			barRangeVar[1] <- barRangeVar[1] - diff(barRangeVar)*0.01
			
			# add bar
			barColor <- if(!is.null(barColorThrVar)){
				styleInterval(
					cuts = barColorThrVar, 
					values = viridis(length(barColorThrVar)+1)
				)
			}else	"black"
			barBg <- styleColorBar(
				data = barRangeVar, 
				color = "green"
			)
			tableDT <- tableDT %>% 
				formatStyle(
					columns = idxVar, 
					color = barColor,
					background = barBg
				)
		}
	}
	
	return(tableDT)
	
}

#' Check if specified variables are in the data.
#' If they are not, they are removed from specified
#' variables and a message is printed.
#' @param var Character vector with variables.
#' @param data Data.frame with data.
#' @param label String with label used in message.
#' @return \code{var} present in \code{data},
#' NULL if empty
checkVarInData <- function(var, data, label){
	
	# check if specified var as in the data
	varNotInData <- setdiff(var, colnames(data))
	if(length(varNotInData) > 0)
		warning(paste(label, "variable(s):",
			sQuote(toString(varNotInData)),
			"not used because not available in the data."), 
		call. = FALSE)
	var <- intersect(var, colnames(data))
	if(length(var) == 0) var <- NULL
	
	return(var)
	
}


