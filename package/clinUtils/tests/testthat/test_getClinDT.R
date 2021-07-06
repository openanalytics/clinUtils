context("Convert to DT wrapper")

# require extra libraries
library(dplyr) # for tibble

library(crosstalk)
library(xml2)
library(jsonlite)

tmpdir <- tempdir()

# utility function to get JSON table from exported HTML file
getTableJSON <- function(file) {
  
	tableHTML <- read_html(file)
	# extract table in JSON format
	tableXMLJSON <- xml_find_all(tableHTML, ".//script[@type='application/json']")
	# extract table as text
	tableXMLJSONTxt <- xml_text(tableXMLJSON)
	# convert table to df
	# by default, converted to a matrix, so lose class
	tableJSON <- jsonlite::fromJSON(txt = tableXMLJSONTxt, simplifyMatrix = FALSE, simplifyDataFrame = TRUE)
	return(tableJSON)
}

# export table and get JSON table
exportAndGetTableJSON <- function(dt) {
  
	file <- file.path(tempdir(), "table.html")
	htmlwidgets::saveWidget(dt, file = file)
	tableJSON <- getTableJSON(file = file)
	unlink(file)
	return(tableJSON)
    
}

test_that("A basic data table is correctly exported", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		stringsAsFactors = FALSE
	)		
		
	expect_silent(dt <- getClinDT(data = data))
	expect_is(dt, "datatables")	
	expect_identical(object = dt$x$data, expected = data)
	
})

test_that("An error is generated if data is of the wrong type", {
	expect_error(getClinDT(data = TRUE))
})

test_that("SharedData data tables are correctly generated", {
	
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		stringsAsFactors = FALSE
	)		
			
	# works
	dataSD <- crosstalk::SharedData$new(data = data, key = as.formula("~USUBJID"))
	expect_silent(dt <- getClinDT(data = dataSD))
	expect_identical(dt$x$data, data)
	
})

test_that("An error is generated if a SharedData with incorrect key is set", {

	expect_error(dt <- getClinDT(crosstalk::SharedData$new(data = data, key = "test")))
	
})

test_that("A data table is correctly generated when input table is a tibble", {
		
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		stringsAsFactors = FALSE
	)		
			
	dataTB <- data %>% group_by(USUBJID)
	expect_silent(dt <- getClinDT(data = dataTB))
	expect_identical(dt$x$data, data)
	
})

test_that("Column names are successfully renamed in data tables", {
	
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		stringsAsFactors = FALSE
	)
	colnames <- c(
		"Subject ID" = "USUBJID",
		"Treatment" = "TRT",
		"Sex" = "SEX"
	)
			
	# correct
	expect_silent(dt <- getClinDT(data = data, colnames = colnames))
	headerTh <- xml_find_all(read_html(dt$x$container), "//th")
	expect_identical(sapply(headerTh, xml_text), names(colnames))
	
})

test_that("A warning is generated if column names are not correctly specified", {
			
	expect_warning(dt <- getClinDT(data = data, colnames = c(TEST = "TEST")))
			
})

test_that("A warning is generated if the old specification for non visible var is used", {
	
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		stringsAsFactors = FALSE
	)
	
	# old spec
	expect_warning(
		dt <- getClinDT(
			data = data, 
			nonVisible = match("TRT", colnames(data))-1
		),
		regex = "deprecated"
	)

})

test_that("Invisible columns are not shown in the data table output", {

	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		stringsAsFactors = FALSE
	)

	expect_silent(
		dt <- getClinDT(
			data = data, 
			nonVisibleVar = "TRT"
		)
	)
	cDefs <- dt$x$options$columnDefs
	cDefsNonVisible <- sapply(cDefs, function(x) isFALSE(x$visible))
	expect_true(any(cDefsNonVisible))
	expect(cDefs[[which(cDefsNonVisible)]]$targets, 1)
	
})

test_that("An error is generated if invisible columns are incorrectly specified", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		stringsAsFactors = FALSE
	)
			
	# in case JS indices not used
	expect_error(
		getClinDT(data = data, nonVisible = ncol(data)),
		"Javascript indexing"
	)
	
})

test_that("Percentages are correctly formatted", {
			
	expect_silent(
		dt <- getClinDT(
			data = data.frame(
				USUBJID = seq.int(5),
				perc = c(0.06, 0.001, 0.65, 0.99, 1)
			), 
			percVar = "perc",
		)
	)
	cDefs <- dt$x$options$columnDefs
	cDefsFmtPercentage <- sapply(cDefs, function(x) "render" %in% names(x) && grepl("formatPercentage", x$render))
	expect_true(any(cDefsFmtPercentage))
	expect(cDefs[[which(cDefsFmtPercentage)]]$targets, 1)		
	
})

test_that("A warning is generated if the variable for barplot is not available", {
	
	data <- data.frame(
		USUBJID = as.character(sample.int(5)),
		AGE = c(20, 40, 67, 36, 50),
		stringsAsFactors = FALSE
	)	

	expect_warning(getClinDT(data = data, barVar = "blabla"))

})
	
test_that("Barplot are correctly rendered within data tables", {			
		
	data <- data.frame(
		USUBJID = as.character(sample.int(5)),
		AGE = c(20, 40, 67, 36, 50),
		stringsAsFactors = FALSE
	)
			
	# specification of variable for the bar
	expect_silent(dt <- getClinDT(data = data, barVar = "AGE"))
	rCB <- dt$x$options$rowCallback
	expect_match(object = rCB, regex = ".*color.*")
	expect_match(object = rCB, regexp = "data[1]", fixed = TRUE)
	
})

test_that("A threshold for the barplot is correctly set", {			

	data <- data.frame(
		USUBJID = as.character(sample.int(5)),
		AGE = c(20, 40, 67, 36, 50),
		stringsAsFactors = FALSE
	)
			
	expect_silent(dt <- getClinDT(data = data, barVar = "AGE", barColorThr = 28))
	rCB <- dt$x$options$rowCallback
	expect_match(object = rCB, regex = ".*data\\[1\\].*28.*")
	
})

test_that("A range for the barplot is correctly set", {			
			
	data <- data.frame(
		USUBJID = as.character(sample.int(5)),
		AGE = c(20, 40, 67, 36, 50),
		stringsAsFactors = FALSE
	)
	
	# specification of range for the bar
	expect_silent(dt <- getClinDT(data = data, barVar = "AGE", barRange = c(0, 100)))
	rCB <- dt$x$options$rowCallback
	expect_match(object = rCB, regex = "100")
	
})

test_that("A range for the barplots of multiple variables is correctly set", {			
				
	data <- data.frame(
		USUBJID = as.character(sample.int(5)),
		AGE = c(20, 40, 67, 36, 50),
		WEIGHTBL = c(60, 45, 89, 120, 78),
		stringsAsFactors = FALSE
	)
			
	# multiple variables
	expect_silent(
		dt <- getClinDT(
			data = data, 
			barVar = c("AGE", "WEIGHTBL"),
			barColorThr = c(AGE = 50),
			barRange = list(AGE = c(0, 100), WEIGHTBL = range(data$WEIGHTBL))
		)
	)
	rCB <- dt$x$options$rowCallback
	expect_match(object = rCB, regex = ".*data\\[1\\].*data\\[2\\]")
	
})

test_that("A warning is generated is a variable for a barplot is not numeric", {			

	expect_warning(dt <- getClinDT(data = data, barVar = "USUBJID"))
	
})

test_that("The location of the filter box is correctly set", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE	
	)
	
	filters <- c("top", "none")
	for(filter in filters){
		
		dt <- getClinDT(data, filter = filter)
		tableFilter <- dt$x$filter
		expect_identical(tableFilter, filter)
		
	}
			
})

test_that("The search box is correctly enabled or disabled in the data table", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE	
	)
			
	for(includeSearchBox in c(TRUE, FALSE)){		
			
		dt <- getClinDT(data, searchBox = includeSearchBox)
		expect_equal(grepl("f", dt$x$options$dom), includeSearchBox)
		
	}
	
})

test_that("The number of records (page length) is correctly set", {

	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE	
	)
			
	dt <- getClinDT(data, pageLength = 2)
	expect_equal(dt$x$options$pageLength, 2) # page length properly set
	expect_true(grepl("p", dt$x$options$dom)) # include pagination control

})

test_that("Fixed columns are correctly specified in the data table", {
	
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE	
	)
			
	fC <- list(leftColumns = 2)
	dt <- getClinDT(data, fixedColumns = fC)
	expect_equal(dt$x$options$fixedColumns, fC)
	
})

test_that("Columns widths are correctly specified in the data table", {	
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE
	)
		
	widths <- c(3, 7)
	dt <- getClinDT(data, columnsWidth = widths)
	
	# extract column defs
	cDefs <- dt$x$options$columnDefs
	cDefs <- unlist(cDefs, recursive = FALSE)
	
	idxColDefs <- which(sapply(cDefs, function(x) "columnsWidth" %in% names(x)))
	cDefs <- cDefs[idxColDefs]
	
	cDefsDf <- do.call(rbind.data.frame, cDefs)
	
	# check if match specified width:
	expect_equal(
		object = cDefsDf[which(cDefsDf$targets == 1), "columnsWidth"],
		expected = 3
	)
	expect_equal(
		object = cDefsDf[which(cDefsDf$targets == 2), "columnsWidth"],
		expected = 7
	)
	
})

test_that("Extra options with default values are correctly overwritten when they are specified", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	# specified options overwrites the default (with message)
	expect_message(
		dt <- getClinDT(data, options = list(pageLength = 20)),
		regexp = "overwrites the default"
	)
	expect_equal(dt$x$options$pageLength, 20) # page length properly set
	
})

test_that("Extra options without default values are correctly set", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE
	)
	
	expect_silent(dt <- getClinDT(data, options = list(lengthChange = 20)))
	expect_equal(dt$x$options[["lengthChange"]], 20)
	
	# Note: wrong 'options' are handled by the JS DataTable library

})

test_that("A single expanded variable is correctly handled in the data table", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE	
	)
			
	# Expand mechanism done in Javascript
	# so check at least than the HTML is well specified
			
	# test in combination of colnames
	expandVar <- sample(colnames(data), ncol(data)/2)
	colnames <- c(
		"Subject ID" = "USUBJID",
		"Treatment" = "TRT"
	)
	
	expect_silent(dt <- getClinDT(data, expandVar = expandVar, colnames = colnames))
	cDefs <- dt$x$options$columnDefs
	
	# target columns are hidden
	idxColsHidden <- which(sapply(cDefs, function(x) "visible" %in% names(x) && !isTRUE(x$visible)))
	expect_length(idxColsHidden, 1)
	expect_setequal(object = idxColsHidden, expected = match(expandVar, colnames(data)))
	
	# and correct variables specified in JS callback
	expandVarLab <- names(colnames)[match(expandVar, colnames)]
	expect_true(all(sapply(expandVarLab, grepl, dt$x$callback, fixed = TRUE)))
	
})

test_that("A warning is generated if a expanded variable is not available", {
		
	expect_warning(dt <- getClinDT(data, expandVar = "blabla"))
			
})


# TODO
test_that("Expand variables with all column options", {
	
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		AGE = c(20, 40, 67, 36, 50),
		stringsAsFactors = FALSE
	)		
			
	dataEscape <- cbind(
		url = sprintf('<a href="customlink" target="_blank">%s</a>', data$USUBJID),
		data,
		stringsAsFactors = FALSE
	)
	
	expandVar <- c("url", colnames(data)[c(1, ncol(data))])
	nonVisibleVar <- colnames(data)[3]
	barVar <- "AGE"
	cAlignLeft <- 3 # 6th column
	
	expect_silent(dt <- 
		getClinDT(
			data = dataEscape, 
			expandVar = expandVar, 
			escape = -1,
			barVar = barVar,
			nonVisibleVar = nonVisibleVar,
			options = list(columnDefs = list(
				list(className = "dt-left", targets = cAlignLeft)			
			))
		)
	)
	tableJSON <- exportAndGetTableJSON(dt)
	cDefs <- tableJSON$x$options$columnDefs
	
	# escape (first column is the '+')
	expect_identical(tableJSON$x$data[[2]], dataEscape[, "url"])
	
	# bar (+1: extra col, -1: JS)
	idxBarVar <- match(barVar, colnames(dataEscape))
	expect_true(grepl("color", tableJSON$x$options$rowCallback))
	
	# non visible vars: expand and non-visible (+1: extra col, -1: JS)
	idxNonVisible <- match(c(nonVisibleVar, expandVar), colnames(dataEscape))
	expect_setequal(
		unlist(subset(cDefs, !visible)$targets),
		expected = idxNonVisible
	)
	
	# extra column defs
	expect_setequal(
		unlist(subset(cDefs, className == "dt-left")$targets),
		expected = cAlignLeft+1 # +1: one extra column added
	)
	
})


test_that("Expand cells", {
		
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		stringsAsFactors = FALSE
	)			
			
	# Expand mechanism done in Javascript
	# so check at least than the HTML is well specified
	
	# wrong spec
	expect_error(dt <- getClinDT(data, expandIdx = list()))
	
	# correct spec
	idxRow <- c(1, 2)
	idxCol <- c(2, 2)
	expandIdx <- cbind(row = idxRow, col = idxCol)
	expect_silent(dt <- getClinDT(data, expandIdx = expandIdx))
	tableJSON <- exportAndGetTableJSON(dt)
	cDefs <- tableJSON$x$options$columnDefs
	
	# target columns are hidden
	expect_silent(cColsHidden <- unlist(subset(cDefs, !visible)$targets))
	expect_setequal(object = cColsHidden, expected = idxCol)
	
	# check that values are correct
	tableJSONData <- tableJSON$x$data
	expandIdxCol <- unique(idxCol)
	expect_identical(tableJSONData[[expandIdxCol]][-idxRow], data[-idxRow, expandIdxCol]) # column displayed
	expect_identical(tableJSONData[[expandIdxCol+1]][idxRow], data[idxRow, expandIdxCol]) # hidden column
	
})

test_that("Escape cells", {
		
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE
	)	
			
	dataEscape <- cbind(
		url = sprintf('<a href="customlink" target="_blank">%s</a>', data$USUBJID),
		data,
		stringsAsFactors = FALSE
	)
	
	# no escape
	expect_silent(dt <- getClinDT(dataEscape))
	tableJSON <- exportAndGetTableJSON(dt)
	expect_false(tableJSON$x$data[[1]][1] == dataEscape[1, "url"])
	
	# as logical
	expect_silent(dt <- getClinDT(dataEscape, escape = FALSE))
	tableJSON <- exportAndGetTableJSON(dt)
	expect_identical(tableJSON$x$data[[1]], dataEscape[, "url"])
	
	expect_error(getClinDT(dataEscape, escape = c(TRUE, FALSE)))
	
	# as negative integer
	expect_silent(dt <- getClinDT(dataEscape, escape = -1))
	tableJSON <- exportAndGetTableJSON(dt)
	expect_identical(tableJSON$x$data[[1]], dataEscape[, "url"])
	
	# wrong spec
	expect_error(dt <- getClinDT(dataEscape, escape = "blabla"))
	expect_error(dt <- getClinDT(dataEscape, escape = ncol(dataEscape) * 2))
	
})

test_that("Row grouping", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		STUDYID = rep("study1", each = 5),
		SITEID = c("a", "b", "c", "a", "c"),
		stringsAsFactors = FALSE
	)	
			
	# Note: done in JS
			
	# properly done 
	rowGroupVars <- c("STUDYID", "SITEID")
	expect_silent(dt <- getClinDT(data, rowGroupVar = rowGroupVars))	
	tableJSON <- exportAndGetTableJSON(dt)
	expect_equal(
		object = tableJSON$x$options$rowGroup$dataSrc, 
		expected = match(rowGroupVars, colnames(data))-1
	)
	
	# variable not available in the data
	expect_warning(
		dt <- getClinDT(data, rowGroupVar = "blabla"),
		"not available in the data."
	)
	
	# old spec
	expect_warning(
		dt <- getClinDT(data, rowGroup = rowGroupVars),
		"deprecated"
	)
	tableJSON <- exportAndGetTableJSON(dt)
	expect_equal(
		object = tableJSON$x$options$rowGroup$dataSrc, 
		expected = match(rowGroupVars, colnames(data))-1
	)
			
})

test_that("Vertical alignment", {
	
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE
	)	
			
	expect_silent(dt <- getClinDT(data, vAlign = "bottom"))
	tableJSON <- exportAndGetTableJSON(dt)
	expect_true(grepl("'vertical-align':'bottom'", tableJSON$x$options$rowCallback))
})

test_that("callback", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE
	)
	
	callback <- JS("testCallback")
	expect_silent(dt <- getClinDT(data, callback = callback))
	expect_true(grepl("testCallback", dt$x$callback))
})

test_that("Buttons", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	# specify buttons
	buttons <- "copy"
	expect_silent(dt <- getClinDT(data, buttons = buttons))
	tableJSON <- exportAndGetTableJSON(dt)
	expect_setequal(tableJSON$x$options$buttons, buttons)
	expect_true(grepl("B", tableJSON$x$options$dom)) # in DOM
	expect_true("Buttons" %in% tableJSON$x$extensions) # Js extension specified
	
	# no buttons
	expect_silent(dt <- getClinDT(data, buttons = NULL))
	tableJSON <- exportAndGetTableJSON(dt)
	expect_length(tableJSON$x$options$buttons, 0)
	expect_false(grepl("B", tableJSON$x$options$dom))
	expect_false("Buttons" %in% tableJSON$x$extensions)
			
	# Note: if uncorrect button specified -> JS error
	
})

test_that("x-scrolling", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	for(scrollX in c(TRUE, FALSE)){		
		
		dt <- getClinDT(data, scrollX = scrollX)
		tableJSON <- exportAndGetTableJSON(dt)
		expect_equal(tableJSON$x$options$scrollX, scrollX)
		
	}
			
})

test_that("Extra datatable parameters", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE
	)
	
	# internal default
	expect_warning(getClinDT(data, rownames = TRUE))
	
	# extra parameter
	width <- "10px"
	expect_silent(dt <- getClinDT(data, width = width))
	expect_equal(dt$width, width)
				
})

test_that("Export", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	# correct file format
	file <- file.path(tmpdir, "test.html"); unlink(file)
	expect_silent(dt <- getClinDT(data, file = file))
	expect_true(file.exists(file))
	unlink(file)
	
	# incorrect file format
	expect_error(
        dt <- getClinDT(data, file = file.path(tmpdir, "test.csv")),
        pattern = "extension"
    )
	
})