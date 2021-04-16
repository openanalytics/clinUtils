context("Convert to DT GLPG wrapper")

# require extra libraries
library(dplyr) # for tibble

library(htmlwidgets)
library(crosstalk)
library(xml2)
library(jsonlite)
library(webshot)

# utility function to get JSON table from exported HTML file
getTableJSON <- function(file){
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
exportAndGetTableJSON <- function(dt){
	file <- "table.html"
	htmlwidgets::saveWidget(dt, file = file)
	tableJSON <- getTableJSON(file = file)
	unlink(file)
	return(tableJSON)
}

exportDTToPng <- function(dt, label){
	file <- paste0("table-", label, ".html")
	filePng <- sub("html", "png", file)
	htmlwidgets::saveWidget(dt, file = file) # export to html file
	webshot::webshot(url = file, file = filePng, debug = TRUE) # screenshot of html file
#	unlink(file)
	return(filePng)
}

test_that("basic", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		stringsAsFactors = FALSE
	)		
		
	expect_silent(dt <- getClinDT(data = data))
	expect_is(dt, "datatables")	
	expect_identical(object = dt$x$data, expected = data)
	
	# check than output contains all columns/rows in correct order
	file <- "table-basic.html"
	htmlwidgets::saveWidget(dt, file = file)
	
	tableData <- getTableJSON(file)$x$data
	tableData <- do.call(data.frame, c(tableData, list(stringsAsFactors = FALSE)))
	expect_true(all(tableData == data))
	# Note: difference in mode (e.g. Date <-> character), and in converted characters (e.g. <  -> &lt;) can be reported
	
	unlink(file)
	
	# wrong type
	expect_error(getClinDT(data = TRUE))
	
})

test_that("SharedData", {
	
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
	
	# incorrect
	expect_error(dt <- getClinDT(crosstalk::SharedData$new(data = data, key = "test")))
	
})

test_that("tibble", {
		
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

test_that("Colnames", {
	
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
	tableJSON <- exportAndGetTableJSON(dt)
	headerTh <- xml_find_all(read_html(tableJSON$x$container), "//th")
	expect_identical(sapply(headerTh, xml_text), names(colnames))
	
	# wrong
	expect_warning(dt <- getClinDT(data = data, colnames = c(TEST = "TEST")))
			
})

test_that("Specify non visible columns", {
	
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		stringsAsFactors = FALSE
	)		
			
	# compare files created with the 'nonVisible' and with removing column upfront
	nonVisibleVar <- sample(colnames(data), 1, replace = TRUE)
	
	# remove var (for comparison)
	expect_silent(
		dt <- getClinDT(data = data[, setdiff(colnames(data), nonVisibleVar)])
	)
	filePngRemoveCol <- exportDTToPng(dt = dt, label = "removeCol")
	
	# old spec
	expect_warning(
		dt <- getClinDT(
			data = data, 
			nonVisible = match(nonVisibleVar, colnames(data))-1
		),
		regex = "deprecated"
	)
	filePngNonVisible <- exportDTToPng(dt = dt, label = "nonVisible")
	expect_message(
		compareFigOne(filePngNonVisible, filePngRemoveCol),
		"Comparing: .* ok!"
	)

	# new spec
	expect_silent(
		dt <- getClinDT(
			data = data, 
			nonVisibleVar = nonVisibleVar
		)
	)
	filePngNonVisibleVar <- exportDTToPng(dt = dt, label = "nonVisibleVar")
	expect_message(
		compareFigOne(filePngNonVisibleVar, filePngRemoveCol),
		"Comparing: .* ok!"
	)
	
	# clean
	unlink(c(filePngRemoveCol, filePngNonVisible, filePngNonVisibleVar, "compareFigs"), recursive = TRUE)
	
	# in case JS indices not used
	expect_error(getClinDT(data = data, nonVisible = ncol(data)))
	
})

test_that("Format percentage var", {
			
	# Compare screenshot of table created with 'percVar' and 
	# and table created from manually formatted percentage variable
	getClinDTWithAlign <- function(...)
		getClinDT(..., options = list(columnDefs = list(list(className = 'dt-center', targets="_all"))))
			
	perc <- c(0.06, 0.001, 0.65, 0.99, 1)
	expect_silent(dt <- getClinDTWithAlign(data = data.frame(perc = perc), percVar = "perc"))
	filePercVar <- exportDTToPng(dt, label = "percentage-percVar")
	
	percST <- paste0(formatC(round(perc*100, 2), digits = 2, format = "f", flag = "0"), "%")
	expect_silent(dt <- getClinDTWithAlign(data = data.frame(perc = percST)))
	fileManFormat <- exportDTToPng(dt, label = "percentage-manFormat")
	
	expect_message(compareFigOne(filePercVar, fileManFormat), "ok!", label = "percVar")	
	
	unlink(c(filePercVar, fileManFormat))
			
})

test_that("Barplot for a variable", {
	
	data <- data.frame(
		USUBJID = as.character(sample.int(5)),
		TRT = c("A", "A", "B", "B", "B"),
		SEX = c("F", "M", "F", "M", "F"),
		AGE = c(20, 40, 67, 36, 50),
		WEIGHTBL = c(60, 45, 89, 120, 78),
		stringsAsFactors = FALSE
	)		
			
	# variable not available
	expect_warning(getClinDT(data = data, barVar = "blabla"))

	# Note: coloring done in JS, not available in HTML file
	# so check if input parameters have an effect on the created image
	
	dt <- getClinDT(data = data)
	file <- exportDTToPng(dt, label = "basic")
			
	# specification of barVar
	expect_silent(dt <- getClinDT(data = data, barVar = "AGE"))
	fileBarVar <- exportDTToPng(dt, label = "bar-barVar")
	expect_message(compareFigOne(file, fileBarVar), "DIFFERENT!", label = "barVar")
	
	# specification of barColorThr
	expect_silent(dt <- getClinDT(data = data, barVar = "AGE", barColorThr = 28))
	fileBarColorThr <- exportDTToPng(dt, label = "bar-barColorThr")
	expect_message(compareFigOne(fileBarColorThr, fileBarVar), "DIFFERENT!", label = "barColorThr")	
	
	# specification of barRange
	expect_silent(dt <- getClinDT(data = data, barVar = "AGE", barRange = c(0, 100)))
	fileBarColorRange <- exportDTToPng(dt, label = "bar-barRange")
	expect_message(compareFigOne(fileBarColorRange, fileBarVar), "DIFFERENT!", label = "barRange")	
			
	# multiple variables
	expect_silent(
		dt <- getClinDT(
			data = data, 
			barVar = c("AGE", "WEIGHTBL"),
			barColorThr = c(AGE = 50),
			barRange = list(AGE = c(0, 100), WEIGHTBL = range(data$WEIGHTBL))
		)
	)
	fileBarMultiple <- exportDTToPng(dt, label = "bar-multiple")
	expect_message(compareFigOne(fileBarMultiple, fileBarVar), "DIFFERENT!", label = "multiple bar variables")	
	
	# wrong types
	expect_warning(dt <- getClinDT(data = data, barVar = "USUBJID"))
			
	unlink(c(file, fileBarVar, fileBarColorThr, fileBarColorRange, fileBarMultiple, "compareFigs"), recursive = TRUE)
	
})

test_that("Filter boxes", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE	
	)
	
	filters <- c("top", "none")
	for(filter in filters){
		
		dt <- getClinDT(data, filter = filter)
		tableJSON <- exportAndGetTableJSON(dt)
		tableFilter <- tableJSON$x$filter
		expect_identical(tableFilter, filter)
		
	}
			
})

test_that("Search box", {
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE	
	)
			
	for(includeSearchBox in c(TRUE, FALSE)){		
			
		dt <- getClinDT(data, searchBox = includeSearchBox)
		tableJSON <- exportAndGetTableJSON(dt)
		expect_equal(grepl("f", tableJSON$x$options$dom), includeSearchBox)
		
	}
	
})

test_that("Page length", {

	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE	
	)
			
	dt <- getClinDT(data, pageLength = 2)
	tableJSON <- exportAndGetTableJSON(dt)
	expect_equal(tableJSON$x$options$pageLength, 2) # page length properly set
	expect_true(grepl("p", tableJSON$x$options$dom)) # include pagination control
})

test_that("Fixed columns", {
	
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE	
	)
			
	fC <- list(leftColumns = 2)
	dt <- getClinDT(data, fixedColumns = fC)
	tableJSON <- exportAndGetTableJSON(dt)
	expect_equal(tableJSON$x$options$fixedColumns, fC)
})

test_that("Columns widths", {	
			
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "A", "B", "B", "B"),
		stringsAsFactors = FALSE
	)
		
	widths <- sample(10, ncol(data), replace = TRUE)
	dt <- getClinDT(data, columnsWidth = widths)
	tableJSON <- exportAndGetTableJSON(dt)
	
	# extract column defs
	cDefs <- tableJSON$x$options$columnDefs
	
	# and column widths
	if(!is.data.frame(cDefs)){ # list if multiple column defs
		idxColDefs <- which(sapply(cDefs, function(x) "columnsWidth" %in% colnames(x)))
		expect_length(idxColDefs, 1) # is column width specified?
		cDefs <- cDefs[[idxColDefs]]
	}
	
	# check if match specified width:
	cColWidths <- cDefs[match(seq_along(widths), cDefs$targets), "columnsWidth"]
	expect_equal(cColWidths, widths)
	
})

test_that("Specification of extra options", {
			
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
	tableJSON <- exportAndGetTableJSON(dt)
	expect_equal(tableJSON$x$options$pageLength, 20) # page length properly set
	
	# extra options not available in the function
	expect_silent(dt <- getClinDT(data, options = list(lengthChange = 20)))
	tableJSON <- exportAndGetTableJSON(dt)
	expect_equal(tableJSON$x$options[["lengthChange"]], 20)
	
	# Note: wrong 'options' are handled by the JS DataTable library

})

test_that("Expand variables", {
			
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
	tableJSON <- exportAndGetTableJSON(dt)
	cDefs <- tableJSON$x$options$columnDefs
	
	# target columns are hidden
	expect_silent(cColsHidden <- unlist(subset(cDefs, !visible)$targets))
	expect_setequal(object = cColsHidden, expected = match(expandVar, colnames(data)))
	
	# and correct variables specified in JS callback
	expandVarLab <- names(colnames)[match(expandVar, colnames)]
	expect_true(all(sapply(expandVarLab, grepl, tableJSON$x$callback, fixed = TRUE)))
	
	# wrong variable
	expect_warning(dt <- getClinDT(data, expandVar = "blabla"))
			
})


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
	file <- "test.html";unlink(file)
	expect_silent(dt <- getClinDT(data, file = file))
	expect_true(file.exists(file))
	unlink(file)
	
	# incorrect file format
	expect_error(dt <- getClinDT(data, file = "test.csv"), pattern = "extension")
	
})