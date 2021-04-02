context("Import SAS and xpt datasets")

library(tools)
library(haven)

test_that("Load data from SAS dataset files properly", {
	
	# create dummy datasets
	data <- data.frame(
		USUBJID = sample.int(5),
		TRT = c("A", "B", "A", "B", "A")
	)
	pathSAS7bdat <- tempfile(fileext = ".sas7bdat")
	haven::write_sas(data = data, path = pathSAS7bdat)

	expect_message(adamData <- loadDataADaMSDTM(files = pathSAS7bdat, verbose = TRUE))
	listNames <- names(adamData)
			
	# Output expectations #
	expect_is(adamData, "list")
	expect_named(adamData)
	fileName <- tools::file_path_sans_ext(basename(pathSAS7bdat))
	isFileImported <- sapply(fileName, function(name) any(grepl(name, x = names(adamData), ignore.case = TRUE)))
	expect_true(all(isFileImported))
	
	datasets <- as.character(sapply(adamData, function(data) unique(data$DATASET)))
	isDatasetSet <- sapply(datasets, function(name) any(grepl(name, x = names(adamData), ignore.case = TRUE)))
	expect_true(all(isDatasetSet))
	
	tmp <- sapply(adamData, function(x) expect_is(x, class = "data.frame"))
	
	# Internal expectations #
	expect_warning(
		loadDataADaMSDTM(files = c(pathSAS7bdat, pathSAS7bdat)),
		regexp = "duplicated file name"
	)
	expect_error(loadDataADamSDTM("test.xlsx"))
	
	# test 'encoding'
	expect_error(loadDataADaMSDTM(files = pathSAS7bdat, encoding = "test"))
	
	# pass arguments to 'read_sas'
	dataSubset <- loadDataADaMSDTM(files = pathSAS7bdat, n_max = 3)
	expect_equal(nrow(dataSubset[[1]]), 3)
			
})

test_that("Load label variables from SAS dataset files properly", {
			
	# create dummy dataset
	data <- data.frame(
		USUBJID = structure(sample.int(5), label = "Subject ID"),
		TRT = structure(c("A", "B", "A", "B", "A"), label = "Treatment"),
		stringsAsFactors = FALSE
	)
	pathSAS7bdat <- tempfile(fileext = ".sas7bdat")
	haven::write_sas(data = data, path = pathSAS7bdat)		
		
	adamData <- loadDataADaMSDTM(files = pathSAS7bdat)	
	labelVars <- attr(adamData, "labelVars")
	
	expect_is(labelVars, "character")
	expect_named(labelVars)
	# all variables have labels, and all labels are present in the data:
	expect_setequal(setdiff(names(labelVars), "DATASET"), colnames(data))
			
})

test_that("Convert date variables properly", {
			
	# create dummy dataset
	data <- data.frame(
		USUBJID = sample.int(3), 
		ADTC = c("2020-04-10", "2020-01-01", "2019-12-03"), 
		stringsAsFactors = FALSE
	)
	pathData <- tempfile(fileext = ".sas7bdat")
	haven::write_sas(data = data, path = pathData)				
	
	# variable in correct date format
	expect_message(
		adamData <- loadDataADaMSDTM(
			files = pathData, 
			convertToDate = TRUE, dateVars = "ADTC"
		),
		message = paste("Convert date to calendar date/time format")
	)
	expect_is(adamData[[1]]$ADTC, "POSIXct")
	
	wrongDateFmtVar <- "CMSTDTC"
	
	# create dummy dataset
	data <- data.frame(
		USUBJID = sample.int(3), 
		ADTC = c("20200410", "20200101", "20180309"), 
		stringsAsFactors = FALSE
	)
	pathData <- tempfile(fileext = ".sas7bdat")
	haven::write_sas(data = data, path = pathData)	
	
	expect_warning(
		adamData <- loadDataADaMSDTM(files = pathData, 
			convertToDate = TRUE, dateVars = "ADTC"),
		message = "Date is not of specified calendar date format"
	)
	expect_is(adamData[[1]]$ADTC, "character")
			
})

test_that("Conversion to date/time", {
	
	expect_warning(convertToDateTime(c("2020", "01-04-2020")), regexp = "not of specified calendar date format")
	expect_message(convertToDateTime(c("2020-04-01")), regexp = "Convert vector to calendar date/time format")
	
	expect_error(convertToDateTime(c(1, 2)))
	
})

test_that("Load data from xpt files properly", {
			
	# create dummy dataset
	data <- data.frame(
		USUBJID = structure(sample.int(5), label = "Subject ID"),
		TRT = structure(c("A", "B", "A", "B", "A"), label = "Treatment"),
		stringsAsFactors = FALSE
	)
	pathXPTFile <- tempfile(fileext = ".xpt")
	haven::write_xpt(data = data, path = pathXPTFile)		
			
	expect_message(adamDataFromXpt <- loadDataADaMSDTM(files = pathXPTFile, verbose = TRUE))
	
	expect_is(adamDataFromXpt, "list")
	expect_named(adamDataFromXpt)
	fileName <- tools::file_path_sans_ext(basename(pathXPTFile))
	isFileImported <- sapply(fileName, function(name) any(grepl(name, x = names(adamDataFromXpt), ignore.case = TRUE)))
	expect_true(all(isFileImported))			
			
})

