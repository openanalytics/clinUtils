context("Import SAS and xpt datasets")

library(tools)

pathSASDatasetsFiles <- list.files(path = system.file("extdata", package = "glpgUtilityFct"), pattern = "*.sas7bdat", full.names = TRUE)

test_that("Load data from SAS dataset files properly", {
	
	expect_message(adamData <- loadDataADaMSDTM(files = pathSASDatasetsFiles, verbose = TRUE))
	listNames <- names(adamData)
			
	# Output expectations #
	expect_is(adamData, "list")
	expect_named(adamData)
	fileName <- tools::file_path_sans_ext(basename(pathSASDatasetsFiles))
	isFileImported <- sapply(fileName, function(name) any(grepl(name, x = names(adamData), ignore.case = TRUE)))
	expect_true(all(isFileImported))
	
	datasets <- as.character(sapply(adamData, function(data) unique(data$DATASET)))
	isDatasetSet <- sapply(datasets, function(name) any(grepl(name, x = names(adamData), ignore.case = TRUE)))
	expect_true(all(isDatasetSet))
	
	tmp <- sapply(adamData, function(x) expect_is(x, class = "data.frame"))
	
	# Internal expectations #
	expect_warning(
		loadDataADaMSDTM(files = c(pathSASDatasetsFiles[1], pathSASDatasetsFiles[1])),
		regexp = "duplicated file name"
	)
	expect_error(loadDataADamSDTM("test.xlsx"))
	
	# test 'encoding'
	expect_error(loadDataADaMSDTM(files = pathSASDatasetsFiles, encoding = "test"))
	
	# pass arguments to 'read_sas'
	dataSubset <- loadDataADaMSDTM(files = pathSASDatasetsFiles[[1]], n_max = 3)
	expect_equal(nrow(dataSubset[[1]]), 3)
			
})

test_that("Load label variables from SAS dataset files properly", {
		
	adamData <- loadDataADaMSDTM(files = pathSASDatasetsFiles)	
	labelVars <- attr(adamData, "labelVars")
	
	expect_is(labelVars, "character")
	expect_named(labelVars)
	# all variables have labels, and all labels are present in the data:
	expect_setequal(names(labelVars), unlist(lapply(adamData, names)))
			
})

test_that("Convert date variables properly", {
			
	pathData <- grep("adcm", pathSASDatasetsFiles, value = TRUE)
	patternDate <- c("(DT|DTC)$")
	
	# variable in correct date format
	correctDateVar <- "TRTSDT"
	expect_message(
		adamData <- loadDataADaMSDTM(files = pathData, 
			convertToDate = TRUE, dateVars = patternDate),
		message = paste("Convert .*", correctDateVar, "to calendar date/time format")
	)
	expect_is(adamData[["ADCM"]][[correctDateVar]], "POSIXct")
	
	wrongDateFmtVar <- "CMSTDTC"
	expect_warning(
		adamData <- loadDataADaMSDTM(files = pathData, 
			convertToDate = TRUE, dateVars = patternDate),
		message = paste(wrongDateFmtVar, "not of specified calendar date format")
	)
	expect_is(adamData[["ADCM"]][[wrongDateFmtVar]], "character")
			
})

test_that("Conversion to date/time", {
	
	expect_warning(convertToDateTime(c("2020", "01-04-2020")), regexp = "not of specified calendar date format")
	expect_message(convertToDateTime(c("2020-04-01")), regexp = "Convert vector to calendar date/time format")
	
	expect_error(convertToDateTime(c(1, 2)))
	
	
})

test_that("Load data from xpt files properly", {
			
	pathXPTFile <- list.files(path = system.file("extdata", package = "glpgUtilityFct"), pattern = "*.xpt", full.names = TRUE)
	expect_message(adamDataFromXpt <- loadDataADaMSDTM(files = pathXPTFile, verbose = TRUE))
	
	expect_is(adamDataFromXpt, "list")
	expect_named(adamDataFromXpt)
	fileName <- tools::file_path_sans_ext(basename(pathXPTFile))
	isFileImported <- sapply(fileName, function(name) any(grepl(name, x = names(adamDataFromXpt), ignore.case = TRUE)))
	expect_true(all(isFileImported))			
			
})

