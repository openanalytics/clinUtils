context("Test miscellaneous functions")

test_that("createPatientProfileVar", {
		
	data <- data.frame(USUBJID = c("subj1", "subj2", "subj3"))		
			
	expect_error(
		createPatientProfileVar(
			data = data,
			patientProfilePath = "unExistingFolder"
		),
		regex  = "*not found"
	)
	
	patientProfilePath <- "patientProfiles"	
	
	expect_silent(
		dataUpdated <- createPatientProfileVar(
			data = data,
			patientProfilePath = patientProfilePath, checkExist = FALSE
		)
	)
	# Patient profile columns properly created
	patientProfileVars <- c("patientProfileLink", "patientProfilePath")
	expect_true(all(patientProfileVars %in% colnames(dataUpdated)))
	expect_true(all(sapply(dataUpdated[, patientProfileVars], inherits, "character")))
	
	dir.create(patientProfilePath)
	expect_warning(
		createPatientProfileVar(
			data = data,
			patientProfilePath = patientProfilePath,
			subjectVar = "usubjid"
		),
		regex = "patient profile variable is not created"
	)
	
	unlink(patientProfilePath)
	
})

test_that("Reorder columns", {
	
	data <- data.frame(
		USUBJID = c("subj1", "subj2", "subj3"),
		TRT = c("A", "B", "A"),
		AGE = sample.int(n = 10, size = 3)
	)
			
	expect_error(
		reorderColumns(
			data = data, 
			vars = c("unexistingColumn" = 1, "VAR1" = 2)
		)
	)
			
	dataOrder <- reorderColumns(
		data = data, 
		vars = c("USUBJID" = 1)
	)			
	expect_is(dataOrder, "data.frame")
	
	# order properly set
	expect_equal(colnames(dataOrder)[1], "USUBJID")
	
	# dataset not modified
	expect_identical(data[, colnames(dataOrder)], dataOrder)
	
	# all columns are retained
	expect_setequal(colnames(data), colnames(dataOrder)) 
	
})

test_that("Extraction of column indices in Javascript", {
	
	data <- data.frame(
		USUBJID = c("subj1", "subj2", "subj3"),
		TRT = c("A", "B", "A"),
		AGE = sample.int(n = 10, size = 3)
	)		
			
	expect_equal(
		getJavaScriptColumnsIdx(data, vars = colnames(data)[c(3, 1)]),
		c(2, 0)
	)
			
})

test_that("Remove columns with all missing values", {
			
	data <- data.frame(A = seq_len(2), B = c(NA, 3), C = c(NA_character_, NA_character_))
	
	expect_identical(
		removeNaCols(data),
		data[, -3]
	)
			
})

test_that("Capitalization of first letter with 'simpleCap'", {
	
	expect_identical(
		object = unname(simpleCap(x = "this is an example sentence")), 
		expected = "This is an example sentence"
	)
	expect_identical(
		object = unname(simpleCap(x = "this is an example sentence", onlyFirst = FALSE)), 
		expected = "This Is An Example Sentence"
	)
	expect_identical(
		object = unname(simpleCap(x = "This is a test", rev = TRUE)), 
		expected = "this is a test"
	)
			
})