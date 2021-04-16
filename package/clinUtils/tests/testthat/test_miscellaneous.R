context("Test miscellaneous functions")

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