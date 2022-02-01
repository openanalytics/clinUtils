context("Test utility functions for visualizations")

test_that("A long string is correctly formatted with new lines", {
			
	labelLong <- paste(
		sapply(sample(5, 100, replace = TRUE),
			function(x) substring("aaaaaaaaaa", 1, x)),
		collapse = " ")
      
	expect_silent(formattedLabel <- formatLongLabel(labelLong, width = 20))
      
	expect_is(formattedLabel, "character")
      
	maxNCharData <- max(nchar(strsplit(labelLong, split = " ")[[1]]))
	maxNCharFormattedLabel <- max(nchar(strsplit(formattedLabel, split = "\n")[[1]]))
	  
	# max number of character is either max from data or specified max width
	expect_lte(maxNCharFormattedLabel, max(maxNCharFormattedLabel, maxNCharData))
      
})

test_that("An empty object is returned if the variable for plot label is not specified", {

	expect_null(
		formatVarForPlotLabel(
			data = data.frame(),
			paramVar = NULL
		)
	)
			
})

test_that("A warning is generated if the grouping variable for plot label is not available in the data", {
			
	expect_warning(
		formatVarForPlotLabel(
			data = data.frame(SEX = c("F", "M")),
			paramVar = "SEX",
			paramGroupVar = "TRT"
		),
		"grouping.*not available"
	)

})

test_that("A variable for plot label is correctly formatted as a factor", {

	data <- data.frame(
		USUBJID = c("1", "1", "2", "2", "3"), 
		SEX = c("F", "F", "F", "F", "M"),
		stringsAsFactors = FALSE
	)	
	expect_silent(
		formattedVar <- formatVarForPlotLabel(
			data = data,
			paramVar = "SEX"
		)
	)
	expect_is(formattedVar, "factor")
	expect_identical(
		levels(formattedVar),
		levels(factor(data$SEX))
	)
			
})

test_that("A variable for plot label is correctly formatted as a factor with reverse order when specified", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2", "3"), 
		SEX = c("F", "F", "F", "F", "M"),
		stringsAsFactors = FALSE
	)	
      
	formattedVar <- formatVarForPlotLabel(
		data = data,
		paramVar = "SEX",
		revert = TRUE
	)
	expect_is(formattedVar, "factor")
	expect_identical(
		levels(formattedVar),
		rev(levels(factor(data$SEX)))
	)
	  
})

test_that("A variable for plot label is correctly ordered based on multiple grouping variables", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2", "3"), 
		SEX = c("F", "F", "F", "F", "M"),
		AEBODSYS = c("II", "I", "I", "I", "II"),
		AEHLT = c("C", "B", "A", "A", "C"),
		AEDECOD = c("c1", "b1", "a1", "a2", "c1"),
		stringsAsFactors = FALSE
	)
	  
	# check ordering when character paramGroupVar is specified
	groupVars <- c("AEBODSYS", "AEHLT")
	varByGroup <- formatVarForPlotLabel(
		data = data,
		paramVar = "AEDECOD",
		paramGroupVar = groupVars,
		width = Inf
	)
	dataGroup <- unique(data[, c(groupVars, "AEDECOD")])
	dataGroup <- dataGroup[do.call(order, dataGroup), ]
	expect_identical(levels(varByGroup), unique(dataGroup$AEDECOD))
	
})
	
test_that("A variable for plot label is correctly ordered based on multiple grouping variables with specified order", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2", "3"), 
		SEX = c("F", "F", "F", "F", "M"),
		AEBODSYS = c("II", "I", "I", "I", "II"),
		AEBDSYCD = c(2, 1, 1, 1, 2),
		AEHLT = c("C", "B", "A", "A", "C"),
		AEDECOD = c("c1", "b1", "a1", "a2", "c1"),
		stringsAsFactors = FALSE
	)	
	data$AEBODSYS <- with(data, reorder(AEBODSYS, AEBDSYCD))
	data$AEBODSYS <- with(data, factor(AEBODSYS, levels = rev(levels(AEBODSYS))))
	groupVars <- c("AEBODSYS", "AEHLT")
	varByGroup <- formatVarForPlotLabel(
		data = data,
		paramVar = "AEDECOD",
		paramGroupVar = groupVars,
		width = Inf
	)
	dataGroup <- unique(data[, c(groupVars, "AEDECOD")])
	dataGroup <-  dataGroup[do.call(order, dataGroup), ]
	expect_identical(levels(varByGroup), unique(dataGroup$AEDECOD))
	
})

test_that("A variable for plot label is correctly formatted to a specific width", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2", "3"), 
		SEX = c("F", "F", "F", "F", "M"),
		AEBODSYS = c("II", "I", "I", "I", "II"),
		stringsAsFactors = FALSE
	)
	
	varMaxWidth <- formatVarForPlotLabel(
		data = data,
		paramVar = "AEBODSYS",
		width = 20
	)
	oldVarWords <- unlist(strsplit(data$AEBODSYS, split = " "))
	newVarWords <- unlist(strsplit(levels(varMaxWidth), split = "\n", fixed = TRUE))
	
	expect_lte(max(nchar(newVarWords)), max(max(nchar(oldVarWords)), 20))
	  
})

