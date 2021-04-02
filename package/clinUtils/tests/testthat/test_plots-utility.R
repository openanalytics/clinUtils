context("Test plots-utility")

test_that("Formatting long labels", {
			
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

test_that("Formatting variables for plot labels", {
			
	dataAE <- data.frame(
		USUBJID = c("1", "1", "2", "2", "3"), 
		SEX = c("F", "F", "F", "F", "M"),
		AEBODSYS = c("II", "I", "I", "I", "II"),
		AEBDSYCD = c(2, 1, 1, 1, 2),
		AEHLT = c("C", "B", "A", "A", "C"),
		AEDECOD = c("c1", "b1", "a1", "a2", "c1"),
		stringsAsFactors = FALSE
	)		
	
      expect_null(
          formatVarForPlotLabel(
              dataAE,
              paramVar = NULL
          )
      )
      expect_warning(
          formatVarForPlotLabel(
              data = dataAE,
              paramVar = "SEX",
              paramGroupVar = "TRT"
          )
      )
      expect_silent(
			formattedVar <- formatVarForPlotLabel(
	          dataAE,
	          paramVar = "SEX"
	      )
  		)
      expect_is(formattedVar, "factor")
      expect_identical(
          levels(formattedVar),
          levels(factor(dataAE$SEX))
      )
      
      formattedVar <- formatVarForPlotLabel(
          dataAE,
          paramVar = "SEX",
          revert = TRUE
      )
      expect_is(formattedVar, "factor")
      expect_identical(
          levels(formattedVar),
          rev(levels(factor(dataAE$SEX)))
      )
	  
	# check ordering when character paramGroupVar is specified
	groupVars <- c("AEBODSYS", "AEHLT")
	varByGroup <- formatVarForPlotLabel(
		data = dataAE,
		paramVar = "AEDECOD",
		paramGroupVar = groupVars,
		width = Inf
	)
	dataGroup <- unique(dataAE[, c(groupVars, "AEDECOD")])
	dataGroup <- dataGroup[do.call(order, dataGroup), ]
	expect_identical(levels(varByGroup), unique(dataGroup$AEDECOD))
	
	# check ordering when factor paramGroupVar is specified
	dataAECustomOrder <- dataAE
	dataAECustomOrder$AEBODSYS <- with(dataAECustomOrder, reorder(AEBODSYS, AEBDSYCD))
	dataAECustomOrder$AEBODSYS <- with(dataAECustomOrder, factor(AEBODSYS, levels = rev(levels(AEBODSYS))))
	varByGroup <- formatVarForPlotLabel(
		data = dataAECustomOrder,
		paramVar = "AEDECOD",
		paramGroupVar = groupVars,
		width = Inf
	)
	dataGroup <- unique(dataAECustomOrder[, c(groupVars, "AEDECOD")])
	dataGroup <-  dataGroup[do.call(order, dataGroup), ]
	expect_identical(levels(varByGroup), unique(dataGroup$AEDECOD))
	
	# width
	varMaxWidth <- formatVarForPlotLabel(
		data = dataAE,
		paramVar = "AEBODSYS",
		width = 20
	)
	oldVarWords <- unlist(strsplit(dataAE$AEBODSYS, split = " "))
	newVarWords <- unlist(strsplit(levels(varMaxWidth), split = "\n", fixed = TRUE))
	
	expect_lte(max(nchar(newVarWords)), max(max(nchar(oldVarWords)), 20))
	  
})

