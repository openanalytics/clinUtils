context("Get palette for a CDISC variable")

test_that("The color palette for a CDISC NRIND variable contains expected elements", {
			
	palette <- colorPaletteNRIND
	expect_is(palette, "character")
	expect_named(palette, c("LOW", "NORMAL", "HIGH", "ABNORMAL", "UNKNOWN", "NA"))
		
})

test_that("The shape palette for a CDISC NRIND variable contains expected elements", {
			
	palette <- shapePaletteNRIND
	expect_is(palette, "numeric")
	expect_named(palette, c("LOW", "NORMAL", "HIGH", "ABNORMAL", "UNKNOWN", "NA"))
			
})

test_that("An error is generated if the type of the CDISC variable is not specified", {
			
	xNRIND <- c("HIGH", "LOW", "NORMAL")
	expect_error(getPaletteCDISC(x = xNRIND, type = "shape"))
	expect_error(getPaletteCDISC(x = xNRIND, type = "shape", var = "blabla"))
			
})

test_that("An error is generated if the type of the palette is not specified", {
			
	xNRIND <- c("HIGH", "LOW", "NORMAL")
	expect_error(getPaletteCDISC(x = xNRIND, var = "NRIND"))
	expect_error(getPaletteCDISC(x = xNRIND, var = "NRIND", type = "blabla"))
			
})

test_that("The shape palette extracted for a string variable contains elements in a meaningful order", {
		
	xNRIND <- c("HIGH", "LOW", "NORMAL")
	palette <- getPaletteCDISC(x = xNRIND, var = "NRIND", type = "shape")
	expect_is(palette, "numeric")	
	expect_length(palette, 3)
	# palette ordered in meaningful order
	expect_named(palette, c("LOW", "NORMAL", "HIGH"))
	
})

test_that("The shape palette extracted for a factor variable retains the order of the input elements", {
			
	xNRIND <- factor(c("HIGH", "LOW", "NORMAL"), levels = c("NORMAL", "LOW", "HIGH"))
	palette <- getPaletteCDISC(x = xNRIND, var = "NRIND", type = "shape")
	expect_is(palette, "numeric")	
	expect_length(palette, 3)
	# palette ordered as levels of the factor
	expect_named(palette, c("NORMAL", "LOW", "HIGH"))
		
})

test_that("The palette contains empty elements as last elements", {
			
	xNRIND <- c("", NA_character_, "LOW", "NORMAL")
	palette <- getPaletteCDISC(x = xNRIND, var = "NRIND", type = "shape")	
	expect_length(palette, 4)
	# palette ordered first in meaningful order, then with extra element(s)
	expect_named(palette, c("LOW", "NORMAL", NA_character_, ""))
			
})

test_that("Extra elements when specified are correctly included in the palette", {
			
	xNRIND <- c("HIGH", "NORMAL", "VERY HIGH")
	palette <- getPaletteCDISC(x = xNRIND, 
		var = "NRIND", type = "shape",
		palette = c(`VERY HIGH` = 12)
	)	
	expect_length(palette, 3)
	# palette ordered first in meaningful order, then with extra element(s)
	expect_named(palette, c("NORMAL", "HIGH", "VERY HIGH"))
	expect_equal(palette["VERY HIGH"], 12, check.attributes = FALSE)
			
})

test_that("The color palette extracted for a string variable contains elements in a meaningful order", {
	
	xNRIND <- c("HIGH", "NORMAL")
	palette <- getPaletteCDISC(x = xNRIND, 
		var = "NRIND", type = "color"
	)
	expect_is(palette, "character")
	expect_length(palette, 2)
	expect_named(palette, c("NORMAL", "HIGH"))
			
})
			

