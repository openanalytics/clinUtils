context("Get palette for CDISC variable")

test_that("Get CDISC NRIND color palette", {
			
	palette <- colorPaletteNRIND
	expect_is(palette, "character")
	expect_named(palette, c("LOW", "NORMAL", "HIGH", "ABNORMAL", "UNKNOWN", "NA"))
		
})

test_that("Get CDISC NRIND shape palette", {
			
	palette <- shapePaletteNRIND
	expect_is(palette, "numeric")
	expect_named(palette, c("LOW", "NORMAL", "HIGH", "ABNORMAL", "UNKNOWN", "NA"))
			
})

test_that("Variable type should be specified during extraction of CDISC palette", {
			
	xNRIND <- c("HIGH", "LOW", "NORMAL")
	expect_error(getPaletteCDISC(x = xNRIND, type = "shape"))
	expect_error(getPaletteCDISC(x = xNRIND, type = "shape", var = "blabla"))
			
})

test_that("Palette type should be specified during extraction of CDISC palette", {
			
	xNRIND <- c("HIGH", "LOW", "NORMAL")
	expect_error(getPaletteCDISC(x = xNRIND, var = "NRIND"))
	expect_error(getPaletteCDISC(x = xNRIND, var = "NRIND", type = "blabla"))
			
})

test_that("Extract shape palette for a CDISC NRIND character variable", {
		
	xNRIND <- c("HIGH", "LOW", "NORMAL")
	palette <- getPaletteCDISC(x = xNRIND, var = "NRIND", type = "shape")
	expect_is(palette, "numeric")	
	expect_length(palette, 3)
	# palette ordered in meaningful order
	expect_named(palette, c("LOW", "NORMAL", "HIGH"))
	
})

test_that("Extract palette for a CDISC NRIND factor variable", {
			
	xNRIND <- factor(c("HIGH", "LOW", "NORMAL"), levels = c("NORMAL", "LOW", "HIGH"))
	palette <- getPaletteCDISC(x = xNRIND, var = "NRIND", type = "shape")
	expect_is(palette, "numeric")	
	expect_length(palette, 3)
	# palette ordered as levels of the factor
	expect_named(palette, c("NORMAL", "LOW", "HIGH"))
		
})

test_that("Extract palette for a CDISC NRIND variable with empty element", {
			
	xNRIND <- c("", NA_character_, "LOW", "NORMAL")
	palette <- getPaletteCDISC(x = xNRIND, var = "NRIND", type = "shape")	
	expect_length(palette, 4)
	# palette ordered first in meaningful order, then with extra element(s)
	expect_named(palette, c("LOW", "NORMAL", NA_character_, ""))
			
})

test_that("Specify custom symbols for a CDISC NRIND variable with extra non standard elements", {
			
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

test_that("Extract color palette for a CDISC NRIND variable", {
	
	xNRIND <- c("HIGH", "NORMAL")
	palette <- getPaletteCDISC(x = xNRIND, 
		var = "NRIND", type = "color"
	)
	expect_is(palette, "character")
	expect_length(palette, 2)
			
})
			

