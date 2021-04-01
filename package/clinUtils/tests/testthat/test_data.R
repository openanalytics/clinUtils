context("Datasets")

test_that("ADaM datasets properly imported", {
	
	data(ADaMDataPelican)
	expect_true(exists("ADaMDataPelican"))
	expect_is(ADaMDataPelican, "list")
	expect_true(all(sapply(ADaMDataPelican, inherits, "data.frame")))
	expect_named(ADaMDataPelican)
	
})

test_that("ADaM labels properly imported", {
	data(labelVarsADaMPelican)
	expect_true(exists("labelVarsADaMPelican"))
	expect_named(labelVarsADaMPelican)
	expect_is(labelVarsADaMPelican, "character")
})
			
test_that("SDTM datasets properly imported", {
	
	data(SDTMDataPelican)
	expect_true(exists("SDTMDataPelican"))
	expect_is(SDTMDataPelican, "list")
	expect_true(all(sapply(SDTMDataPelican, inherits, "data.frame")))
	expect_named(SDTMDataPelican)
			
})

test_that("SDTM labels properly imported", {
	data(labelVarsSDTMPelican)
	expect_true(exists("labelVarsSDTMPelican"))
	expect_named(labelVarsSDTMPelican)
	expect_is(labelVarsSDTMPelican, "character")
})
