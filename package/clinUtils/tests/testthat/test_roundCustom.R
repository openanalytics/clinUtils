context("Test 'roundCustom'")

test_that("'roundCustom' function returns correct results", {
			
	## positive value
	expect_equal(roundCustom(x = 0.001, digits = 1), 0)		
			
	expect_equal(roundCustom(x = 4.0, digits = 2), 4.00)
	expect_equal(roundCustom(x = 0.55, digits = 1), 0.6)
	expect_equal(roundCustom(x = 0.55, digits = 1), round(0.55, 1))
	
	# differ than R default: 'round to even'
	expect_equal(roundCustom(x = 0.45, digits = 1), 0.5)
	expect_false(roundCustom(x = 0.45, digits = 1) == round(0.45, 1))
	
	# test case leading to issue with implementation in version < 0.21.0 (was returning 18.27)
	expect_equal(roundCustom(x = 18.275, digits = 2), 18.28)
	
	# option no longer supported
	expect_error(roundCustom(x = 0.55, digits = 1, format = "text"))
	
	## negative numbers
	expect_equal(roundCustom(x = -0.55, digits = 1), -0.6)
	expect_equal(roundCustom(x = -0.45, digits = 1), -0.5)
	
	## very small number
	expect_equal(roundCustom(x = 1e-300, digits = 300), 1e-300)
	
})