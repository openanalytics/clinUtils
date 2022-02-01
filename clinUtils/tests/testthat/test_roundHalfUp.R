context("Round a number half up")

test_that("Positive numbers are correctly rounded half up", {
      
	## positive value
	expect_equal(roundHalfUp(x = 0.001, digits = 1), 0)		
      
	expect_equal(roundHalfUp(x = 4.0, digits = 2), 4.00)
	expect_equal(roundHalfUp(x = 0.55, digits = 1), 0.6)
	expect_equal(roundHalfUp(x = 0.55, digits = 1), round(0.55, 1))
	
	# differ than R default: 'round to even'
	expect_equal(roundHalfUp(x = 0.45, digits = 1), 0.5)
	expect_false(roundHalfUp(x = 0.45, digits = 1) == round(0.45, 1))
	
	# test case leading to issue with implementation in version < 0.21.0 (was returning 18.27)
	expect_equal(roundHalfUp(x = 18.275, digits = 2), 18.28)
	
	  
})

test_that("Negative numbers are correctly rounded half up", {
			
	expect_equal(roundHalfUp(x = -0.55, digits = 1), -0.6)
	expect_equal(roundHalfUp(x = -0.45, digits = 1), -0.5)
	
})

test_that("A very small number is correctly rounded half up", {
			
	expect_equal(roundHalfUp(x = 1e-300, digits = 300), 1e-300)
      
})

test_that("Numbers are correctly formatted as rounded half up text", {
      
	expect_equal(roundHalfUpTextFormat(x = 4.0, digits = 2), "4.00")
	expect_equal(roundHalfUpTextFormat(x = 0.55, digits = 1), "0.6")
	expect_equal(
		roundHalfUpTextFormat(x = 0.55, digits = 1),
		as.character(round(0.55, 1))
	)
      
})
