context("Figure comparison")

# create example figures

data(iris)
vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

tmpdir <- tempdir()
dirPlots <- file.path(tmpdir, "figures")
dir.create(dirPlots)

pathPlotBasic <- file.path(dirPlots, paste0("scatterplotIris-basic", ".png"))
png(pathPlotBasic)
pairs(iris[, vars])
tmp <- dev.off()

pathPlotColor <- file.path(dirPlots, paste0("scatterplotIris-color", ".png"))
png(pathPlotColor)
pairs(iris[, vars], col = iris$Species)
tmp <- dev.off()

outputDir <- file.path(tmpdir, "compareFigures")
unlink(outputDir, recursive = TRUE)

test_that("Two identical figures are identified as similar", {
			
	expect_message(
		compareFigOne(
			pathFigRef = pathPlotBasic, 
			pathFigNew = pathPlotBasic, 
			outputDir = outputDir
		),
		pattern = "Comparing: .* ok"
	)
	expect_length(list.files(path = outputDir), 0)

})

test_that("Two different figures are identified as different", {
			
	unlink(outputDir, recursive = TRUE)
	expect_message(
		compareFigOne(
			pathFigRef = pathPlotBasic, 
			pathFigNew = pathPlotColor, 
			outputDir = outputDir
		),
		pattern = "Comparing: .* : DIFFERENT!.* Check difference in: "
	)
	
#	expect_length(list.files(path = outputDir), 1)
	
	unlink(outputDir, recursive = TRUE) # clean
			
})

test_that("Comparison of two directories with figures", {
			
	dirPlots2 <- file.path(tmpdir, "figures2")
	dir.create(dirPlots2)
	
	# same figure
	file.copy(from = pathPlotBasic, to = file.path(dirPlots2, basename(pathPlotBasic)))
	# different figure
	file.copy(from = pathPlotBasic, to = file.path(dirPlots2, basename(pathPlotColor)))
	
	expect_message(
		compareFigs(
			pathFigRef = dirPlots, 
			pathFigNew = dirPlots2, 
			outputDir = outputDir
		),
		pattern = paste0(
			"Comparing: ", basename(pathPlotBasic), ": ok! .*",
			"Comparing: ", basename(pathPlotColor), ": DIFFERENT!"
		)
	)
	
#	expect_length(list.files(path = outputDir), 1)
	
	# clean
	unlink(outputDir, recursive = TRUE)
	unlink(dirPlots2, recursive = TRUE)
			
})

test_that("Directories don't exist", {
			
	expect_warning(
		compareFigs(
			pathFigRef = "", 
			pathFigNew = ""
		)
	)
	
})

unlink(dirPlots, recursive = TRUE) # clean