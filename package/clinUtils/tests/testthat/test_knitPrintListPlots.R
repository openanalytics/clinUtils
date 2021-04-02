context("Include list of plots in knitr")

library(ggplot2)
library(knitr)
library(rmarkdown)
library(plotly)
library(flextable)

plotsListStatic <- list(
	A = ggplot(data = cars, aes(x = speed, y = dist)) + geom_point(),
	B = ggplot(data = cars, aes(x = speed, y = dist)) + geom_line()
)
generalLabel <- "scatter"
labels <- names(plotsListStatic)

figDir <- "figuresTest/"
knitr::opts_chunk$set(fig.path = figDir)

test_that("Knitting list of plots with labels", {	
			
	expect_silent(
		resKPLP <- capture.output(
			knitPrintListPlots(
				plotsListStatic, 
				type = "ggplot2",
				labels = labels
			)
		)
	)
	
	idxFigInOutput <- sapply(labels, function(lab) which(grepl(lab, resKPLP)))
	expect_true(all(!is.na(idxFigInOutput)), info = "missing figures")
	expect_true(all(diff(idxFigInOutput) > 0), info = "figures not ordered as specified")
	
	isFigCreated <- sapply(labels, function(lab) any(
		grepl(
			paste0("^", lab, ".+.png"), 
			list.files(figDir)
		)
	))
	expect_true(all(isFigCreated), info = "figures not created")
	
	unlink(figDir, recursive = TRUE) # clean
	
})

test_that("Knitting list of plots with general label", {
						
	expect_silent(
		resKPLP <- capture.output(
			knitPrintListPlots(
				plotsListStatic, 
				type = "ggplot2",
				generalLabel = generalLabel
			)
		)
	)		
	
	isGeneralLabelUsed <- all(grepl(generalLabel, resKPLP[resKPLP != ""]))
	expect_true(isGeneralLabelUsed, info = "figures not created")
	
	isFigCreated <- sapply(list.files(figDir), grepl, pattern = paste0("^", generalLabel, ".+.png"))
	expect_true(all(isFigCreated), info = "general label not used")		
	
	unlink(figDir, recursive = TRUE) # clean
	
})

test_that("Knitting list of plots with titles", {

	titles <- paste("Visualization:", names(plotsListStatic))
	expect_silent(
		resKPLP <- capture.output(
			knitPrintListPlots(
				plotsListStatic, 
				type = "ggplot2",
				titles = titles,
				titleLevel = 4
			)
		)
	)	
	
	expect_true(all(paste("####", titles) %in% resKPLP))
	
	unlink(figDir, recursive = TRUE) # clean

})

test_that("Specify valid custom knitr option", {
			
	captions <- paste("Caption:", seq_along(plotsListStatic))
	expect_silent(
		resKPLP <- capture.output(
			knitPrintListPlots(
				plotsListStatic, 
				type = "ggplot2",
				fig.cap = captions
			)
		)
	)	
	idxCaptionInOutput <- sapply(captions, function(lab) which(grepl(lab, resKPLP)))
	expect_true(all(!is.na(idxCaptionInOutput)), info = "missing caption")
	expect_true(all(diff(idxCaptionInOutput) > 0), info = "captions not ordered as specified")
			
	unlink(figDir, recursive = TRUE) # clean
	
})

includePlotsInRmdDoc <- function(includePlotCmd, file)
	cat(
		"---",
		"title: test inclusion list of plotly objects",
		"output: rmarkdown::html_document",
		"---",
		"```{r results = 'asis'}",
		"library(plotly);library(clinUtils)",
		"plotsListInteractive <- list(
			A = plot_ly(data = cars, x = ~speed, y = ~dist, type = \"scatter\", mode = \"marker\"),
			B = plot_ly(data = cars, x = ~speed, y = ~dist, type = \"scatter\", mode = \"line\")
		)",
		includePlotCmd,
		"```",
		file = file, sep = "\n"
	)

test_that("Knitting list of interactive plots with 'knitPrintListObjects'", {
	
	file <- "includeListPlotly_knitPrintListPlots.Rmd"
	includePlotsInRmdDoc(
		"knitPrintListPlots(plotsList = plotsListInteractive, type = \"plotly\")",
		file = file
	)
	expect_silent(outputRmd <- rmarkdown::render(file, quiet = TRUE))
			
	outputHTMLPlots <- readLines(outputRmd)
	expect_length(grep("class=\"plotly html-widget\"", outputHTMLPlots), 2)
	
	unlink(c(file, outputRmd)) # clean
	
})

test_that("Knitting list of interactive plots with 'knitPrintListObjects'", {
			
	file <- "includeListPlotly_knitPrintListObjects.Rmd"
	includePlotsInRmdDoc(
		"knitPrintListObjects(xList = plotsListInteractive, printObject = FALSE)",
		file = file
	)
	expect_silent(outputRmd <- rmarkdown::render(file, quiet = TRUE))
	outputHTMLObjects <- readLines(outputRmd)
	expect_length(grep("class=\"plotly html-widget\"", outputHTMLObjects), 2)
	
	unlink(c(file, outputRmd)) # clean
	
})

test_that("Knitting list of interactive flextable with 'knitPrintListObjects'", {
		
	file <- "includeListFlextable.Rmd"
	cat(
		"---",
		"title: test inclusion list of tables",
		"output: rmarkdown::html_document",
		"---",
		"```{r results = \"asis\"}",
		"library(flextable);library(clinUtils)",
		"listTables <- list(flextable(iris), flextable(cars))",
		"knitPrintListObjects(xList = listTables)",
		"```",
		file = file, sep = "\n"
	)
	
	expect_silent(outputRmd <- rmarkdown::render(file, quiet = TRUE))
	outputHTMLFt <- readLines(outputRmd)
	expect_length(grep("class=\"tabwid\"", outputHTMLFt), 2)
	
	unlink(c(file, outputRmd)) # clean
			
})