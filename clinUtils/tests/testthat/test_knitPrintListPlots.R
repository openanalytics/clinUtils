context("Include list of plots/objects in a report")

# Note: some tests are skipped in CRAN
# because of pandoc requirement

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

tmpdir <- tempdir()

test_that("Labels are correctly set for each plot when specified", {	
      
      figDir <- paste0(file.path(tmpdir, "knitPrintListPlots-labels"), "/")
      dir.create(figDir)
      knitr::opts_chunk$set(fig.path = figDir)
      
      expect_silent(
          resKPLP <- capture.output(
              knitPrintListPlots(
                  plotsListStatic, 
                  type = "ggplot2",
                  labels = labels
              )
          )
      )
      
      idxFigInOutput <- sapply(labels, function(lab) which(grepl(lab, basename(resKPLP))))
      expect_true(all(!is.na(idxFigInOutput)), info = "missing figures")
      expect_true(all(diff(idxFigInOutput) > 0), info = "figures not ordered as specified")
      
      isFigCreated <- sapply(labels, function(lab) any(
                grepl(
                    paste0("^", lab, ".+.png"), 
                    list.files(figDir)
                )
            ))
      expect_true(all(isFigCreated), info = "figures not created")
      
    })

test_that("A general label is correctly set for all plots", {
      
      figDir <- paste0(file.path(tmpdir, "knitPrintListPlots-generalLabel"), "/")
      dir.create(figDir)
      knitr::opts_chunk$set(fig.path = figDir)
      
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
      
    })

test_that("Titles are correctly set for the plots", {
      
      figDir <- paste0(file.path(tmpdir, "knitPrintListPlots-titles"), "/")
      dir.create(figDir)
      knitr::opts_chunk$set(fig.path = figDir)
      
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
      
    })

test_that("A custom knitr option is correctly set", {
      
      figDir <- paste0(file.path(tmpdir, "knitPrintListPlots-captions"), "/")
      dir.create(figDir)
      knitr::opts_chunk$set(fig.path = figDir)
      
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

test_that("A list of interactive plots (plotly) is correctly included", {
      
      skip_if_not(
          condition = rmarkdown::pandoc_available(), 
          message = "pandoc is not available"
      )
      
      file <- tempfile(pattern = "includeListPlotly_knitPrintListPlots", fileext = ".Rmd")
      includePlotsInRmdDoc(
          "knitPrintListPlots(plotsList = plotsListInteractive, type = \"plotly\")",
          file = file
      )
      expect_silent(outputRmd <- rmarkdown::render(file, quiet = TRUE))
      
      outputHTMLPlots <- readLines(outputRmd)
      expect_length(grep('class="plotly html-widget', outputHTMLPlots), 2)

    })

test_that("A list of interactive objects (plotly) is correctly included", {
      
      skip_if_not(
          condition = rmarkdown::pandoc_available(), 
          message = "pandoc is not available"
      )
      
      file <- tempfile(pattern = "includeListPlotly_knitPrintListObjects", fileext = ".Rmd")
      includePlotsInRmdDoc(
          "knitPrintListObjects(xList = plotsListInteractive, printObject = FALSE)",
          file = file
      )
      expect_silent(outputRmd <- rmarkdown::render(file, quiet = TRUE))
      outputHTMLObjects <- readLines(outputRmd)
      expect_length(grep('class="plotly html-widget', outputHTMLObjects), 2)

    })

test_that("A list of interactive table objects (flextable) is correctly included", {
      
      skip_if_not(
          condition = rmarkdown::pandoc_available(), 
          message = "pandoc is not available"
      )
      
      file <- tempfile(pattern = "includeListFlextable", fileext = ".Rmd")
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
      
    })