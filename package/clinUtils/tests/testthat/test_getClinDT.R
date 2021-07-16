context("Get an interactive DataTables for clinical data")

# Note: last tests are skipped in CRAN
# because of pandoc requirement

# require extra libraries
library(tibble)
library(crosstalk)

test_that("A basic DataTables is correctly exported", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          SEX = c("F", "M", "F", "M", "F"),
          stringsAsFactors = FALSE
      )		
      
      expect_silent(dt <- getClinDT(data = data))
      expect_is(dt, "datatables")	
      expect_identical(object = dt$x$data, expected = data)
      
    })

test_that("An error is generated if data is of the wrong type", {
      expect_error(getClinDT(data = TRUE))
    })

test_that("SharedData DataTables are correctly generated", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          SEX = c("F", "M", "F", "M", "F"),
          stringsAsFactors = FALSE
      )		
      
      # works
      dataSD <- crosstalk::SharedData$new(data = data, key = as.formula("~USUBJID"))
      expect_silent(dt <- getClinDT(data = dataSD))
      expect_identical(dt$x$data, data)
      
    })

test_that("An error is generated if a SharedData with incorrect key is set", {
      
      expect_error(dt <- getClinDT(crosstalk::SharedData$new(data = data, key = "test")))
      
    })

test_that("A DataTables is correctly generated when input table is a tibble", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          SEX = c("F", "M", "F", "M", "F"),
          stringsAsFactors = FALSE
      )		
      
      dataTB <- tibble::as_tibble(data)
      expect_silent(dt <- getClinDT(data = dataTB))
      expect_identical(dt$x$data, data)
      
    })

test_that("Column names are successfully renamed in DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          SEX = c("F", "M", "F", "M", "F"),
          stringsAsFactors = FALSE
      )
      colnames <- c(
          "Sex" = "SEX",
          "Subject ID" = "USUBJID",
          "Treatment" = "TRT"
      )
      
      # correct
      expect_silent(dt <- getClinDT(data = data, colnames = colnames))
      expect_match(
          object = dt$x$container,
          regexp = ".+<th>Subject ID</th>.+<th>Treatment</th>.+<th>Sex</th>.+"
      )
      
    })

test_that("A warning is generated if column names are not correctly specified", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          SEX = c("F", "M", "F", "M", "F"),
          stringsAsFactors = FALSE
      )
      expect_warning(dt <- getClinDT(data = data, colnames = c(TEST = "TEST")))
      
    })

test_that("A warning is generated if the old specification for non visible var is used", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          SEX = c("F", "M", "F", "M", "F"),
          stringsAsFactors = FALSE
      )
      
      # old spec
      expect_warning(
          dt <- getClinDT(
              data = data, 
              nonVisible = match("TRT", colnames(data))-1
          ),
          regex = "deprecated"
      )
      
    })

test_that("Invisible columns are not shown in the DataTables output", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          SEX = c("F", "M", "F", "M", "F"),
          stringsAsFactors = FALSE
      )
      
      expect_silent(
          dt <- getClinDT(
              data = data, 
              nonVisibleVar = "TRT"
          )
      )
      cDefs <- dt$x$options$columnDefs
      cDefsNonVisible <- sapply(cDefs, function(x) isFALSE(x$visible))
      expect_true(any(cDefsNonVisible))
      expect(cDefs[[which(cDefsNonVisible)]]$targets, 1)
      
    })

test_that("An error is generated if invisible columns are incorrectly specified", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          SEX = c("F", "M", "F", "M", "F"),
          stringsAsFactors = FALSE
      )
      
      # in case JS indices not used
      expect_warning(
          expect_error(	
              getClinDT(data = data, nonVisible = ncol(data)),
              "Javascript indexing"
          )
      )
      
    })

test_that("Percentages are correctly formatted", {
      
      expect_silent(
          dt <- getClinDT(
              data = data.frame(
                  USUBJID = seq.int(5),
                  perc = c(0.06, 0.001, 0.65, 0.99, 1)
              ), 
              percVar = "perc",
          )
      )
      cDefs <- dt$x$options$columnDefs
      cDefsFmtPercentage <- sapply(cDefs, function(x) "render" %in% names(x) && grepl("formatPercentage", x$render))
      expect_true(any(cDefsFmtPercentage))
      expect(cDefs[[which(cDefsFmtPercentage)]]$targets, 1)		
      
    })

test_that("A warning is generated if the variable for barplot is not available", {
      
      data <- data.frame(
          USUBJID = as.character(1:5),
          AGE = c(20, 40, 67, 36, 50),
          stringsAsFactors = FALSE
      )	
      
      expect_warning(getClinDT(data = data, barVar = "blabla"))
      
    })

test_that("Barplot are correctly rendered within DataTables", {			
      
      data <- data.frame(
          USUBJID = as.character(1:5),
          AGE = c(20, 40, 67, 36, 50),
          stringsAsFactors = FALSE
      )
      
      # specification of variable for the bar
      expect_silent(dt <- getClinDT(data = data, barVar = "AGE"))
      rCB <- dt$x$options$rowCallback
      expect_match(object = rCB, regex = ".*color.*")
      expect_match(object = rCB, regexp = "data[1]", fixed = TRUE)
      
    })

test_that("A threshold for the barplot is correctly set", {			
      
      data <- data.frame(
          USUBJID = as.character(1:5),
          AGE = c(20, 40, 67, 36, 50),
          stringsAsFactors = FALSE
      )
      
      expect_silent(dt <- getClinDT(data = data, barVar = "AGE", barColorThr = 28))
      rCB <- dt$x$options$rowCallback
      expect_match(object = rCB, regex = ".*data\\[1\\].*28.*")
      
    })

test_that("A range for the barplot is correctly set", {			
      
      data <- data.frame(
          USUBJID = as.character(1:5),
          AGE = c(20, 40, 67, 36, 50),
          stringsAsFactors = FALSE
      )
      
      # specification of range for the bar
      expect_silent(dt <- getClinDT(data = data, barVar = "AGE", barRange = c(0, 100)))
      rCB <- dt$x$options$rowCallback
      expect_match(object = rCB, regex = "100")
      
    })

test_that("A range for the barplots of multiple variables is correctly set", {			
      
      data <- data.frame(
          USUBJID = as.character(1:5),
          AGE = c(20, 40, 67, 36, 50),
          WEIGHTBL = c(60, 45, 89, 120, 78),
          stringsAsFactors = FALSE
      )
      
      # multiple variables
      expect_silent(
          dt <- getClinDT(
              data = data, 
              barVar = c("AGE", "WEIGHTBL"),
              barColorThr = c(AGE = 50),
              barRange = list(AGE = c(0, 100), WEIGHTBL = range(data$WEIGHTBL))
          )
      )
      rCB <- dt$x$options$rowCallback
      expect_match(object = rCB, regex = ".*data\\[1\\].*data\\[2\\]")
      
    })

test_that("A warning is generated is a variable for a barplot is not numeric", {			
      
      data <- data.frame(USUBJID = as.character(1:5))
      expect_warning(dt <- getClinDT(data = data, barVar = "USUBJID"))
      
    })

test_that("The location of the filter box is correctly set", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE	
      )
      
      filters <- c("top", "none")
      for(filter in filters){
        
        dt <- getClinDT(data, filter = filter)
        tableFilter <- dt$x$filter
        expect_identical(tableFilter, filter)
        
      }
      
    })

test_that("The search box is correctly enabled or disabled in the DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE	
      )
      
      for(includeSearchBox in c(TRUE, FALSE)){		
        
        dt <- getClinDT(data, searchBox = includeSearchBox)
        expect_equal(grepl("f", dt$x$options$dom), includeSearchBox)
        
      }
      
    })

test_that("The number of records (page length) is correctly set", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE	
      )
      
      dt <- getClinDT(data, pageLength = 2)
      expect_equal(dt$x$options$pageLength, 2) # page length properly set
      expect_true(grepl("p", dt$x$options$dom)) # include pagination control
      
    })

test_that("Fixed columns are correctly specified in the DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE	
      )
      
      fC <- list(leftColumns = 2)
      dt <- getClinDT(data, fixedColumns = fC)
      expect_equal(dt$x$options$fixedColumns, fC)
      
    })

test_that("Columns widths are correctly specified in the DataTables", {	
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      widths <- c(3, 7)
      dt <- getClinDT(data, columnsWidth = widths)
      
      # extract column defs
      cDefs <- dt$x$options$columnDefs
      cDefs <- unlist(cDefs, recursive = FALSE)
      
      idxColDefs <- which(sapply(cDefs, function(x) "columnsWidth" %in% names(x)))
      cDefs <- cDefs[idxColDefs]
      
      cDefsDf <- do.call(rbind.data.frame, cDefs)
      
      # check if match specified width:
      expect_equal(
          object = cDefsDf[which(cDefsDf$targets == 1), "columnsWidth"],
          expected = 3
      )
      expect_equal(
          object = cDefsDf[which(cDefsDf$targets == 2), "columnsWidth"],
          expected = 7
      )
      
    })

test_that("Extra options with default values are correctly overwritten when they are specified", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      # specified options overwrites the default (with message)
      expect_message(
          dt <- getClinDT(data, options = list(pageLength = 20)),
          regexp = "overwrites the default"
      )
      expect_equal(dt$x$options$pageLength, 20) # page length properly set
      
    })

test_that("Extra options without default values are correctly set", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      expect_silent(dt <- getClinDT(data, options = list(lengthChange = 20)))
      expect_equal(dt$x$options[["lengthChange"]], 20)
      
      # Note: wrong 'options' are handled by the JS DataTable library
      
    })

test_that("A single expanded variable is correctly handled in the DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE	
      )
      
      # Expand mechanism done in Javascript
      # so check at least than the HTML is well specified
      
      # test in combination of colnames
      expandVar <- "USUBJID"
      colnames <- c(
          "Subject ID" = "USUBJID",
          "Treatment" = "TRT"
      )
      
      expect_silent(dt <- getClinDT(data, expandVar = expandVar, colnames = colnames))
      cDefs <- dt$x$options$columnDefs
      
      # target columns are hidden
      idxColsHidden <- which(sapply(cDefs, function(x) "visible" %in% names(x) && !isTRUE(x$visible)))
      expect_length(idxColsHidden, 1)
      expect_setequal(object = idxColsHidden, expected = match(expandVar, colnames(data)))
      
      # and correct variables specified in JS callback
      expandVarLab <- names(colnames)[match(expandVar, colnames)]
      expect_true(all(sapply(expandVarLab, grepl, dt$x$callback, fixed = TRUE)))
      
    })

test_that("A warning is generated if a expanded variable is not available", {
      
      data <- data.frame(USUBJID = 1:5)
      expect_warning(dt <- getClinDT(data, expandVar = "blabla"))
      
    })


test_that("Multiple column specifications at once are correctly handled in the DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          SEX = c("F", "M", "F", "M", "F"),
          AGE = c(20, 40, 67, 36, 50),
          stringsAsFactors = FALSE
      )		
      
      dataEscape <- cbind(
          url = sprintf('<a href="customlink" target="_blank">%s</a>', data$USUBJID),
          data,
          stringsAsFactors = FALSE
      )
      
      expandVar <- c("url", colnames(data)[c(1, ncol(data))])
      nonVisibleVar <- colnames(data)[3]
      barVar <- "AGE"
      cAlignLeft <- 3 # 6th column
      
      expect_silent(dt <- 
              getClinDT(
                  data = dataEscape, 
                  expandVar = expandVar, 
                  escape = -1,
                  barVar = barVar,
                  nonVisibleVar = nonVisibleVar,
                  options = list(columnDefs = list(
                          list(className = "dt-left", targets = cAlignLeft)			
                      ))
              )
      )
      cDefs <- dt$x$options$columnDefs
      
      # escape (first column is the '+')
      expect_identical(dt$x$data[[2]], dataEscape[, "url"])
      
      # bar (+1: extra col, -1: JS)
      expect_true(grepl("color", dt$x$options$rowCallback))
      
      # non visible vars: expand and non-visible (+1: extra col, -1: JS)
      idxNonVisible <- match(c(nonVisibleVar, expandVar), colnames(dataEscape))
      idxCDefsNonVisible <- which(sapply(cDefs, function(x) 
                "visible" %in% names(x) && !x$visible
          ))
      expect_setequal(
          cDefs[[idxCDefsNonVisible]]$targets,
          expected = idxNonVisible
      )
      
      # extra column defs
      idxCDefsCtrl <- which(sapply(cDefs, function(x) 
                "className" %in% names(x) && x$className == "dt-left"
          ))
      expect_setequal(
          cDefs[[idxCDefsCtrl]]$targets,
          expected = cAlignLeft+1 # +1: one extra column added
      )
      
    })


test_that("Cells can be correctly expanded in DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          SEX = c("F", "M", "F", "M", "F"),
          stringsAsFactors = FALSE
      )			
      
      # Expand mechanism done in Javascript
      # so check at least than the HTML is well specified
      
      # correct spec
      idxRow <- c(1, 2)
      idxCol <- c(2, 2)
      expandIdx <- cbind(row = idxRow, col = idxCol)
      expect_silent(dt <- getClinDT(data, expandIdx = expandIdx))
      cDefs <- dt$x$options$columnDefs
      
      # target columns are hidden
      idxCDefsNonVisible <- which(sapply(cDefs, function(x) 
                "visible" %in% names(x) && !x$visible
          ))
      expect_silent(cColsHidden <- cDefs[[idxCDefsNonVisible]]$targets)
      expect_setequal(object = cColsHidden, expected = idxCol)
      
      # check that values are correct
      dtData <- dt$x$data
      expandIdxCol <- unique(idxCol)
      # column displayed
      expect_identical(
          object = dtData[[expandIdxCol]][-idxRow], 
          expected = data[-idxRow, expandIdxCol]
      )
      # hidden column
      expect_identical(
          object = as.character(dtData[[expandIdxCol+1]][idxRow]), 
          expected = data[idxRow, expandIdxCol]
      ) 
      
    })

test_that("An error is generated if the specification of cells to expand is incorrect", {
      
      data <- data.frame(USUBJID = 1:5)
      expect_error(dt <- getClinDT(data, expandIdx = list()))
      
    })

test_that("HTML is correctly escaped by default in the DataTables", {
      
      dataEscape <- data.frame(
          url = sprintf('<a href="customlink" target="_blank">%s</a>', 1:5),
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      expect_silent(dt <- getClinDT(dataEscape))
      expect_match(attr(dt$x$options, "escapeIdx"), regexp = "1,2,3")
      
    })

test_that("HTML is correctly escaped when specified for all columns at once in the DataTables", {
      
      dataEscape <- data.frame(
          url = sprintf('<a href="customlink" target="_blank">%s</a>', 1:5),
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      # as logical
      expect_silent(dt <- getClinDT(dataEscape, escape = FALSE))
      expect_false(grepl("1,2,3", attr(dt$x$options, "escapeIdx")))
      
      # wrong specification
      expect_error(
          getClinDT(dataEscape, escape = c(TRUE, FALSE)),
          "logical.*of length 1"
      )
      
    })

test_that("HTML is correctly not escaped for a specific column in the DataTables", {
      
      dataEscape <- data.frame(
          url = sprintf('<a href="customlink" target="_blank">%s</a>', 1:5),
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      # as negative integer
      expect_silent(dt <- getClinDT(dataEscape, escape = -1))
      expect_false(grepl("1", attr(dt$x$options, "escapeIdx")))
      
    })

test_that("An error is generated when the column to escape is not available", {
      
      dataEscape <- data.frame(
          url = sprintf('<a href="customlink" target="_blank">%s</a>', 1:5),
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      # wrong spec
      expect_error(
          dt <- getClinDT(dataEscape, escape = "blabla"),
          "not found in data"
      )
      expect_error(
          dt <- getClinDT(dataEscape, escape = ncol(dataEscape) * 2),
          "columns not in data"
      )
      
    })

test_that("Row grouping is correctly handled in the DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          STUDYID = rep("study1", each = 5),
          SITEID = c("a", "b", "c", "a", "c"),
          stringsAsFactors = FALSE
      )	
      
      # Note: done in JS
      
      rowGroupVars <- c("STUDYID", "SITEID")
      expect_silent(dt <- getClinDT(data, rowGroupVar = rowGroupVars))	
      expect_equal(
          object = dt$x$options$rowGroup$dataSrc, 
          expected = match(rowGroupVars, colnames(data))-1
      )
      
    })

test_that("A warning is generated when the variable to consider for the row grouping is not available", {
      
      data <- data.frame(USUBJID = 1:5)	
      
      # variable not available in the data
      expect_warning(
          dt <- getClinDT(data, rowGroupVar = "blabla"),
          "not available in the data."
      )
      
    })

test_that("A warning is generated when old specification for row grouping is used", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          STUDYID = rep("study1", each = 5),
          SITEID = c("a", "b", "c", "a", "c"),
          stringsAsFactors = FALSE
      )	
      
      rowGroupVars <- c("STUDYID", "SITEID")
      
      # old spec
      expect_warning(
          dt <- getClinDT(data, rowGroup = rowGroupVars),
          "deprecated"
      )
      expect_equal(
          object = dt$x$options$rowGroup$dataSrc, 
          expected = match(rowGroupVars, colnames(data))-1
      )
      
    })

test_that("Vertical alignment can be set in the DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )	
      
      expect_silent(dt <- getClinDT(data, vAlign = "bottom"))
      expect_true(grepl("'vertical-align':'bottom'", dt$x$options$rowCallback))
      
    })

test_that("A custom call back Javascript function can be set in the DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      callback <- htmlwidgets::JS("testCallback")
      expect_silent(dt <- getClinDT(data, callback = callback))
      expect_true(grepl("testCallback", dt$x$callback))
      
    })

test_that("Buttons can be correctly specified in the DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      # specify buttons
      buttons <- "copy"
      expect_silent(dt <- getClinDT(data, buttons = buttons))
      expect_setequal(dt$x$options$buttons, buttons)
      expect_true(grepl("B", dt$x$options$dom)) # in DOM
      expect_true("Buttons" %in% dt$x$extensions) # Js extension specified
      
    })

test_that("Buttons can be unset in the DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      expect_silent(dt <- getClinDT(data, buttons = NULL))
      expect_length(dt$x$options$buttons, 0)
      expect_false(grepl("B", dt$x$options$dom))
      expect_false("Buttons" %in% dt$x$extensions)
      
      # Note: if uncorrect button specified -> JS error
      
    })

test_that("Scrolling along the x-axis can be correctly set in the DataTables", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      for(scrollX in c(TRUE, FALSE)){		
        
        dt <- getClinDT(data, scrollX = scrollX)
        expect_equal(dt$x$options$scrollX, scrollX)
        
      }
      
    })

test_that("Additional DataTables options can be passed to the DataTables outside of the 'options' parameter", {
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      # internal default
      expect_warning(getClinDT(data, rownames = TRUE))
      
      # extra parameter
      width <- "10px"
      expect_silent(dt <- getClinDT(data, width = width))
      expect_equal(dt$width, width)
      
    })

test_that("DataTables can be exported to a file", {
      
      skip_on_cran()  
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      # correct file format
      file <- tempfile(fileext = ".html")
      expect_silent(dt <- getClinDT(data, file = file))
      expect_true(file.exists(file))
      
    })

test_that("An error is generated if the file to export to has an incorrect extension", {
      
      skip_on_cran()
      
      data <- data.frame(
          USUBJID = 1:5,
          TRT = c("A", "A", "B", "B", "B"),
          stringsAsFactors = FALSE
      )
      
      # incorrect file format
      file <- tempfile(fileext = ".csv")
      expect_error(
          dt <- getClinDT(data, file = file),
          pattern = "extension"
      )
      
    })