context("Comparison of data.frames")

test_that("no change is detected if data.frame are the same", {
      
      newData <- oldData <- data.frame(id = 1, a = 1, b = 2)	
      diffTable <- compareTables(
          newData, oldData, referenceVars = "id", 
          changeableVars = c("a", "b"), outputType = "table-comparison"
      )
      expect_s3_class(diffTable, "data.frame")
      expect_named(diffTable, c("Comparison type", "Version", c("id", "a", "b")))
      expect_equal(nrow(diffTable), 0)
      
    })

test_that("addition is correctly detected", {
      
      newData <- data.frame(id = 1, a = 1, b = 2)
      oldData <- data.frame(id = numeric(), a = numeric(), b = numeric())
      diffTable <- compareTables(
          newData, oldData, referenceVars = "id", 
          changeableVars = c("a", "b"), outputType = "table-comparison"
      )
      expect_s3_class(diffTable, "data.frame")
      diffTableRef <- data.frame(
          `Comparison type` = "Addition", Version = "Current", 
          id = 1, a = 1, b = 2,
          check.names = FALSE,
          stringsAsFactors = TRUE
      )
      expect_equal(diffTable, diffTableRef, check.attributes = FALSE)
      
    })

test_that("removal is correctly detected", {
      
      newData <- data.frame(id = numeric(), a = numeric(), b = numeric())
      oldData <- data.frame(id = 1, a = 1, b = 2)
      diffTable <- compareTables(
          newData, oldData, referenceVars = "id", 
          changeableVars = c("a", "b"), outputType = "table-comparison"
      )
      expect_s3_class(diffTable, "data.frame")
      diffTableRef <- data.frame(
          `Comparison type` = factor("Removal", 
              levels = c("Addition", "Change", "Removal")
          ),
          Version = factor("Previous", levels = c("Current", "Previous")),
          id = 1, a = 1, b = 2,
          check.names = FALSE
      )
      expect_equal(diffTable, diffTableRef, check.attributes = FALSE)
      
    })

test_that("change is correctly detected", {
      
      newData <- data.frame(id = 1, a = 1, b = 3)
      oldData <- data.frame(id = 1, a = 1, b = 2)
      diffTable <- compareTables(
          newData, oldData, referenceVars = "id", 
          changeableVars = c("a", "b"), outputType = "table-comparison"
      )
      expect_s3_class(diffTable, "data.frame")
      diffTableRef <- rbind(
          cbind(
              data.frame(`Comparison type` = "Change", Version = "Current", check.names = FALSE,  stringsAsFactors = TRUE), 
              newData
          ),
          cbind(
              data.frame(`Comparison type` = "Change", Version = "Previous", check.names = FALSE,  stringsAsFactors = TRUE), 
              oldData
          )
      )
      diffTableRef$`Comparison type` <- factor(diffTableRef$`Comparison type`,
          levels = c("Addition", "Change", "Removal"))
      expect_equal(diffTable, diffTableRef, check.attributes = FALSE)
      
    })

test_that("comparison with duplicated records in newData is successful", {
      
      newData <- data.frame(id = rep(1, 2), a = c(1, 1), b = c(2, 3))		
      oldData <- data.frame(id = 1, a = 1, b = 2)
      diffTable <- compareTables(
          newData, oldData, referenceVars = "id", 
          changeableVars = c("a", "b"), outputType = "table-comparison"
      )
      expect_s3_class(diffTable, "data.frame")
      diffTableRef <- data.frame(
          `Comparison type` = "Addition", Version = "Current", 
          id = 1, a = 1, b = 3,
          check.names = FALSE,
          stringsAsFactors = TRUE
      )
      expect_equal(diffTable, diffTableRef, check.attributes = FALSE)
      
    })

test_that("error if specified columns are not in data", {
      
      #newData <- oldData <- data.frame(id = 1, a = 1, b = 2)	
      expect_error(
          diffTable <- compareTables(
              data.frame(), data.frame(), referenceVars = "id", 
              changeableVars = c("a", "b"), 
              outputType = "table-comparison"
          ),
          regexp = "Some items of .+ are not column names: .+"
      )
      
    })

test_that("all columns are considered as reference if referenceVars is not specified", {
      
      newData <- data.frame(id = 1, a = 2, b = 3, d = 5)
      oldData <- data.frame(id = 1, a = 3, b = 2, e = 10)
      expect_equal(
          compareTables(newData, oldData),
          compareTables(newData, oldData, referenceVars = c("id", "a", "b"))
      )
      
    })


test_that("a version is correctly extracted when the tables are compared", {
      
      newData <- data.frame(id = 1, a = 2, b = 3, d = 5)
      oldData <- data.frame(id = 1, a = 3, b = 2, e = 10)
      expect_silent(
          res <- compareDiff(
              newData, oldData, 
              referenceVars = intersect(colnames(newData), colnames(oldData))
          )
      )
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 2)
      expect_equal(res$Version, factor(c("Current", "Previous")))
      
    })

test_that("old or new data should be specified when merging with diff data", {
      
      newData <- data.frame(id = 1, a = 2, b = 3, d = 5)
      oldData <- data.frame(id = 1, a = 3, b = 2, e = 10)
      
      diffData <- compareDiff(
          newData, oldData, 
          referenceVars = intersect(colnames(newData), colnames(oldData)), 
      )
      
      expect_error(mergeDiffWithData(diffData = diffData))
      
    })

test_that("comparison type terms of diff data should be among a standard set during merging", {
      
      newData <- data.frame(id = 1, a = 2)
      oldData <- data.frame(id = 1, a = 3)
      
      diffData <- data.frame(
          `Comparison type` = "Different", 
          Version = c("Current"),
          id = 1, a = 2,
          check.names = FALSE
      )
      attr(diffData, "referenceVars") <- "id"
      attr(diffData, "changeableVars") <- "a"
      
      expect_error(
          mergeDiffWithData(diffData = diffData, newData = newData),
          "Comparison type should be among.*"
      )
      
    })

test_that("diff data object is merged correctly with new data", {
      
      newData <- data.frame(id = 1, a = 2, b = 3, d = 5)
      oldData <- data.frame(id = 1, a = 3, b = 2, e = 10)
      
      diffData <- compareDiff(
          newData, oldData, 
          referenceVars = intersect(colnames(newData), colnames(oldData)), 
      )
      
      expect_silent(
          res <- mergeDiffWithData(
              diffData = diffData, newData = newData,
          )
      )
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), nrow(newData))
      expect_equal(res[, - c(1, 2)], newData)
      
    })

test_that("error when 'Comparison type' and 'Version' are in new data", {
      
      newData <- data.frame(id = 1, a = 2, b = 3, d = 5)
      oldData <- data.frame(id = 1, a = 3, b = 2, e = 10)
      diffData <- compareDiff(
          newData, oldData, 
          referenceVars = intersect(colnames(newData), colnames(oldData))
      )
      newData$`Comparison type` <- "Addition"
      newData$Version <- "Current"
      
      expect_error(
          mergeDiffWithData(
              diffData = diffData, newData = newData,
          ),
          ".+ are reserved names for diff data, so shouldn't be available in new data."
      )
      
    })

test_that("diff data object is merged with old data", {
      
      newData <- data.frame(id = 1, a = 2, b = 3, d = 5)
      oldData <- data.frame(id = 1, a = 3, b = 2, e = 10)
      
      diffData <- compareDiff(
          newData, oldData, 
          referenceVars = intersect(colnames(newData), colnames(oldData)), 
      )
      
      expect_silent(
          res <- mergeDiffWithData(
              diffData = diffData, oldData = oldData,
          )
      )
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), nrow(oldData))
      expect_equal(res[, - c(1, 2)], oldData)
      
    })

test_that("error if export type is not available", {
      
      newData <- data.frame(id = 1, a = 2, b = 3, d = 5)
      oldData <- data.frame(id = 1, a = 3, b = 2, e = 10)
      
      diffData <- compareDiff(
          newData, oldData, 
          referenceVars = intersect(colnames(newData), colnames(oldData)), 
      )
      
      expect_error(
          exportDiffData(diffData, to = "dt"),
          "'arg' should be one of .+"
      )
      
    })

test_that("diff data is exported to an interactive table", {
      
      newData <- data.frame(id = 1, a = 2, b = 3, d = 5)
      oldData <- data.frame(id = 1, a = 3, b = 2, e = 10)
      
      diffData <- compareDiff(
          newData, oldData, 
          referenceVars = intersect(colnames(newData), colnames(oldData)), 
      )
      
      expect_silent(
          res <- exportDiffData(diffData)
      )
      expect_is(res, "datatables")
      
      expect_equal(
          res$x$data[, colnames(diffData)],
          diffData,
          check.attributes = FALSE
      )
      
    })

test_that("difference in new data is exported to an interactive table", {
      
      newData <- data.frame(id = 1, a = 2, b = 3, d = 5)
      oldData <- data.frame(id = 1, a = 3, b = 2, e = 10)
      
      diffData <- compareDiff(
          newData, oldData, 
          referenceVars = intersect(colnames(newData), colnames(oldData)), 
      )
      newDataDiff <- mergeDiffWithData(
          diffData = diffData, newData = newData,
      )
      
      expect_silent(
          res <- exportDiffData(diffData, newDataDiff = newDataDiff)
      )
      expect_is(res, "datatables")
      
      expect_equal(
          res$x$data[, colnames(newDataDiff)],
          newDataDiff,
          check.attributes = FALSE
      )
    })

test_that("difference in old data is exported to an interactive table", {
      
      newData <- rbind(
          data.frame(id = 1, a = 2, b = 4),
          data.frame(id = 1, a = 3, b = 2)
      )
      oldData <- rbind(
          data.frame(id = 1, a = 2, b = 4), # identity
          data.frame(id = 1, a = 3, b = 5), # change
          data.frame(id = 1, a = 4, b = 2) # removal
      )
      
      expect_silent(
          res <- compareTables(
              newData, oldData, 
              referenceVars = c("id", "a"),
              changeableVars = "b",
              outputType = "oldData-diff-interactive"
          )
      )
      expect_s3_class(res, "datatables")
      
      # identical records correctly tagged
      expect_setequal(
          subset(res$x$data, a == 2)[, c("id.diff", "a.diff", "b.diff")],
          "=="
      )
      # removed records correctly tagged
      expect_setequal(
          subset(res$x$data, a == 4)[, c("id.diff", "a.diff", "b.diff")],
          "-"
      )
      # changed records correctly tagged
      expect_setequal(
          subset(res$x$data, a == 3)[, c("id.diff", "a.diff")],
          "=="
      )
      expect_setequal(
          subset(res$x$data, a == 3)[, c("b.diff")],
          "!="
      )
      
    })

test_that("a placeholder is displayed in the interactive table if there is no difference between the new and old data", {
      
      data <- data.frame(id = 1, a = 2, b = 3, d = 5)
      
      diffData <- compareDiff(
          newData = data, oldData = data, 
          referenceVars = "id"
      )
      
      expect_silent(
          res <- exportDiffData(diffData)
      )
      expect_is(res, "datatables")
      expect_equal(
          res$x$options$language$zeroRecords,
          "There is no difference between the previous and the current data."
      )
      
    })

test_that("column names are specified in the interactive diff data table", {
      
      newData <- data.frame(id = 1, a = 2, b = 3, d = 5)
      oldData <- data.frame(id = 1, a = 3, b = 2, e = 10)
      
      diffData <- compareDiff(
          newData, oldData, 
          referenceVars = intersect(colnames(newData), colnames(oldData)), 
      )
      
      expect_silent(
          res <- exportDiffData(
              diffData, 
              colnames = c(`Variable a` = "a", `Variable b` = "b")
          )
      )
      expect_equal(
          attr(res$x, "colnames")[match(c("a", "b"), colnames(res$x$data))], 
          c("Variable a", "Variable b")
      )
      
    })