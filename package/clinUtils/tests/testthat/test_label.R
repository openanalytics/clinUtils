context("Test labels")

label1 <- "oneString"
label2 <- "secondString"

test_that("Formatting label with strings", {
      
      expect_identical(formatLabel(label1), label1)
      
      formattedLabels <- formatLabel(label1, label2)
      
      expect_identical(
          formattedLabels,
          sprintf("%s-%s", label1, label2)
      )
      
    })

test_that("Formatting label with data frames", {
      
      df1 <- data.frame(col1 = label1, stringsAsFactors = FALSE)
      expect_identical(formatLabel(df1), label1)
      
      df2 <- data.frame(col1 = label2, stringsAsFactors = FALSE)
      formattedLabels <- formatLabel(df1, df2)
      
      expect_identical(
          formattedLabels,
          sprintf("%s-%s", label1, label2)
      )
      
      expect_error(
          formatLabel(rbind(df1, df2))
      )
      
    })

test_that("Formatting label for chunks", {
      
      formattedLabelChunk <- formatLabelChunk(label1, label2)
      
      expect_identical(
          formattedLabelChunk,
          sprintf("%s-%s", label1, label2)
      )
      
    })


test_that("Formatting label for tables", {
      
      formatTableLabel1 <- formatTableLabel(label1)
      
      expect_identical(
          formatTableLabel1,
          paste0("(\\#tab:", label1, ")")
      )
      
      formatTableLabel2 <- formatTableLabel(label1, label2)
      
      expect_identical(
          formatTableLabel2,
          paste0("(\\#tab:", sprintf("%s-%s", label1, label2), ")")
      )
      
    })


