context("Format labels")

label1 <- "oneString"
label2 <- "secondString"

test_that("Strings are correctly formatted", {
      
      expect_identical(formatLabel(label1), label1)
      
      formattedLabels <- formatLabel(label1, label2)
      
      expect_identical(
          formattedLabels,
          sprintf("%s-%s", label1, label2)
      )
      
    })

test_that("Labels from data frame are correctly formatted", {
      
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

test_that("Labels are correctly formatted for chunks", {
      
      formattedLabelChunk <- formatLabelChunk(label1, label2)
      
      expect_identical(
          formattedLabelChunk,
          sprintf("%s-%s", label1, label2)
      )
      
    })


test_that("Table labels are correctly formatted", {
      
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


