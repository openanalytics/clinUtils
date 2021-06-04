context("Palettes")

library(ggplot2)
library(datasets)
library(viridisLite)
library(grDevices)

test_that("Palette is extracted for the correct number of elements", {
      
      expect_length(getColorPalette(n = 5), 5)
      
    })

test_that("Error is returned if the number of elements is missing or empty", {
      
      expect_error(getColorPalette())  
      expect_error(getColorPalette(n = NA_integer_))
      expect_error(getColorPalette(n = integer()))
      
      expect_error(getShapePalette()) 
      expect_error(getShapePalette(n = NA_integer_))
      expect_error(getShapePalette(n = integer()))
      
      expect_error(getLinetypePalette())
      expect_error(getLinetypePalette(n = NA_integer_))
      expect_error(getLinetypePalette(n = integer()))
      
    })

test_that("Error is returned if the palette is not a function or a vector", {
      
      expect_error(
          getColorPalette(n = 1, palette = data.frame(A = 1)),
          "'palette' should be a vector or a function."
      )
      
    })

test_that("Missing values in palette are extracted as specified", {
      
      xWithNA <- c(NA_character_, "group1")
      expect_false('NA' %in% names(getColorPalette(x = xWithNA)))
      expect_true('NA' %in% names(getColorPalette(x = xWithNA, includeNA = TRUE)))
      expect_false('NA' %in% names(getShapePalette(x = xWithNA)))
      expect_true('NA' %in% names(getShapePalette(x = xWithNA, includeNA = TRUE)))
      expect_false('NA' %in% names(getLinetypePalette(x = xWithNA)))
      expect_true('NA' %in% names(getLinetypePalette(x = xWithNA, includeNA = TRUE)))
      
    })

test_that("Empty values in palette are retained", {
      
      xWithEmpty <- c("", "group1")
      expect_silent(palette <- getColorPalette(x = xWithEmpty))
      expect_equal(names(palette), xWithEmpty)
      
      xWithEmpty <- c("", "group1")
      expect_silent(palette <- getShapePalette(x = xWithEmpty))
      expect_equal(names(palette), xWithEmpty)
      
      xWithEmpty <- c("", "group1")
      expect_silent(palette <- getLinetypePalette(x = xWithEmpty))
      expect_equal(names(palette), xWithEmpty)
      
    })

test_that("Combination of Unicode and standard palettes is successful",{
      
      data(iris)
      gg <- ggplot(data = iris, 
          aes(x = `Sepal.Length`, y = `Sepal.Width`, shape = Species)
      ) + geom_point() 
      
      shapePaletteText <- getShapePalette(x = iris$Species, asText = TRUE)
      shapePaletteText[1] <- '\u25C0'
      
      # Note: produce warnings in R CMD check
      gg <- gg + scale_shape_manual(values = shapePaletteText)
      expect_error(
          ggplot2::ggsave(
              gg,
              filename = file.path(tempdir(), "test.pdf"),
              device = grDevices::cairo_pdf
          ),
          NA
      )
      
    })

test_that("Palettes are successful when specifying number of elements", {
      
      shapes <- getShapePalette(n = 2)
      expect_named(shapes, NULL)
      expect_identical(
          shapes,
          as.integer(c(21, 22))
      )
      
      linetypes <- getLinetypePalette(n = 2)
      expect_named(linetypes, NULL)
      expect_identical(
          linetypes,
          c("solid", "dashed")
      )
      
    })





