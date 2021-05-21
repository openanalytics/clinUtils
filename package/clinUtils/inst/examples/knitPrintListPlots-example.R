\dontrun{

# Note: the following code should be included within a chunk of a knitr (e.g. RMarkdown) document
# to include/export a list of figures in the Rmarkdown output
data(iris)

# include a list of static plots
library(ggplot2)
plotsListStatic <- list(
	point = ggplot(data = cars, aes(x = speed, y = dist)) + geom_point(),
	line = ggplot(data = cars, aes(x = speed, y = dist)) + geom_line()
)
# with general label (used to name exported figure)
knitPrintListPlots(
  plotsList = plotsListStatic, 
  generalLabel = "scatter-cars"
)
# with label for each plot (used to name exported figure)
knitPrintListPlots(
  plotsList = plotsListStatic, 
  labels = names(plotsListStatic)
)
# with section header (header of level 1 in Markdown)
knitPrintListPlots(
  plotsList = plotsListStatic, 
  titles = names(plotsListStatic), 
  titleLevel = 3
)
# specify caption and dimension for each figure (figure height is replicated)
knitPrintListPlots(
  plotsList = plotsListStatic, 
  fig.cap = names(plotsListStatic), 
  fig.width = 3*seq_along(plotsListStatic), 
  fig.height = 6
)

# include a list of interactive plots
library(plotly)
plotsListInteractive <- list(
	point = plot_ly(data = cars, x = ~speed, y = ~dist, type = "scatter", mode = "marker"),
	line = plot_ly(data = cars, x = ~speed, y = ~dist, type = "scatter", mode = "line")
)
knitPrintListPlots(
 plotsList = plotsListInteractive, 
  type = "plotly", 
  titles = names(plotsListInteractive), 
  titleLevel = 3
)

}