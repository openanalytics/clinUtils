\dontrun{

# Note: the following code should be included 
# within a chunk of a knitr (e.g. RMarkdown) document
# to include a list of figures in the Rmarkdown output
data(iris)

## Static plots

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
# with caption for each figure
knitPrintListPlots(
	plotsList = plotsListStatic, 
	fig.cap = names(plotsListStatic)
)
	
# specify dimension for each figure
knitPrintListPlots(
	plotsList = plotsListStatic, 
	# first plot has width of 3, second of 6
	fig.width = c(3, 6), 
	# both plots have a height of 6
	fig.height = 6
)

## Interactive plots

library(plotly)
plotsListInteractive <- list(
	point = plot_ly(data = cars, x = ~speed, y = ~dist, type = "scatter", mode = "marker"),
	line = plot_ly(data = cars, x = ~speed, y = ~dist, type = "scatter", mode = "line")
)

# with titles
knitPrintListPlots(
	plotsList = plotsListInteractive, 
	type = "plotly", 
	titles = names(plotsListInteractive), 
	titleLevel = 3
)

}