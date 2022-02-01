\dontrun{
	
# Note: the following code should be included 
# within a chunk of a knitr (e.g. RMarkdown) document
# to include a list of objects in the Rmarkdown output

# list of flextable objects
library(flextable)
listTables <- list(flextable(iris), flextable(cars))
knitPrintListObjects(
	xList = listTables,
	titles = c("Iris dataset", "Cars dataset")
)

}