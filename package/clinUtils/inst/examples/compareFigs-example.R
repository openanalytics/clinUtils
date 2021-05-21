\dontrun{

# ImageMagick should be installed and available
data(dataADaMCDISCP01)

library(ggplot2)

dataLB <- dataADaMCDISCP01$ADLBC

# default ggplot2 behaviour
pathFigRef <- "compareFigs_ref"
dir.create(pathFigRef)
gg1 <- ggplot(data = subset(dataLB, PARAMCD == "CHOL"), 
	aes(x = ADY, y = CHG, color = SUBJID)) +
	geom_line(aes(linetype = SUBJID)) +
	geom_point(aes(shape = SUBJID)) +
	theme(legend.position = "bottom") +
	ggtitle("Patient profiles")
ggsave(filename = file.path(pathFigRef, "example1.png"), plot = gg1)
ggsave(filename = file.path(pathFigRef, "example2.png"), plot = gg1)
ggsave(filename = file.path(pathFigRef, "example3.png"), plot = gg1)

# use default-color, and linetype/shape palette with enough elements:
pathFigNew <- "compareFigs_new"
dir.create(pathFigNew)
ggsave(
	filename = file.path(pathFigNew, "example1.png"), 
	plot = gg1
)
ggsave(
	filename = file.path(pathFigNew, "example2.png"), 
	plot = gg1 + ggtitle("This is a new title")
)
ggsave(
	filename = file.path(pathFigNew, "example3.png"), 
	plot = gg1 + scale_color_manual(values = getColorPalette(x = dataLB$SUBJID))
)

compareFigs(pathFigRef = pathFigRef, pathFigNew = pathFigNew)

}
