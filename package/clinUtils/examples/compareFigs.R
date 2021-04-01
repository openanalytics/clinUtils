\dontrun{

# ImageMagick should be installed and available
data(ADaMDataPelican)

library(ggplot2)

# default ggplot2 behaviour
pathFigRef <- "compareFigs_ref"
dir.create(pathFigRef)
gg1 <- ggplot(data = subset(dataLB, PARAMCD == "CRP"), 
	aes(x = ADY, y = CHG, color = SUBJID)) +
	geom_line(aes(linetype = SUBJID)) +
	geom_point(aes(shape = SUBJID)) +
	theme(legend.position = "bottom") +
	ggtitle("Patient profiles")
ggsave(filename = file.path(pathFigRef, "example1.png"), plot = gg1)
ggsave(filename = file.path(pathFigRef, "example2.png"), plot = gg1)
ggsave(filename = file.path(pathFigRef, "example3.png"), plot = gg1)

# use GLPG-color, and linetype/shape palette with enough elements:
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
	plot = gg1 + scale_color_manual(values = getGLPGColorPalette(x = dataLB$SUBJID))
)

compareFigs(pathFigRef = pathFigRef, pathFigNew = pathFigNew)

}
