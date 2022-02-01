# Create standard palette for CDISC variable
# 
# Author: Laure Cougnaud
###############################################################################

shapePaletteNRIND <- c(
	LOW = 25, # triangle down filled
	NORMAL = 21, # circle filled
	HIGH = 24, # triangle filled
	ABNORMAL = 18, # diamond
	UNKNOWN = 3, # cross
	'NA' = 3 # cross
)

save(shapePaletteNRIND, version = 2, file = "../data/shapePaletteNRIND.RData")

colorPaletteNRIND <- c(
	LOW = "orange", 
	NORMAL = "green4",
	HIGH = "orange",
	ABNORMAL = "red",
	UNKNOWN = "grey",
	'NA' = "grey"
)
save(colorPaletteNRIND, version = 2, file = "../data/colorPaletteNRIND.RData")
