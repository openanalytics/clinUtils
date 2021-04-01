# Create standard palette
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

save(shapePaletteNRIND, file = "../data/shapePaletteNRIND.RData")

glpgColors <- glpgStyle::glpgColor()
colorPaletteNRIND <- c(
	LOW = unname(glpgColors["orange"]), 
	NORMAL = unname(glpgColors["green"]),
	HIGH = unname(glpgColors["orange"]),
	ABNORMAL = unname(glpgColors["signalRed"]),
	UNKNOWN = unname(glpgColors["grey"]),
	'NA' = unname(glpgColors["grey"])
)
save(colorPaletteNRIND, file = "../data/colorPaletteNRIND.RData")
