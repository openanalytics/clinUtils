# Create example dataset for the package
#
# A subset of the CDISC Pilot 01 SDTM dataset, available from:
# https://github.com/phuse-org/phuse-scripts/tree/master/data/sdtm/cdiscpilot01/
# is exported for demonstration purpose
#
# Author: Laure Cougnaud
###############################################################################

library(haven)

sdtmPath <- "~/git/phuse-scripts/data/sdtm/cdiscpilot01"

extdataPath <- "../inst/extdata/cdiscpilot01/"
if(!dir.exists(extdataPath))	dir.create(extdataPath, recursive = TRUE)

subjects <- c(
	# subjects that died
	"01-704-1445", "01-710-1083", "01-701-1211",
	# serious event (not resulting in death)
	"01-718-1371",
	# UseR!2019 example for patient profiles presentation
	"01-701-1148", "01-718-1427" 
)

# Subset of the Demography dataset

dataDM <- read_xpt(file = file.path(sdtmPath, "dm.xpt"))
dataDMExample <- subset(dataDM, USUBJID %in% subjects)
haven::write_xpt(
	data = dataDMExample, 
	path = file.path(extdataPath, "dm.xpt")
)

# Subset of the Laboratory dataset

dataLB <- read_xpt(file = file.path(sdtmPath, "lb.xpt"))
dataLBExample <- subset(dataLB, USUBJID %in% subjects)
haven::write_xpt(
	data = dataLBExample, 
	path = file.path(extdataPath, "lb.xpt")
)

# Subset of the Adverse event dataset

dataAE <- read_xpt(file = file.path(sdtmPath, "ae.xpt"))
dataAEExample <- subset(dataAE, USUBJID %in% subjects)
haven::write_xpt(
	data = dataAEExample, 
	path = file.path(extdataPath, "ae.xpt")
)

