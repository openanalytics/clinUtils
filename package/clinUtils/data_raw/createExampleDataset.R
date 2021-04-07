# Create example dataset for the package
#
# A subset of the CDISC Pilot 01 SDTM dataset, available from:
# https://github.com/phuse-org/phuse-scripts/tree/master/data/[sdtm|adam]/cdiscpilot01/
# is exported for demonstration purpose
#
# Author: Laure Cougnaud
###############################################################################

library(clinUtils)

extdataPath <- "../inst/extdata/cdiscpilot01/"
if(!dir.exists(extdataPath))	dir.create(extdataPath, recursive = TRUE)

subjects <- c(
	# subjects that died
	"01-704-1445", "01-710-1083", "01-701-1211",
	# serious event (not resulting in death)
	"01-718-1371",
	# UseR!2019 example for patient profiles presentation
	"01-701-1148", "01-718-1427",
	# example of patient with start of CM long before the study start
	"01-701-1192"
)

# Domains of interest: Demography, Adverse Events and Laboratory

# Subset of the SDTM Datasets

sdtmPath <- "~/git/phuse-scripts/data/sdtm/cdiscpilot01"

dataSDTMPath <- list.files(pattern = "^(dm|ae|lb|cm|mh|ex|sv|vs).xpt$", path = sdtmPath, full.names = TRUE)
dataSDTMCDISCP01 <- loadDataADaMSDTM(files = dataSDTMPath)
labelVars <- attr(dataSDTMCDISCP01, "labelVars")
dataSDTMCDISCP01 <- sapply(dataSDTMCDISCP01, subset, USUBJID %in% subjects, simplify = FALSE)
attr(dataSDTMCDISCP01, "labelVars") <- labelVars

save(dataSDTMCDISCP01, version = 2, file = "../data/dataSDTMCDISCP01.RData")

# Subset of the ADaM Datasets

adamPath <- "~/git/phuse-scripts/data/adam/cdiscpilot01"

dataADaMPath <- list.files(pattern = "^(adsl|adae|adlbc|advs).xpt$", path = adamPath, full.names = TRUE)
dataADaMCDISCP01 <- loadDataADaMSDTM(files = dataADaMPath)
labelVars <- attr(dataADaMCDISCP01, "labelVars")
dataADaMCDISCP01 <- sapply(dataADaMCDISCP01, subset, USUBJID %in% subjects, simplify = FALSE)
attr(dataADaMCDISCP01, "labelVars") <- labelVars

save(dataADaMCDISCP01, version = 2, file = "../data/dataADaMCDISCP01.RData")
