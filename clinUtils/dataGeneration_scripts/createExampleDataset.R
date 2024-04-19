# Create example datasets for the package
#
# A subset of the CDISC Pilot 01 SDTM dataset, available from:
# https://github.com/phuse-org/phuse-scripts/tree/master/data/[sdtm|adam]/cdisc/
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

## SDTM Datasets

# Import the data
sdtmPath <- "~/git/phuse-scripts/data/sdtm/cdiscpilot01"
dataSDTMPath <- list.files(pattern = "^(dm|ae|lb|cm|mh|ex|sv|vs|ds|qs|suppdm).xpt$", path = sdtmPath, full.names = TRUE)
dataSDTMCDISCP01 <- loadDataADaMSDTM(files = dataSDTMPath)
labelVars <- attr(dataSDTMCDISCP01, "labelVars")

# Subset only patients of interest
dataSDTMCDISCP01 <- sapply(dataSDTMCDISCP01, function(data){
	dataSubset <- subset(data, USUBJID %in% subjects)
	rownames(dataSubset) <- NULL
	dataSubset
}, simplify = FALSE)
dataSDTMCDISCP01[["QS"]] <- subset(
  dataSDTMCDISCP01[["QS"]], 
  subset = QSTESTCD %in% c("ACTOT", "CIBIC", "NPTOT")
)
attr(dataSDTMCDISCP01, "labelVars") <- labelVars

# Export the data
save(dataSDTMCDISCP01, version = 2, file = "../data/dataSDTMCDISCP01.RData")

# check
# dataSDTMCDISCP01New <- dataSDTMCDISCP01
# data("dataSDTMCDISCP01", package = "clinUtils")
# all.equal(
#   structure(dataSDTMCDISCP01New[names(dataSDTMCDISCP01)], labelVars = NULL),
#   structure(dataSDTMCDISCP01, labelVars = NULL)
# )

## ADaM Datasets

# Import the data
adamPath <- "~/git/phuse-scripts/data/adam/cdisc"
dataADaMPath <- list.files(
	pattern = "^(adsl|adae|adlbc|advs|adcm|adqsadas|adqscibc|adpp|adqsnpix).xpt$", 
	path = adamPath, full.names = TRUE
)
dataADaMCDISCP01 <- loadDataADaMSDTM(files = dataADaMPath)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

# Subset only patients of interest
subjectsPP <- c("Study001_1001001", "Study001_1001002", "Study001_1001003", "Study001_1001004" )
dataADaMCDISCP01 <- sapply(names(dataADaMCDISCP01), function(dataset){
	data <- dataADaMCDISCP01[[dataset]]
	if(dataset == "ADPP"){
		dataSubset <- subset(data, USUBJID %in% subjectsPP)
	}else{
		dataSubset <- subset(data, USUBJID %in% subjects)
	}
	rownames(dataSubset) <- NULL
	dataSubset
}, simplify = FALSE)
attr(dataADaMCDISCP01, "labelVars") <- labelVars

# Export the data
save(dataADaMCDISCP01, version = 2, file = "../data/dataADaMCDISCP01.RData")

# check
# dataADaMCDISCP01New <- dataADaMCDISCP01
# data("dataADaMCDISCP01", package = "clinUtils")
# all.equal(
#   structure(dataADaMCDISCP01New[names(dataADaMCDISCP01)], labelVars = NULL),
#   structure(dataADaMCDISCP01, labelVars = NULL)
# )
