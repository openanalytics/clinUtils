# Create example SDTM exports for the package
#
# A subset of the CDISC Pilot 01 SDTM dataset, available from:
# https://github.com/phuse-org/phuse-scripts/tree/master/data/sdtm/cdiscpilot01/
# is exported for demonstration purpose
#
# Author: Laure Cougnaud
###############################################################################

library(haven)

# Import the data
sdtmPath <- "~/git/phuse-scripts/data/sdtm/cdiscpilot01"
dataSDTMPath <- list.files(
  pattern = "^(dm|ae|lb|cm|mh|ex|sv|vs|ds|qs|suppdm).xpt$", 
  path = sdtmPath, 
  full.names = TRUE
)
dataList <- sapply(dataSDTMPath, function(fileI) {
  haven::read_xpt(fileI)
}, simplify = FALSE)
names(dataList) <- basename(names(dataList))

# Subset only patients of interest
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
dataListSubset <- sapply(dataList, 
  subset, subset = USUBJID %in% subjects, 
  simplify = FALSE
)

# for the qs dataset, keep only primary and secondary endpoints
dataListSubset[["qs.xpt"]] <- subset(
  dataListSubset[["qs.xpt"]], 
  subset = QSTESTCD %in% c("ACTOT", "CIBIC", "NPTOT")
)

# Export the data
extdataPath <- "../inst/extdata/cdiscpilot01/SDTM"
if(!dir.exists(extdataPath))	dir.create(extdataPath, recursive = TRUE)
tmp <- sapply(names(dataListSubset), function(datasetName){
  haven::write_xpt(
    data = dataListSubset[[datasetName]],
    path = file.path(extdataPath, datasetName)
  )
})

# check: compare new and previous versions of the data
# dataSDTMPathPkg <- list.files(
#   path = system.file("extdata", "cdiscpilot01", "SDTM", package = "clinUtils"),
#   pattern = "*.xpt", full.names = TRUE
# )
# dataListPkg <- sapply(dataSDTMPathPkg, function(fileI) {
#   read_xpt(fileI)
# }, simplify = FALSE)
# names(dataListPkg) <- basename(names(dataListPkg))
# identical(x = dataListSubset[names(dataListPkg)], y = dataListPkg)
