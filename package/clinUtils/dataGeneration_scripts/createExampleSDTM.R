# Store subset of SDTM data sets


library(haven)

extdataPath <- "inst/extdata/cdiscpilot01/SDTM"
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

# Subset of the SDTM Datasets

sdtmPath <- "~/git/phuse-scripts/data/sdtm/cdiscpilot01"

dataSDTMPath <- list.files(pattern = "^(dm|ae|lb|cm|mh|ex|sv|vs).xpt$", path = sdtmPath, full.names = TRUE)

dataList <- sapply(dataSDTMPath, function(fileI) {
      read_xpt(fileI)
    }, simplify = FALSE)

dataListSubset <- sapply(names(dataList), function(fileI) {
      
      dataI <- dataList[[fileI]]
      dataSubset <- subset(dataI, USUBJID %in% subjects)
      dataSubset
      
    }, simplify = FALSE)

sapply(names(dataListSubset), function(fileI) {
      
      dataI <- dataListSubset[[fileI]]
      datasetName <- gsub(".+[/](.+)[.]xpt", "\\1.xpt", fileI)
      
      write_xpt(
          data = dataI,
          path = file.path(extdataPath, datasetName)
      )
    })


