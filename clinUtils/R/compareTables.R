#' General parameters used for the comparison table functionality
#' 
#' @name comparisonTables-common-args
#' @param newData data.frame object representing the new data
#' @param oldData data.frame object representing the old data
#' @param referenceVars character vector of the columns in the data that are the used as
#' reference for the comparison. \cr 
#' If not specified, all columns present both in
#' \code{newData} and \code{oldData} are considered.
#' @param changeableVars character vector of the columns in the data for which you want to assess the change,
#' e.g. variables that might have changed from the old to the new data.\cr
#' If not specified, only 'Addition' and 'Removal' are detected.
#' @param diffData Object of class 'diff.data' containing
#' differences between datasets, as returned by the \code{\link{compareDiff}} function.
#' @param outputType String describing which output should be returned,
#' (multiple are possible), either:
#' \itemize{
#' \item{'table-comparison': }{data.frame containing difference between
#' two datasets, see 'output' of \code{\link{compareDiff}} function.}
#' \item{'table-comparison-interactive': }{\code{\link[DT]{datatable}} object with differences between
#' the two datasets, see 'output' of \code{\link{exportDiffData}}.}
#' \item{'newData-diff' or 'oldData-diff': }{data.frame with new/old data respectively,
#' containing the information if each record differs in the old/new datasets respectively.
#' See output of \code{\link{mergeDiffWithData}}.}
#' \item{'newData-diff-interactive' or 'oldData-diff-interactive': }{\code{\link[DT]{datatable}} with new/old data respectively,
#' containing the information if each record differs in the old/new datasets respectively.
#' See output of \code{\link{exportDiffData}}.}
#' }
#' @return The comparison of the two input tables.
NULL

#' Compare tables
#' @inheritSection compareDiff Identification of the differences between datasets
#' @param ... Any parameters passed to the \code{\link{exportDiffData}} function.
#' These are only used if 'table-comparison-interactive' is specified in \code{outputType}.
#' @inheritParams comparisonTables-common-args
#' @return One of the output types specified in \code{outputType}.
#' By default, all outputs are returned.
#' If multiple output types are specified, a list of those are returned
#' (named by output type).
#' @author Laure Cougnaud, Michela Pasetto
#' @example inst/examples/compareTables-example.R
#' @export 
compareTables <- function(
    newData,
    oldData,
    referenceVars = intersect(colnames(newData), colnames(oldData)),
    changeableVars = NULL,
    outputType = c(
        "table-comparison", 
        "newData-diff", 
        "oldData-diff",
        "table-comparison-interactive",
        "newData-diff-interactive",
        "oldData-diff-interactive"
    ),
#	"table-summary-comparison", "table-summary-comparison-interactive",
    ...){
  
  outputType <- match.arg(outputType, several.ok = TRUE)
  
  # compare difference between datasets:
  comparisonDiff <- compareDiff(
      newData = newData, oldData = oldData, 
      referenceVars = referenceVars,
      changeableVars = changeableVars
  )
  
  output <- list()
  
  if("table-comparison" %in% outputType) {
    output <- c(output, list("table-comparison" = comparisonDiff))
  }
  
  if(any(grepl("^newData-diff", outputType))) {
    
    newDataDiff <- mergeDiffWithData(
        diffData = comparisonDiff,
        newData = newData
    )
    
    if("newData-diff" %in% outputType)
      output <- c(output, list("newData-diff" = newDataDiff))
    
    if("newData-diff-interactive" %in% outputType) {
      
      newDataDiffDT <- exportDiffData(
          diffData = comparisonDiff,
          newDataDiff = newDataDiff,
          to = "DT", ...
      )
      output <- c(output, list("newData-diff-interactive" = newDataDiffDT))
    }
    
  }
  
  if(any(grepl("^oldData-diff", outputType))) {
    
    oldDataDiff <- mergeDiffWithData(
        diffData = comparisonDiff,
        oldData = oldData
    )
    
    if("oldData-diff" %in% outputType)
      output <- c(output, list("oldData-diff" = oldDataDiff))
    
    if("oldData-diff-interactive" %in% outputType) {
      
      oldDataDiffDT <- exportDiffData(
          diffData = comparisonDiff,
          oldDataDiff = oldDataDiff,
          to = "DT", ...
      )
      output <- c(output, list("oldData-diff-interactive" = oldDataDiffDT))
    }
    
  }
  if("table-comparison-interactive" %in% outputType){
    
    comparisonDiffDT <- exportDiffData(diffData = comparisonDiff, to = "DT", ...)
    output <- c(output, list("table-comparison-interactive" = comparisonDiffDT))
    
  }
  
  if(length(outputType) == 1)
    output <- output[[outputType]]
  
  return(output)
  
}

#' Get differences between two data.frames
#' @section Identification of the differences between datasets:
#' To identify the differences between datasets, the following steps are followed:
#' \enumerate{
#' \item{removal of records identical between the old and new dataset
#' (will be considered as 'Identical' later on)}
#' \item{records with a reference value present in the old dataset but not
#' in the new dataset are considered 'Removal'}
#' \item{records with a reference value present in the new dataset but not
#' in the old dataset are considered 'Addition'}
#' \item{records with reference value present both in the new and old dataset,
#' \strong{after filtering of identical records} and with difference in
#' the changeable variables are considered 'Change'}
#' }
#' @inheritParams comparisonTables-common-args
#' @return Object of class 'diff.data', i.e. a data.frame with columns:
#' \itemize{
#' \item{'Comparison type': }{type of difference between
#' the old and new data, either:
#' \itemize{
#' \item{'Change': }{records present both in new and old data,
#' based on the reference variables, but with difference(s) in changeable vars}
#' \item{'Addition': }{records with reference variables
#' present in new but not in old data}
#' \item{'Removal': }{records with reference variables 
#' present in old but not in new data}
#' }}
#' \item{'Version': }{'Previous' or 'Current' depending if record represents
#' content from old or new data respectively}
#' \item{\code{referenceVars}}
#' \item{\code{changeableVars}}
#' }
#' @author Laure Cougnaud
#' @import data.table
#' @export 
compareDiff <- function(
    newData, oldData, 
    referenceVars = intersect(colnames(newData), colnames(oldData)), 
    changeableVars = NULL
){
  
  # convert input to data.table
  newData <- convertToDatatable(newData)
  oldData <- convertToDatatable(oldData)
  
  # extract only columns of interest: reference vars for keys and only specified columns
  cols <- c(referenceVars, changeableVars)
  newData <- unique(newData[, .SD, .SDcols = cols])
  oldData <- unique(oldData[, .SD, .SDcols = cols])
  
  #############
  ## Crucial ##
  ## remove records both in new and old data
  oldDataDiff <- fsetdiff(oldData, newData)
  newDataDiff <- fsetdiff(newData, oldData)
  
  ## among these records...
  
  # Rows in old not in new
  removal <- getSetDiff(x = oldDataDiff, y = newDataDiff, referenceVars)
  removal[, c("Comparison type", "Version") := list("Removal", "Previous")]
  
  # Rows in new not in old
  addition <- getSetDiff(x = newDataDiff, y = oldDataDiff, referenceVars)
  addition[, c("Comparison type", "Version") := list("Addition", "Current")]
  
  ## Get changed records
  
  if(!is.null(changeableVars)){
    
    changeWide <- newDataDiff[oldDataDiff, , on = referenceVars, nomatch = NULL]
    
    # extract info on changed records in newData
    changeNew <- changeWide[, .SD, .SDcols = cols]
    changeNew[, c("Comparison type", "Version") := list("Change", "Current")]
    
    # extract info on changed records in oldData
    colDtOld <- c(referenceVars, paste0("i.", changeableVars))
    changeOld <- changeWide[, .SD, .SDcols = colDtOld]
    setnames(x = changeOld, old = paste0("i.",changeableVars), new = changeableVars)
    changeOld[, c("Comparison type", "Version") := list("Change", "Previous")]
    
    change <- rbindlist(list(changeNew, changeOld))
    
    # filter records equal old and new (is there a better way to do it)?
    change <- change[!duplicated(change, by = cols), ]
    
  }else	change <- data.table()
  
  ## Build comparison object:
  
  comparisonDiff <- rbindlist(list(removal, addition, change))
  colsDiff <- c("Comparison type", "Version", referenceVars, changeableVars)
  comparisonDiff <- comparisonDiff[, .SD, .SDcols = colsDiff]
  
  # use factors for new 'Comparison type' and 'Version' columns
  comparisonDiff$`Comparison type` <- factor(
      comparisonDiff$`Comparison type`, levels = c("Addition", "Change", "Removal")
  )
  comparisonDiff$Version <- factor(comparisonDiff$Version, levels = c("Current", "Previous"))
  
  # fast ordering by reference
  colsOrder <- c(referenceVars, "Comparison type", "Version")
  setorderv(comparisonDiff, cols = colsOrder)
  
  comparisonDiff <- as.data.frame(comparisonDiff)
  class(comparisonDiff) <- c("diff.data", class(comparisonDiff))
  
  comparisonDiff <- structure(comparisonDiff, referenceVars = referenceVars, changeableVars = changeableVars)
  
  return(comparisonDiff)
  
}

#' Convert to data.table
#' 
#' Convert a data frame into a \code{data.table} object.
#' @param data A data.frame
#' @return A \code{data.table} object.
#' @import data.table
convertToDatatable <- function(data) {
  
  if(!inherits(data, "data.table")) {
    data <- as.data.table(data)
  }
  
  return(data)
  
}

#' Get additions/removals
#' 
#' Get only additions and removals from two data sets
#' (\code{data.table} objects).
#' The additions/removals are extracted as \code{x} vs
#' \code{y}.
#' This function assumes that the objects \code{x}
#' and \code{y} don't share identical rows.
#' @param x A \code{data.table} object
#' @param y A \code{data.table} object
#' @inheritParams comparisonTables-common-args
#' @return A \code{data.table} object with the 
#' additions/removals with respect of the comparison
#' between \code{x} vs \code{y}.
#' @import data.table
getSetDiff <- function(x, y, referenceVars) {
  diffDt <- fsetdiff(
      x = x[, .SD, .SDcols = referenceVars], 
      y = y[, .SD, .SDcols = referenceVars]
  )
  diffDt <- x[diffDt, on = referenceVars]
  return(diffDt)
}


#' Merge the 'diff.data' object from \code{\link{compareDiff}}
#' with the original \code{newData} or \code{oldData}.
#' 
#' The \code{newData}/\code{oldData} are merged 
#' with \code{diffData} based on the columns of \code{diffData}
#' excepted 'Comparison type' and 'Version'.
#' @inheritParams comparisonTables-common-args
#' @return The \code{newData} or \code{oldData} (as a data frame object)
#' with the extra column 'Comparison type' specifying the type
#' of change, either:
#' \itemize{
#' \item{'Change': }{record present in both dataset based 
#' on the reference variables, but with changes in the
#' changeable variables}
#' \item{'Addition': }{records present in new but not in old data}
#' \item{'Removal': }{records present in old but not in new data}
#' \item{'Identical': }{records identical in the old and new datasets
#' (on both the reference and changeable variables)}
#' }
#' @import data.table
#' @export 
mergeDiffWithData <- function(
    diffData, newData, oldData
) {
  
  if(missing(newData) & missing(oldData) || (!missing(newData) & !missing(oldData))){
    stop("'newData' or 'oldData' should be specified.")
  }
  
  typeData <- ifelse(!missing(newData), "new", "old")
  version <- ifelse(typeData == "new", "Current", "Previous")
  
  # subset diff data of interest
  diffData <- convertToDatatable(diffData)
  
  # extract input data
  inputData <- switch(typeData, new = newData, old = oldData)
  inputData <- convertToDatatable(inputData)
  
  # merge input data with diff
  colsDiff <- c("Comparison type", "Version")
  colsBy <- setdiff(colnames(diffData), colsDiff)
  
  if(any(colsDiff %in% colnames(inputData)))
    stop(paste(toString(shQuote(colsDiff)), "are reserved names for diff data,",
            "so shouldn't be available in", typeData, "data.")) 
  
  inputDataWithDiff <- mergeInputDiff(
      diffData = diffData, inputData = inputData,
      typeData = typeData,
      colsBy = colsBy
  )
  
  # format output
  colsDiff <- c("Comparison type", "Version", colnames(inputData))  
  inputDataWithDiff <- inputDataWithDiff[, .SD, .SDcols = colsDiff]
  inputDataWithDiff[
      is.na(inputDataWithDiff$`Comparison type`), 
      `:=` ("Comparison type" = "Identical", "Version" = version)
  ]
  
  compTypes <- c(
      if(typeData == "new")	"Addition", 
      "Change", 
      if(typeData == "old")	"Removal",
      "Identical"
  )
  if(!all(inputDataWithDiff$`Comparison type` %in% compTypes))
    stop("Comparison type should be among: ", 
        toString(shQuote(compTypes)), 
        "."
    )
  inputDataWithDiff$`Comparison type` <- factor(
      inputDataWithDiff$`Comparison type`, 
      levels = compTypes
  )
  
  inputDataWithDiff <- as.data.frame(inputDataWithDiff)
  
  return(inputDataWithDiff)
  
}

#' Custom merge of difference data with input data
#' 
#' Custom merge (left join) of difference data with some input data
#' @param diffData A \code{data.table} object as output from \code{\link{compareDiff}}.
#' @param inputData A \code{data.table} object. For instance, the \code{newData} or
#' the \code{oldData} argument from \code{\link{compareTables}}.
#' @param typeData String with type of data, as "new" for \code{newData} or
#' "old" for \code{oldData}.
#' @param colsBy Character vector of columns for doing the merge by.
#' @return A \code{data.table} object.
#' The \code{inputData} is joined with the columns \code{Comparison type}
#' and \code{Version} from the \code{diffData} argument.
#' @import data.table
mergeInputDiff <- function(
    diffData, inputData, typeData, colsBy
) {
  
  version <- ifelse(typeData == "new", "Current", "Previous")
  diffDataSubset <- diffData[which(diffData$`Version` == version), ]
  
  inputDataWithDiff <- merge(
      x = inputData, y = diffDataSubset,
      all.x = TRUE, sort = FALSE, by = colsBy
  )
  
  # sort columns as in input
  colsOrder <- c(
      setdiff(colnames(diffDataSubset), colnames(inputData)),
      colnames(inputData)
  )
  colsOrder <- unique(colsOrder)
  inputDataWithDiff <- inputDataWithDiff[, .SD, .SDcols = colsOrder]
  
  # check
  isInputSame <- all.equal(
      target = inputData, 
      current = inputDataWithDiff[, .SD, .SDcols = colnames(inputData)], 
      check.attributes = FALSE # ignore attributes in input object
  )
  if(!isTRUE(isInputSame)){
    stop("Issue in merging the comparison diff data and the input dataset.")
  }
  
  return(inputDataWithDiff)
}

#' Export the 'diff.data' object from \code{\link{compareDiff}}
#' to a user-friendly format
#' 
#' @param newDataDiff data.frame with new data with differences
#' as returned by the \code{\link{mergeDiffWithData}}.
#' The data set contains the new data with the
#' information if each record differs in the new dataset.
#' @param oldDataDiff data.frame with old data with differences
#' as returned by the \code{\link{mergeDiffWithData}}.
#' The data set contains the old data with the
#' information if each record differs in the old dataset.
#' @param to String with export format,
#' currently only: \code{DT} is available to export to
#' a \code{\link[DT]{datatable}} object.
#' @param ... Extra parameters besides 'data' and 'nonVisibleVars',
#' currently passed to the \code{\link{getClinDT}} function.
#' @inheritParams comparisonTables-common-args
#' @return Depending on the \code{to} parameter:
#' \itemize{
#' \item{'DT': }{a \code{\link[DT]{datatable}} with the difference between datasets,
#' with:
#' \itemize{
#' \item{highlighting depending on the difference between datasets:
#' \itemize{
#' \item{'Addition' in green}
#' \item{'Removal' in yellow}
#' \item{'Change' in lightblue}
#' \item{'Identical' are not highlighted}
#' }
#' }
#' \item{records only present in the old dataset are displayed in italic}
#' }
#' }
#' }
#' @import DT
#' @export 
exportDiffData <- function(
    diffData,
    newDataDiff, oldDataDiff,
    referenceVars = attr(diffData, "referenceVars"), 
    changeableVars = attr(diffData, "changeableVars"), 
    to = "DT", ...){
  
  to <- match.arg(to)
  
  argsGetClinDT <- list(...)
  
  diffData <- convertToDatatable(diffData)
    
  # placeholder in case there is no records in the diff data
  if(nrow(diffData) == 0 & (missing(newDataDiff) | missing(oldDataDiff))) {
    if(is.null(argsGetClinDT$options$language$zeroRecords)) {
      argsGetClinDT$options <- c(argsGetClinDT$options,
          list(
              language = list(
                  zeroRecords = paste(
                      "There is no difference between",
                      "the previous and the current data."
                  )
              )
          )
      )
    }
  }
  
  # build columns containing for coloring
  diffDataDetails <- formatDetailsComparison(
      diffData = diffData, 
      referenceVars = referenceVars, 
      changeableVars = changeableVars
  )
  diffCols <- attr(diffDataDetails, "colsDiff")
  
  # extract 'diff' cols for [new/old]DataDiff
  if(!missing(newDataDiff) | !missing(oldDataDiff)) {
    
    typeData <- ifelse(!missing(newDataDiff), "new", "old")
    inputData <- switch(typeData, new = newDataDiff, old = oldDataDiff)
    inputData <- convertToDatatable(inputData)
    
    inputDataWithDiff <- mergeInputDiff(
        diffData = diffDataDetails,
        inputData = inputData,
        colsBy = colnames(diffData),
        typeData = typeData
    )
    # Set 'diff columns for identical records to: '=='
    inputDataWithDiff[
        which(inputDataWithDiff$`Comparison type` == "Identical"), 
        (diffCols) := "==",
    ]
    diffDataDetails <- inputDataWithDiff
    
  }
  
  # include diff columns as latest
  nonDiffCols <- setdiff(colnames(diffDataDetails), diffCols)
  colsOrdered <- c(nonDiffCols, diffCols)
  diffDataDetails <- diffDataDetails[, .SD, .SDcols = colsOrdered]
  
  # convert diffData to data.table
  # couldn't we use DT::datatable directly?
  argsGetClinDT$data <- diffDataDetails
  nonVisibleVar <- c(argsGetClinDT$nonVisibleVar, diffCols)
  argsGetClinDT$nonVisibleVar <- nonVisibleVar
  argsGetClinDT$verbose <- FALSE
  
  diffDataDt <- do.call(getClinDT, argsGetClinDT)
  
  # if column names have been renamed, the new column names
  # should be specified in formatStyle:
  if("colnames" %in% names(argsGetClinDT)){
    labelVars <- setNames(names(argsGetClinDT$colnames), argsGetClinDT$colnames)
    names(diffCols) <- getLabelVar(var = names(diffCols), label = labelVars)
  }
  
  diffDataDt <- diffDataDt %>%			
      formatStyle(
          columns = names(diffCols),
          valueColumns = diffCols,
          backgroundColor = styleEqual(
              c("+", "-", "==", "!="), 
              c("#74D055FF", "#DCE318FF", "white", "#238A8DFF")
          )
      ) %>%			
      formatStyle(
          "Version",
          target = "row",
          'font-style' = styleEqual("Previous", "italic")
      )
  
  return(diffDataDt)
  
  
}

#' @importFrom utils globalVariables
utils::globalVariables("Comparison type")

#' Format details comparison
#' @inheritParams comparisonTables-common-args
#' @return \code{diffData} with extra columns:
#' '[].diff' for the \code{referenceVars} and \code{changeableVars}
#' columns, and attributes: 'colsDiff' as a named vector
#' with mapping with input variables (names) and corresponding
#' diff variables.
#' @author Laure Cougnaud
#' @importFrom stats setNames
#' @import data.table
formatDetailsComparison <- function(
    diffData, 
    referenceVars = attr(diffData, "referenceVars"), 
    changeableVars = attr(diffData, "changeableVars")){
  
  diffDataDetailed <- copy(diffData)
  
  cols <- c(referenceVars, changeableVars)
  colsDiff <- setNames(paste0(cols, ".diff"), cols)
  
  diffDataDetailed[, (colsDiff) := {
        if(all(`Comparison type` == "Addition")) {
          as.list(rep("+", length(cols)))
        } else	if(all(`Comparison type` == "Removal")) {
          as.list(rep("-", length(cols)))
        } else {
          lapply(.SD, function(x) {
                ifelse(uniqueN(x) == 1, "==", "!=")		
              })
        }
      },	
      .SDcols = cols,
      by = referenceVars
  ]  
  
  attr(diffDataDetailed, "colsDiff") <- colsDiff
  
  return(diffDataDetailed)
  
}
