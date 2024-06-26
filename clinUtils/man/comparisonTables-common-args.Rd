% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareTables.R
\name{comparisonTables-common-args}
\alias{comparisonTables-common-args}
\title{General parameters used for the comparison table functionality}
\arguments{
\item{newData}{data.frame object representing the new data}

\item{oldData}{data.frame object representing the old data}

\item{referenceVars}{character vector of the columns in the data that are the used as
reference for the comparison. \cr 
If not specified, all columns present both in
\code{newData} and \code{oldData} are considered.}

\item{changeableVars}{character vector of the columns in the data for which you want to assess the change,
e.g. variables that might have changed from the old to the new data.\cr
If not specified, only 'Addition' and 'Removal' are detected.}

\item{diffData}{Object of class 'diff.data' containing
differences between datasets, as returned by the \code{\link{compareDiff}} function.}

\item{outputType}{String describing which output should be returned,
(multiple are possible), either:
\itemize{
\item 'table-comparison': data.frame containing difference between
two datasets, see 'output' of \code{\link{compareDiff}} function.
\item 'table-comparison-interactive': \code{\link[DT]{datatable}} object 
with differences between the two datasets, see 'output' of 
\code{\link{exportDiffData}}.
\item 'newData-diff' or 'oldData-diff': data.frame with new/old data 
respectively, containing the information if each record differs in the 
old/new datasets respectively. See output of \code{\link{mergeDiffWithData}}.
\item 'newData-diff-interactive' or 'oldData-diff-interactive': 
\code{\link[DT]{datatable}} with new/old data respectively, containing the 
information if each record differs in the old/new datasets respectively.
See output of \code{\link{exportDiffData}}.
}}
}
\value{
The comparison of the two input tables.
}
\description{
General parameters used for the comparison table functionality
}
