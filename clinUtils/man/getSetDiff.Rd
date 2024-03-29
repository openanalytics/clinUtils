% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareTables.R
\name{getSetDiff}
\alias{getSetDiff}
\title{Get additions/removals}
\usage{
getSetDiff(x, y, referenceVars)
}
\arguments{
\item{x}{A \code{data.table} object}

\item{y}{A \code{data.table} object}

\item{referenceVars}{character vector of the columns in the data that are the used as
reference for the comparison. \cr 
If not specified, all columns present both in
\code{newData} and \code{oldData} are considered.}
}
\value{
A \code{data.table} object with the 
additions/removals with respect of the comparison
between \code{x} vs \code{y}.
}
\description{
Get only additions and removals from two data sets
(\code{data.table} objects).
The additions/removals are extracted as \code{x} vs
\code{y}.
This function assumes that the objects \code{x}
and \code{y} don't share identical rows.
}
