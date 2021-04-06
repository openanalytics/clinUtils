#' Example of SDTM datasets from the CDISC original Pilot 01 study
#' 
#' This contains a subset of the CDISC original Pilot 01 study dataset for:
#' \itemize{
#' \item{a selected subset of subjects}
#' \item{a selected subset of domains: demographics ('dm'),
#'  adverse event ('ae') and laboratory ('lb') data}
#' }
#' @format List of data.frame containing the SDTM
#' dataset for each selected domain.\cr
#' Labels for the different variables across datasets
#' is available via the \code{labelVars} attribute.
#' @seealso \link{loadDataADaMSDTM}
#' @name dataSDTMCDISCP01
#' @docType data
#' @author Laure Cougnaud
#' @source Original (and entire) datasets are available in: \cr
#' \url{https://github.com/phuse-org/phuse-scripts/tree/master/data/sdtm/cdiscpilot01}
#' See in particular the \emph{define.xml} file
#' for further description of the
#' datasets and variables name.
#' @keywords data
NULL

#' Example of ADaM datasets from the CDISC original Pilot 01 study
#' 
#' This contains a subset of the CDISC original Pilot 01 study dataset for:
#' \itemize{
#' \item{a selected subset of subjects}
#' \item{a selected subset of domains: subject-level ('ADSL'),
#'  adverse event ('adae') and laboratory chemistry ('adlbc') data}
#' }
#' @format List of data.frame containing the ADaM
#' dataset for each selected domain.\cr
#' Labels for the different variables across datasets
#' is available via the \code{labelVars} attribute.
#' @seealso \link{loadDataADaMSDTM}
#' @name dataADaMCDISCP01
#' @docType data
#' @author Laure Cougnaud
#' @source Original (and entire) datasets are available in: \cr
#' \url{https://github.com/phuse-org/phuse-scripts/tree/master/data/adam/cdiscpilot01}
#' See in particular the \emph{define.xml} file
#' for further description of the
#' datasets and variables name.
#' @keywords data
NULL
