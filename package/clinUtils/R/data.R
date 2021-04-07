#' Example of SDTM datasets from the CDISC original Pilot 01 study
#' 
#' This contains a subset of the CDISC original Pilot 01 study dataset for:
#' \itemize{
#' \item{a selected subset of subjects}
#' \item{a selected subset of domains: 
#' \itemize{
#' \item{demographics ('dm')}
#' \item{treatment exposure ('ex')}
#' \item{concomitant medications ('cm')}
#' \item{medical history ('mh')}
#' \item{adverse event ('ae')}
#' \item{laboratory ('lb')}
#' \item{vital signs ('vs')}
#' \item{subject visit ('sv')}
#' }
#' }
#' }
#' This dataset was created following the SDTM Version 1.1
#' standard.
#' @format List of data.frames containing the SDTM
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
#' This dataset was created following the ADaM Version 2.0
#' standard.
#' @format List of data.frames containing the ADaM
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
