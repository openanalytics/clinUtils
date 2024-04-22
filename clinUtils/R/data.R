#' Example of SDTM datasets from the CDISC original Pilot 01 study
#' 
#' This contains a subset of the CDISC original Pilot 01 study dataset for:
#' \itemize{
#' \item a selected subset of subjects
#' \item a selected subset of domains: 
#' \itemize{
#' \item adverse event ('ae')
#' \item concomitant medications ('cm')
#' \item disposition ('ds')
#' \item demographics ('dm')
#' \item treatment exposure ('ex')
#' \item laboratory ('lb')
#' \item medical history ('mh')
#' \item questionnaire ('qs') - only:
#' \itemize{
#' \item ADAS-Cog (11) primary endpoint (\code{QSTESTCD == 'ACTOT'})
#' \item CIBIC+ primary endpoint (\code{QSTESTCD == 'CIBIC'})
#' \item NPI-X Item secondary endpoint (\code{QSTESTCD == 'NPTOT'})
#' }
#' \item demographics supplemental dataset ('suppdm')
#' \item subject visits ('sv')
#' \item vital signs ('vs')
#' }
#' }
#' This dataset was created following the SDTM Version 2
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
#' This contains a subset of the CDISC Pilot 01 study dataset for:
#' \itemize{
#' \item a selected subset of subjects
#' \item a selected subset of domains: 
#' \itemize{
#' \item subject-level ('adsl')
#' \item adverse event ('adae')
#' \item laboratory chemistry data('adlbc')
#' \item vital signs ('advs')
#' \item concomitant medications ('adcm')
#' \item efficacy: 
#' \itemize{
#' \item ADAS-COG Data ('adqsadas'), containing one of the 
#'   primary endpoint: ADAS-Cog (11)\cr
#'   (Alzheimer's Disease Assessment Scale - Cognitive Subscale
#' \item CIBIC+ questionnaire data ('adqscibc'), containing one 
#' of the primary endpoint: CIBIC+\cr
#' (Video-referenced Clinician's Interview-based Impression of Change)
#' \item NPI-X Item data ('adqsnpix'), containing the secondary endpoint: 
#' NPI-X\cr (Mean Revised Neuropsychiatric Inventory)
#' }
#' \item pharmacokinetic parameters ('adpp')\cr
#' Please note that this dataset contains different sets 
#' of subjects than the other example datasets.
#' }}
#' This dataset was created following the ADaM Version 2.0
#' standard.
#' This dataset contains the 'Modified and augmented version 
#' of cdiscpilot01' dataset.
#' @format List of data.frames containing the ADaM
#' dataset for each selected domain.\cr
#' Labels for the different variables across datasets
#' is available via the \code{labelVars} attribute.
#' @seealso \link{loadDataADaMSDTM}
#' @name dataADaMCDISCP01
#' @docType data
#' @author Laure Cougnaud
#' @source Original (and entire) datasets are available in: \cr
#' \url{https://github.com/phuse-org/phuse-scripts/tree/master/data/adam/cdisc}
#' See in particular the \emph{define.xml} file
#' for further description of the
#' datasets and variables name.
#' @keywords data
NULL
