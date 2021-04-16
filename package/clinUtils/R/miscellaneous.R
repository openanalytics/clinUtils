#' Capitalize the first letter of a word/sentence.
#' 
#' This implementation is inspired from the help of the \code{toupper} function.
#' @param x Character vector to capitalize
#' @param onlyFirst Logical, if TRUE (by default)
#' capitalize the first letter of the first word only.
#' Otherwise, capitalize the first letters of all words of the sentence.
#' See also \code{link[tools]{toTitleCase}} for a more syntax-friendly implementation.
#' @param rev Logical, if TRUE (FALSE by default), set first letter to lower case (otherwise upper case)
#' @return Character vector with first letter capitalized
#' @author author of the 'toupper' function?
#' @seealso \code{link[tools]{toTitleCase}}
#' @examples 
#' # capitalize only the first word of the sentence
#' simpleCap(x = "this is the caption of my figure.")
#' # capitalize all words
#' simpleCap(x = "this is the caption of my figure.", onlyFirst = FALSE)
#' # opposite: set the first letter of the first word to lower case
#' simpleCap(x = "This is the caption of my figure.", rev = TRUE)
#' @export
simpleCap <- function(x, onlyFirst = TRUE, rev = FALSE) {
	paste0c <- function(...) paste(..., sep = "", collapse = " ")
	fctToUse <- get(ifelse(rev, "tolower", "toupper"))
	simpleCap1 <- function(s) paste0c(fctToUse(substring(s, 1, 1)), substring(s, 2))
	res <- sapply(x, function(x){	
		if(!is.na(x) && x != ""){
			s <- strsplit(x, " ")[[1]]
			if(onlyFirst)	paste0c(c(simpleCap1(s[1]), s[-1]))	else	simpleCap1(s)
		}else x
	})
	return(res)
}

#' Create link to patient profile
#' 
#' Create a link to a patient profile directory
#' (where the patient profile files are saved) by adding an extra column with the link
#' in the data.
#' The path to the patient profile is built as:
#' [patientProfilePath]/subjectProfile-[subjectID].pdf,
#' where '/' are replaced with '-' in the subject 
#' identifier (\code{subjectVar}).
#' @param data a data.frame
#' @param patientProfilePath string indicating the directory 
#' where the patient profiles are stored.
#' @param subjectVar string indicating which column in the data represents the
#' unique subject identifier, "USUBJID" by default.
#' @param checkExist Logical, if TRUE (by default)
#' the \code{patientProfilePath} is checked for existence,
#' and an error is returned if this directory doesn't exist.
#' @return A data.frame with two extra columns:
#' \code{patientProfilePath} and \code{patientProfileLink} with
#' the path to the patient profile and an hyperlink to it, respectively.
#' @author Michela Pasetto
#' @examples 
#' # Typical CDISC dataset contains universal subject ID (USUBJID)
#' data <- data.frame(USUBJID = c("subj1", "subj2", "subj3"))
#' dataWithPatientProfileVar <- createPatientProfileVar(
#'   data = data, 
#'   patientProfilePath = "pathProfiles", 
#'   checkExist = FALSE
#' )
#' # path and HTML link are included in the output dataset
#' head(dataWithPatientProfileVar[, c("USUBJID", "patientProfilePath", "patientProfileLink")])
#' @export 
createPatientProfileVar <- function(
		data,
		patientProfilePath,
		subjectVar = "USUBJID",
		checkExist = TRUE
) {
	
	#pathToDir <- paste(patientProfilePath, collapse = "/")
	
	fileExist <- file.exists(patientProfilePath)	
	if(checkExist & !fileExist) 
		stop("File path for patient profiles not found.")
	
	if(! subjectVar %in% colnames(data)) {
		
		warning(
			sprintf(
				paste("Unique subject identifier '%s' not available in the data,",
					"so the patient profile variable is not created."
				), subjectVar
			)
		)
		return(data)
		
	}	
	
	data$patientProfilePath <- file.path(
			patientProfilePath, 
			sprintf(
					"subjectProfile-%s.pdf",
					sub("/", "-", data[, subjectVar])
			)
	)
	
	data$patientProfileLink <- sprintf(
			'<a href="%s" target="_blank">%s</a>',
			data$patientProfilePath,
			data[, subjectVar]
	)
	
	return(data)
	
}


#' Function for reordering columns
#' 
#' @param data a data.frame
#' @param vars named vector indicating the position in the data frame of the specified variable
#' @export 
#' @examples
#' someData <- data.frame(
#'		"Col1" = c(1, 2),
#'		"Col2" = c(2, 3),
#'		"Col3" = c(3, 4)
#'		)
#' reorderColumns(
#'		data = someData,
#'		vars = c("Col3" = 1)
#'		)
reorderColumns <- function(data, vars){
	
	# Sort out inputs
	dataNames <- names(data)
	numberVars <- length(dataNames)
	varNames <- names(vars)
	if(any(! varNames %in% dataNames)) stop("One or more vars is not available in the data")
	varPosition <- vars
	# Prepare output
	outPosition <- character(numberVars)
	outPosition[varPosition]  <- varNames
	outPosition[-varPosition] <- dataNames[ !(dataNames %in% varNames) ]
	# Re-arrange vars by position
	data <- data[ , outPosition]
	return(data)
}
