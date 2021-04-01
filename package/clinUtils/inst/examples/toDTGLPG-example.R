library(dplyr)

data(ADaMDataPelican)
data(labelVarsADaMPelican)

# example of simple adverse event table
dataAE <- ADaMDataPelican$ADAE
subjectsSafety <- subset(ADaMDataPelican$ADSL, SAFFL == "Y")$USUBJID

# add patient profiles link (if available)
dataAE <- createPatientProfileVar(
	data = dataAE, 
	patientProfilePath = "patientProfiles", 
	checkExist = FALSE
)

# compute counts of subjects presenting each AE
tableAE <- dataAE %>% 
	group_by(AESOC, AEDECOD) %>% 
	filter(USUBJID %in% subjectsSafety) %>%
	summarise(
		N = n_distinct(USUBJID),
		perc = round(N/length(subjectsSafety)*100, 3),
		USUBJID = toString(patientProfileLink)
	) %>%
	arrange(desc(perc))


# extract new variables labels
tableAELabels <- getLabelVar(
	var = colnames(tableAE),
	labelVars = labelVarsADaMPelican,
	label = c(N = '# subjects', perc = "% subjects")
)
# 'colnames' should be specified as c('new name' = 'old name', ...)
tableAELabelsDT <- setNames(names(tableAELabels), tableAELabels)

tableAEBase <- tableAE[, setdiff(colnames(tableAE), "USUBJID")]

## create table with bar

# default:
toDTGLPG(
	data = tableAEBase,
	barVar = "perc",
	colnames = tableAELabelsDT
)

# specify range for the bar
toDTGLPG(
	data = tableAEBase,
	filter = "none",
	barVar = "perc",
	barRange = c(0, 100),
	colnames = tableAELabelsDT
)

# change color according to thresold
toDTGLPG(
	data = tableAEBase,
	filter = "none",
	barVar = "perc",
	barColorThr = seq(from = 0, to = 100, by = 25),
	colnames = tableAELabelsDT
)

# group per system organ class:
tableAEGroup <- tableAEBase %>%
	group_by(AESOC) %>%
	mutate(NAESOC = sum(N)) %>%
	arrange(desc(NAESOC), AESOC, desc(perc)) %>%
	select(-NAESOC)
	
toDTGLPG(
	data = tableAEGroup,
	filter = "none",
	barVar = "perc",
	barRange = c(0, 100),
	colnames = tableAELabelsDT,
	rowGroupVar = "AESOC",
	pageLength = Inf
)

# expand the subject ID column, will
# be accessible when clicking on the '+' button
# Format URL correctly with: 'escape',
# please note that indexing starts at 0!
toDTGLPG(
	data = tableAE,
	barVar = "perc",
	colnames = tableAELabelsDT,
	expandVar = "USUBJID", 
	escape = grep("USUBJID", colnames(tableAE))-1
)

# with fixed columns:
toDTGLPG(
	data = tableAEGroup,
	colnames = tableAELabelsDT,
	fixedColumns = list(leftColumns = 1),
	columnsWidth = c(0.1, 0.7, 0.1, 0.1),
	width = "350px" # change dimension table
)

# change default buttons
toDTGLPG(
	data = tableAEGroup,
	colnames = tableAELabelsDT,
	# remove general filter
	filter = "none",
	# custom set of buttons
	buttons = c("csv", "excel", "pdf"),
	# change number of records displayed at each page
	pageLength = 20
)

# hide the first column:
toDTGLPG(
	data = tableAEGroup,
	nonVisibleVar = "AESOC"
)

# with specific caption
library(htmltools)
caption <- tags$caption(
	"Number of subjects with adverse events grouped by system organ class.",
	br(), 
	paste(
		"Percentages are based on the total number of patients having",
		"received a first study treatment."
	)
)
toDTGLPG(
	data = tableAEGroup,
	filter = "none",
	barVar = "perc",
	barRange = c(0, 100),
	pageLength = Inf,
	colnames = tableAELabelsDT,
	rowGroupVar = "AESOC",
	caption = caption
)