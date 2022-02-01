data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

# example of simple adverse event table
dataAE <- dataADaMCDISCP01$ADAE
subjectsSafety <- subset(dataADaMCDISCP01$ADSL, SAFFL == "Y")$USUBJID

# compute counts of subjects presenting each AE
tableAE <- stats::aggregate(
	formula = USUBJID ~ AESOC:AEDECOD,
	data = dataAE, 
	FUN = function(usubjid)	length(unique(usubjid))
)
colnames(tableAE)[colnames(tableAE) == "USUBJID"] <- "N"
# and percentages
tableAE$perc <- round(tableAE$N/length(subjectsSafety)*100, 3)
# sort records in decreasing percentage
tableAE <- tableAE[order(tableAE$perc, decreasing = TRUE), ]

# extract new variables labels
tableAELabels <- getLabelVar(
	var = colnames(tableAE),
	labelVars = labelVars,
	label = c(N = '# subjects', perc = "% subjects")
)
# 'colnames' for DT should be specified as c('new name' = 'old name', ...)
tableAELabelsDT <- setNames(names(tableAELabels), tableAELabels)

## create table with bar

# default:
getClinDT(
	data = tableAE,
	barVar = "perc",
	colnames = tableAELabelsDT
)

# specify range for the bar
getClinDT(
	data = tableAE,
	filter = "none",
	barVar = "perc",
	barRange = c(0, 100),
	colnames = tableAELabelsDT
)

# change color according to threshold
getClinDT(
	data = tableAE,
	filter = "none",
	barVar = "perc",
	barColorThr = seq(from = 0, to = 100, by = 25),
	colnames = tableAELabelsDT
)

## group per system organ class (and decreasing N):
tableAESOC <- aggregate(formula = N ~ AESOC, data = tableAE, FUN = sum)
tableAE$AESOC <- factor(tableAE$AESOC,
	levels = tableAESOC[order(tableAESOC$N, decreasing = FALSE), "AESOC"]
)
tableAE <- tableAE[order(tableAE$AESOC, tableAE$perc, decreasing = TRUE), ]
	
getClinDT(
	data = tableAE,
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
getClinDT(
	data = tableAE,
	barVar = "perc",
	colnames = tableAELabelsDT,
	expandVar = "USUBJID", 
	escape = grep("USUBJID", colnames(tableAE))-1
)

# fix size for columns
getClinDT(
	data = tableAE,
	colnames = tableAELabelsDT,
	fixedColumns = list(leftColumns = 1),
	columnsWidth = c(0.1, 0.7, 0.1, 0.1),
	width = "350px" # change dimension table
)

# change default buttons
getClinDT(
	data = tableAE,
	colnames = tableAELabelsDT,
	# remove general filter
	filter = "none",
	# custom set of buttons
	buttons = getClinDTButtons(type = c("csv", "excel", "pdf"))
)
# add button to select columns
getClinDT(
	data = tableAE,
	colnames = tableAELabelsDT,
	# custom set of buttons
	buttons = getClinDTButtons(typeExtra = "colvis")
)
# export pdf in landscape format
buttons <- getClinDTButtons(
	opts = list(pdf = list(orientation = "landscape"))
)
getClinDT(
	data = tableAE,
	colnames = tableAELabelsDT,
	# custom set of buttons
	buttons = buttons
)

# hide the first column:
getClinDT(
	data = tableAE,
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
getClinDT(
	data = tableAE,
	filter = "none",
	barVar = "perc",
	barRange = c(0, 100),
	pageLength = Inf,
	colnames = tableAELabelsDT,
	rowGroupVar = "AESOC",
	caption = caption
)