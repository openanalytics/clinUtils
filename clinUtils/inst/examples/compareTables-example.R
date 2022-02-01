## Example 1
# In this case the referenceVar 'a' is the same
# the comparison highlights only as change in the variables 'c' and 'd' 
newData <- data.frame(
	"a" = c(1, 2, 3, 4),
	"b" = c(5, 6, 7, 8),
	"c" = rep(1, 4),
	"d" = rep(2, 4)
)
oldData <- data.frame(
	"a" = c(1, 2, 3, 4),
	"b" = c(3, 4, 7, 8),
	"c" = rep(2, 4),
	"d" = rep(1, 4)
)
compareTables(
	newData = newData,
	oldData = oldData,
	referenceVars = "a",
	changeableVars = c("c", "d")
)

## Example 2
# In this case the referenceVar 'a' changes in the last two rows
# the comparison highlights as change the second and third rows in the variables 'c' and 'd'
# whereas the last rows are additions/removals with respect to the reference 'a'
newData <- data.frame(
		"a" = c(7, 1, 2, 3, 4),
		"b" = c(2, 1, 6, 7, 8),
		"c" = rep(1, 5),
		"d" = rep(2, 5)
)
oldData <- data.frame(
		"a" = c(7, 1, 2, 5, 6),
		"b" = c(2, 3, 4, 7, 8),
		"c" = c(1, rep(2, 4)),
		"d" = c(2, rep(1, 4))
)
compareTables(
		newData = newData,
		oldData = oldData,
		referenceVars = "a",
		changeableVars = c("c", "d")
)

## Example 3
# In this case the referenceVar 'a' is the same
# also the variable 'c' is the same and it's the only changeable var evaluated
newData <- data.frame(
		"a" = c(1, 2, 3, 4),
		"b" = c(5, 6, 7, 8),
		"c" = rep(1, 4),
		"d" = rep(2, 4)
)
oldData <- data.frame(
		"a" = c(1, 2, 3, 4),
		"b" = c(3, 4, 7, 8),
		"c" = rep(1, 4),
		"d" = rep(1, 4)
)
compareTables(
		newData = newData,
		oldData = oldData,
		referenceVars = "a",
		changeableVars = "c"
)

## In case only a specific output should be returned:

newData <- data.frame(
	"a" = c(7, 1, 2, 3, 4),
	"b" = c(2, 1, 6, 7, 8),
	"c" = rep(1, 5),
	"d" = rep(2, 5)
)
oldData <- data.frame(
	"a" = c(7, 1, 2, 5, 6),
	"b" = c(2, 3, 4, 7, 8),
	"c" = c(1, rep(2, 4)),
	"d" = c(2, rep(1, 4))
)

# get only the differences between datasets:

# as a data.frame
compareTables(newData = newData, oldData = oldData, 
	referenceVars = "a", changeableVars = c("c", "d"), 
	outputType = "table-comparison")

# as an interactive DataTable
compareTables(newData = newData, oldData = oldData, 
	referenceVars = "a", changeableVars = c("c", "d"), 
	outputType = "table-comparison-interactive"
)
# only the new data
compareTables(
    newData = newData, oldData = oldData, 
	referenceVars = "a", changeableVars = c("c", "d"), 
	outputType = "newData-diff"
)
# only the new data in interactive mode
compareTables(
    newData = newData, oldData = oldData, 
    referenceVars = "a", changeableVars = c("c", "d"), 
    outputType = "newData-diff-interactive"
)
# only the new data in static and interactive mode
compareTables(
    newData = newData, oldData = oldData, 
    referenceVars = "a", changeableVars = c("c", "d"), 
    outputType = c("newData-diff", "newData-diff-interactive")
)
# only the old data
compareTables(newData = newData, oldData = oldData, 
	referenceVars = "a", changeableVars = c("c", "d"), 
	outputType = "oldData-diff"
)
# only the old data in interactive mode
compareTables(
    newData = newData, oldData = oldData, 
    referenceVars = "a", changeableVars = c("c", "d"), 
    outputType = "oldData-diff-interactive"
)
# only the old data in static and interactive mode
compareTables(
    newData = newData, oldData = oldData, 
    referenceVars = "a", changeableVars = c("c", "d"), 
    outputType = c("oldData-diff", "oldData-diff-interactive")
)
## no changeable vars

newData <- data.frame(
	"a" = c(7, 1, 2, 3, 4),
	"b" = c(2, 1, 6, 7, 8),
	"c" = rep(1, 5),
	"d" = rep(2, 5)
)
oldData <- data.frame(
	"a" = c(7, 1, 2, 5, 6),
	"b" = c(2, 3, 4, 7, 8),
	"c" = c(1, rep(2, 4)),
	"d" = c(2, rep(1, 4))
)

compareTables(newData = newData, oldData = oldData, 
	referenceVars = "a"
)

## duplicated records

# in case there are multiple records for the same reference variables,
# identical records are flagged as 'Identity' and reported in the table
# reporting differences; and the different record are flagged as 'Change', 'Addition' or 'Removal'
newData <- data.frame(
	"a" = c(7, 7),
	"b" = c(1, 2),
	"c" = c(1, 2),
	"d" = c(2, 3)
)
oldData <- data.frame(
	"a" = c(7, 7, 7),
	"b" = c(3, 4, 5),
	"c" = c(1, 3, 5),
	"d" = c(2, 4, 6)
)
compareTables(
	newData = newData, oldData = oldData, 
	referenceVars = "a", changeableVars = c("c", "d"), 
)

## with labels in the interactive format, see ? getClinDT

newData <- data.frame(
	"a" = c(7, 1, 2, 3, 4),
	"b" = c(2, 1, 6, 7, 8),
	"c" = rep(1, 5),
	"d" = rep(2, 5)
)
oldData <- data.frame(
	"a" = c(7, 1, 2, 5, 6),
	"b" = c(2, 3, 4, 7, 8),
	"c" = c(1, rep(2, 4)),
	"d" = c(2, rep(1, 4))
)
compareTables(
	newData = newData,
	oldData = oldData,
	referenceVars = "a",
	changeableVars = c("c", "d"),
	# parameters passed to datatable
	colnames = c(
		"My reference variable" = "a", 
		"Changeable variable c" = "c",
		"Changeable variable d" = "d"
	)
)

