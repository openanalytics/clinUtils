context("Get labels of variables in SAS datasets")

test_that("Get label of one specific variable in a dataset", {
			
	data <- data.frame(
		USUBJID = structure(sample.int(5), label = "Subject ID"),
		stringsAsFactors = FALSE
	)
	var <- head(colnames(data), 1)
	
	# var
	expect_identical(
		object = getLabelVar(var = var),
		expected = setNames(var, var),
		info = "label variable set to variable itself"
	)
	
	# data
	expect_identical(
		object = getLabelVar(var = var, data = data),
		expected = setNames(attr(data[[var]], "label"), var),
		info = "label variable not extracted from dataset"
	)
	
	dataWithoutLabels <- data
	attr(dataWithoutLabels[[var]], "label") <- NULL
	expect_identical(
		object = getLabelVar(var = var, data = dataWithoutLabels),
		expected = setNames(var, var),
		info = "label variable not set to var"
	)
	
	# labelVars
	labelCustom <- setNames("test", var)
	expect_identical(
		object = getLabelVar(var = var, data = data, labelVars = labelCustom),
		expected = labelCustom,
		info = "label variable not extracted from 'labelVars'"
	)
	
	# label
	label <- setNames("test", var)
	expect_identical(
		object = getLabelVar(var = var, data = data, label = label),
		expected = label,
		info = "label variable extracted from named 'label'"
	)
	label <- "test"
	expect_identical(
		object = getLabelVar(var = var, data = data, label = label),
		expected = setNames(label, var),
		info = "label variable extracted from 'label'"
	)
			
})

test_that("Get label of multiple specific variables in a dataset", {

	data <- data.frame(
		USUBJID = structure(sample.int(5), label = "Subject ID"),
		TRT = structure(c("A", "B", "A", "B", "A"), label = "Treatment"),
		stringsAsFactors = FALSE
	)
	vars <- head(colnames(data))
	varsLabels <- getLabelVar(var = vars, data = data)
	
	expect_identical(
		object = varsLabels,
		expected = sapply(data[, vars], attr, "label")
	)
	expect_length(varsLabels, length(vars))
			
})

test_that("Get label of all variables in datasets", {

	data1 <- data.frame(
		USUBJID = structure(sample.int(5), label = "Subject ID"),
		TRT = structure(c("A", "B", "A", "B", "A"), label = "Treatment"),
		stringsAsFactors = FALSE
	)
	data2 <- data.frame(
		USUBJID = structure(sample.int(5), label = "Subject ID"),
		AVAL = structure(rnorm(5), label = "Analysis value"),
		stringsAsFactors = FALSE	
	)
	dataList <- list(data1, data2)
		
	dataListLabels <- getLabelVars(data = dataList)
	
	expect_is(dataListLabels, "character")
	expect_named(dataListLabels)
	
	# all variables have labels (beside 'DATASET')
	allVars <- setdiff(unlist(sapply(dataList, names)), "DATASET")
	expect_setequal(allVars, names(dataListLabels))
	
	expect_warning(getLabelVars(dataList = dataList))
		
})
		

test_that("Get label from parameter codes", {
			
	# for ADaM dataset
	dataADaM <- data.frame(
		USUBJID = sample.int(6), 
		PARAM = rep(c("Cholesterol", "Triglycerides"), each = 3),
		PARAMCD = rep(c("CHOL", "TRIGL"), each = 3),
		stringsAsFactors = FALSE
	)
	paramcdsADaM <- unique(dataADaM$PARAMCD)
	expect_silent(paramLabelsADaM <- getLabelParamcd(paramcd = paramcdsADaM, data = dataADaM))
	expect_is(paramLabelsADaM, "character")
	expect_named(paramLabelsADaM)
	expect_identical(names(paramLabelsADaM), paramcdsADaM)
	expect_equal(unname(paramLabelsADaM), dataADaM[match(paramcdsADaM, dataADaM$PARAMCD), "PARAM"])
	
	# for SDTM (custom variables)
	dataSDTM <- data.frame(
		USUBJID = sample.int(6), 
		LBTEST = rep(c("Cholesterol", "Triglycerides"), each = 3),
		LBTESTCD = rep(c("CHOL", "TRIGL"), each = 3),
		stringsAsFactors = FALSE
	)
	paramcdsSDTM <- unique(dataSDTM$LBTESTCD)
	expect_error(paramLabels <- getLabelParamcd(paramcd = paramcdsSDTM, data = dataSDTM))
	expect_silent(paramLabelsSDTM <- getLabelParamcd(
		paramcd = paramcdsSDTM, 
		data = dataSDTM, 
		paramcdVar = "LBTESTCD", paramVar = "LBTEST"
	))
	expect_identical(names(paramLabelsSDTM), paramcdsSDTM)
	
})
	
			