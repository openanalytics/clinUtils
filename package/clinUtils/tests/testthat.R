library(testthat)
library(clinUtils)

if (Sys.getenv("TESTTHAT_OUTPUT_FILE") != "")
	options(testthat.output_file = Sys.getenv("TESTTHAT_OUTPUT_FILE", stdout()))
test_check(
	"clinUtils",
	reporter = Sys.getenv("TESTTHAT_DEFAULT_CHECK_REPORTER", "check")
)
