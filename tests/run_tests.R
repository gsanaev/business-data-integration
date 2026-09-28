# =====================================================================
# run_tests.R
# testthat runner for the v3.0 test suite
# =====================================================================

testthat::test_dir(
  "tests/testthat",
  reporter = "summary"
)
