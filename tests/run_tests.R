# =====================================================================
# run_tests.R
# Dependency-free test runner
# =====================================================================

test_files <- sort(
  list.files(
    "tests",
    pattern = "^test_.*\\.R$",
    full.names = TRUE
  )
)

if (
  length(test_files) == 0L
) {
  stop(
    "No test files found.",
    call. = FALSE
  )
}

cat(
  "Running",
  length(test_files),
  "test file(s)...\n\n"
)

for (test_file in test_files) {
  cat(
    "Running ",
    test_file,
    "...\n",
    sep = ""
  )

  source(
    test_file,
    local = new.env(
      parent = globalenv()
    )
  )

  cat("\n")
}

cat(
  "All tests passed.\n"
)
