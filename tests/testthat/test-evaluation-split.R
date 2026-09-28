suppressPackageStartupMessages(
  library(dplyr)
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_candidates.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_features.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_similarity.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "evaluation_split.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "evaluation_benchmark.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "evaluation_policy.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "evaluation_heldout.R"
  )
)


testthat::test_that(
  "enterprise split is reproducible and has the frozen 70/30 size",
  {
    ids <-
      sprintf(
        "F%05d",
        1:1500
      )

    split_1 <-
      create_enterprise_split(
        ids,
        development_share = 0.70,
        seed = 202604L
      )

    split_2 <-
      create_enterprise_split(
        rev(ids),
        development_share = 0.70,
        seed = 202604L
      )

    testthat::expect_identical(
      split_1,
      split_2
    )

    testthat::expect_equal(
      sum(
        split_1$sample_role ==
          "development"
      ),
      1050L
    )

    testthat::expect_equal(
      sum(
        split_1$sample_role ==
          "heldout"
      ),
      450L
    )

    testthat::expect_setequal(
      split_1$truth_firm_id,
      ids
    )

    testthat::expect_false(
      anyDuplicated(
        split_1$truth_firm_id
      ) > 0L
    )

    testthat::expect_silent(
      validate_enterprise_split(
        split_1,
        ids,
        development_share = 0.70
      )
    )
  }
)


testthat::test_that(
  "enterprise split rejects invalid specifications",
  {
    ids <-
      sprintf(
        "F%05d",
        1:10
      )

    testthat::expect_error(
      create_enterprise_split(
        ids,
        development_share = 1
      ),
      "strictly between 0 and 1"
    )

    split <-
      create_enterprise_split(
        ids,
        development_share = 0.70,
        seed = 202604L
      )

    invalid_split <-
      split

    invalid_split$sample_role[1] <-
      "other"

    testthat::expect_error(
      validate_enterprise_split(
        invalid_split,
        ids,
        development_share = 0.70
      ),
      "invalid sample roles"
    )
  }
)


