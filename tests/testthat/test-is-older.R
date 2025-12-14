test_that("Old files are older than newer files.", {
  old <- tempfile()
  cat("old", file = old)
  newer <- tempfile()
  cat("newer", file = newer)
  expect_true(is_older(old, newer))
})

