# This contains a series of tests to ensure the functions in cp.R are

test_that("Credits Test", {
  expect_true(!is.na(cgr::credits()))
})
