context("checkScalar")

test_that("checkScalar", {
  expect_true(testScalar(TRUE))
  expect_true(testScalar(1L))
  expect_true(testScalar(1))
  expect_true(testScalar(1+1i))
  expect_false(testScalar(list(1)))
  expect_false(testScalar(NA, na.ok = FALSE))
  expect_true(testScalar(NA, na.ok = TRUE))
  expect_error(assertScalar(integer(0)), "scalar")
})