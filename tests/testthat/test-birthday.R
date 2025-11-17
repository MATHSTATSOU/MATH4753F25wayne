test_that("the birthday function works", {
  expect_gt(birthday(23), 0.40)
  expect_gt(birthday(30), 0.50)
})
