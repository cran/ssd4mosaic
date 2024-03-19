test_that("Ordering of censored data is correct", {
  data <- data.frame(
    left = c(5, NA, 6, 3, 3, NA, 12),
    right = c(10, 7, 6, 21, NA, 3, NA),
    other = c(7.5, 7, 6, 12, 3, 3, 12)
  )
  ordered <- order_cens_data(data)
  # reset row names
  row.names(ordered) <- NULL
  expect_identical(dim(ordered), dim(data))
  expected <- data.frame(
    left = c(NA, NA, 6, 5, 3, 3, 12),
    right = c(3, 7, 6, 10, 21, NA, NA),
    other = c(3, 7, 6, 7.5, 12, 3, 12)
  )
  expect_identical(ordered, expected)
})
