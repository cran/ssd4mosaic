test_that("Pointwise data can be fitted", {
  data <- endosulfan
  dist <- list("lnorm", "llogis")
  fits <- get_fits(data, dist, FALSE)
  expect_length(fits, length(dist))
  expect_s3_class(fits[[1]], "fitdist")
  expect_equal(lapply(fits, "[[", "distname"), dist)
})

test_that("Censored data can be fitted", {
  data <- salinity_family
  dist <- list("lnorm", "llogis")
  fits <- get_fits(data, dist, TRUE)
  expect_length(fits, length(dist))
  expect_s3_class(fits[[1]], "fitdistcens")
  expect_equal(lapply(fits, "[[", "distname"), dist)
})
