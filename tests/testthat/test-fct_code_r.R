test_that("R code can be generated", {
  data <- endosulfan
  distributions <- list("lnorm", "llogis")
  code <- code_r_ssd(data, distributions)

  expect_type(code, "character")
  expect_true(nchar(code) > 1)
})
