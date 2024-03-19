data <- endosulfan
two_fits <- get_fits(data, list("lnorm", "llogis"), FALSE)
bts <- get_bootstrap(two_fits)[[1]]

test_that("Fit parameters are given in html format", {
  html <- get_parameters_html(two_fits, bts)
  expect_match(html, "Log normal distribution")
  expect_match(html, "Log logistic distribution")
  html_no_bts <- get_parameters_html(two_fits)
  expect_no_match(html_no_bts, "[", fixed = TRUE)
})

test_that("HCx values are computed", {
  hcx <- get_HCx_table(two_fits, list("lnorm", "llogis"), bts)
  expect_s3_class(hcx, "data.frame")
  expect_equal(colnames(hcx), c("lnorm", "llogis"))
  expect_equal(rownames(hcx), c("HC5", "HC10", "HC20", "HC50"))
  # each cell should have same number of characters because all the numbers have
  # the same mantissa
  expect_true(all(apply(hcx, c(1,2), nchar) == 20))

  hcx_single <- get_HCx_table(two_fits[1], list("lnorm"), bts[1])
  expect_s3_class(hcx_single, "data.frame")
  expect_equal(colnames(hcx_single), "lnorm")
  expect_true(all(apply(hcx_single, c(1,2), nchar) == 20))

  hcx_no_bts <- get_HCx_table(two_fits, list("lnorm", "llogis"))
  expect_true(all(apply(hcx_no_bts, c(1,2), nchar) == 4))
})
