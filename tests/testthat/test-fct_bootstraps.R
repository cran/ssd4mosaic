fit <- get_fits(endosulfan, list("lnorm"), FALSE)[[1]]
bts <- lapply(seq_len(5), bootdist_fun, ft = fit)

test_that("Appropriate bootstrap function is selected", {
  expect_s3_class(bts[[1]], "bootdist")
})

test_that("Bootstraps are combined", {
  combined <- combine_boot_samples(bts)
  # correct object type
  expect_s3_class(combined, "bootdist")
  # information on the base fit retained
  expect_identical(combined$fitpart, bts[[1]]$fitpart)
  # all iterations have been combined
  expect_equal(combined$nbboot, sum(unlist(lapply(bts, "[[", "nbboot"))))
})

test_that("Bootstraps are computed with convergence", {
  bootstrap <- get_bootstrap(list(fit))
  expect_length(bootstrap, 2)
  expect_length(bootstrap[[1]], 1)
  expect_length(bootstrap[[2]], 1)
  expect_s3_class(bootstrap[[1]][[1]], "bootdist")
  expect_type(bootstrap[[2]][[1]], "logical")
})
