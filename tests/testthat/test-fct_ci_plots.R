data <- endosulfan
fits <- get_fits(data, list("lnorm", "llogis"), FALSE)
bts <- get_bootstrap(fits)[[1]]
data_cens <- salinity_family
fits_cens <- get_fits(data_cens, list("lnorm"), TRUE)
bts_cens <- get_bootstrap(fits_cens)[[1]]

test_that("Confidence intervals are added to plots", {
  ci_plot_utils <- my_CIcdfplot(bts[[1]], FALSE, CI.level = 0.95)
  ci_plot_utils_cens <- my_CIcdfplot(bts_cens[[1]], FALSE, CI.level = 0.95)
  expect_s3_class(ci_plot_utils, "ggplot")
  expect_s3_class(ci_plot_utils_cens, "ggplot")
  # check truthfulness of documentation's note
  expect_length(ci_plot_utils$layers, 8)
  expect_length(ci_plot_utils_cens$layers, 7)

  plot <- base_cdf(fits, "unit", TRUE)
  plot_ci <- add_CI_plot(plot, bts, TRUE)
  plot_cens <- base_cdf(fits_cens, "unit", TRUE)
  plot_cens_ci <- add_CI_plot(plot_cens, bts_cens, TRUE)

  # Add 3 layers per bootstrap object
  expect_length(plot_ci$layers, length(plot$layers) + 6)
  expect_length(plot_cens_ci$layers, length(plot_cens$layers) + 3)

  # base information of the plot is the same (partial)
  expect_identical(plot_ci$data, plot$data)
  expect_identical(plot_cens_ci$data, plot_cens$data)
})
