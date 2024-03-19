data <- endosulfan
fits <- get_fits(data, list("lnorm", "llogis"), FALSE)
data_cens <- salinity_family
fits_cens <- get_fits(data_cens, list("lnorm", "llogis"), TRUE)

test_that("Data can be plotted with a basic cdf from {fitdistrplus}", {
  plot_base <- base_cdf(fits, "unit", TRUE)
  plot_name <- base_cdf(fits, "unit", TRUE, names = data$name)
  plot_no_h <- base_cdf(fits, "unit", TRUE, horizontals = FALSE)
  plot_xlim <- base_cdf(fits, "unit", TRUE, xlim = c(0.1, 1000))
  plot_1_ft <- base_cdf(fits[1], "unit", TRUE)
  plot_cens <- base_cdf(fits_cens, "unit", TRUE)

  expect_s3_class(plot_base, "ggplot")
  expect_s3_class(plot_name, "ggplot")
  expect_s3_class(plot_no_h, "ggplot")
  expect_s3_class(plot_xlim, "ggplot")
  expect_s3_class(plot_1_ft, "ggplot")
  expect_s3_class(plot_cens, "ggplot")

  # Number of layers (elements in the plot)
  # base plot: 5 (data, horizontals, 2 invisible segments, fits)
  expect_length(plot_base$layers, 5)
  # plot_name: 6 (base + name labels)
  expect_length(plot_name$layers, 6)
  # plot_no_h: 2 (data, fits)
  expect_length(plot_no_h$layers, 2)
  # plot_cens: 4 (grey rectangles, horizontal segments, vertical segments, fits)
  expect_length(plot_cens$layers, 4)
})

test_that("Raw censored data can be plotted with a lines plot", {
  plot_cens <- cens_lines_plot(data_cens, "unit", TRUE)
  plot_fits <- cens_lines_plot(data_cens, "unit", TRUE, fits = fits_cens)
  plot_rlim <- cens_lines_plot(data_cens, "unit", TRUE, rightNA = 55)
  plot_group <- cens_lines_plot(data_cens, "unit", TRUE, color_group = TRUE)

  expect_s3_class(plot_cens, "ggplot")
  expect_s3_class(plot_fits, "ggplot")
  expect_s3_class(plot_rlim, "ggplot")
  expect_s3_class(plot_group, "ggplot")

  expect_length(plot_cens$layers, 2)
  expect_length(plot_fits$layers, 3)

  expect_identical(plot_rlim$coordinates$limits$x[2], 55)
})
