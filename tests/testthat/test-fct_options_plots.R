test_that("Uncensored data can be coloured by group", {
  data <- endosulfan
  fits <- get_fits(data, list("lnorm"), FALSE)
  p <- base_cdf(fits, "unit", TRUE)
  p <- suppressMessages(group_cdf_uncensored(data, p))
  expect_s3_class(p, "ggplot")

  group_p <- p$layers[[1]]$data$group[order(p$layers[[1]]$data$group)]

  group <- data$group[order(data$group)]
  expect_identical(group_p, group)
})

test_that("Censored data can have name labels displayed", {
  data <- fluazinam
  p <- cens_lines_plot(data, "unit", TRUE)
  p_names <- name_plot_censored(data, p)

  expect_s3_class(p_names, "ggplot")
  expect_length(p_names$layers, length(p$layers) + 1)

  names_p <- p_names$layers[[length(p_names$layers)]]$data$name
  names_p <- names_p[order(names_p)]

  expect_identical(names_p, data$name[order(data$name)])
})
