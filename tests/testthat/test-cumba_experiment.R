test_that("cumba_experiment returns a valid data frame", {
  data(tomatoFoggia)
  data(cumbaParameters)
  yr  <- max(lubridate::year(tomatoFoggia$weather$DATE))
  wx1 <- tomatoFoggia$weather[lubridate::year(tomatoFoggia$weather$DATE) == yr, ]

  invisible(utils::capture.output(
    ex <- cumba_experiment(
      weather              = wx1,
      param                = cumbaParameters,
      irrigation_df        = tomatoFoggia$irrigation,
      estimateRad          = TRUE,
      estimateET0          = TRUE,
      irrigationEfficiency = 0.9
    )
  ))

  expect_s3_class(ex, "data.frame")
  expect_gt(nrow(ex), 0)
  expect_true(all(c("site", "year", "experiment", "yield", "brix") %in% names(ex)))
})

test_that("cumba_experiment reproduces observed yield and Brix", {
  skip_on_cran()
  data(tomatoFoggia)
  data(cumbaParameters)

  invisible(utils::capture.output(
    ex <- cumba_experiment(
      weather              = tomatoFoggia$weather,
      param                = cumbaParameters,
      irrigation_df        = tomatoFoggia$irrigation,
      estimateRad          = TRUE,
      estimateET0          = TRUE,
      irrigationEfficiency = 0.9
    )
  ))

  harvest <- dplyr::ungroup(dplyr::slice_tail(
    dplyr::group_by(ex, site, year, experiment), n = 1))
  cmp <- dplyr::inner_join(
    dplyr::mutate(harvest, experiment = as.integer(experiment)),
    tomatoFoggia$production, by = c("experiment" = "ID"))
  cmp$yield_t <- cmp$yield / 10

  expect_gt(stats::cor(cmp$yield_t, cmp$yield_ref, use = "complete.obs"), 0.6)
  expect_gt(stats::cor(cmp$brix,    cmp$brix_ref,  use = "complete.obs"), 0.3)
})
