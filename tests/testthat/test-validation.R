test_that("invalid cardinal temperatures raise an error", {
  data(tomatoFoggia)
  data(cumbaParameters)
  bad <- cumbaParameters
  bad$Tmax$value <- 10   # Tmax < Topt
  expect_error(suppressWarnings(invisible(utils::capture.output(
    cumba_scenario(weather = tomatoFoggia$weather, param = bad)
  ))))
})

test_that("field capacity below wilting point raises an error", {
  data(tomatoFoggia)
  data(cumbaParameters)
  bad <- cumbaParameters
  bad$FieldCapacity$value <- 0.10
  bad$WiltingPoint$value  <- 0.30
  expect_error(suppressWarnings(invisible(utils::capture.output(
    cumba_scenario(weather = tomatoFoggia$weather, param = bad)
  ))))
})

test_that("missing weather columns raise an error", {
  data(cumbaParameters)
  wx_bad <- data.frame(Site = "X", Tn = 10, DATE = as.Date("2025-05-01"))
  expect_error(suppressWarnings(invisible(utils::capture.output(
    cumba_scenario(weather = wx_bad, param = cumbaParameters)
  ))))
})
