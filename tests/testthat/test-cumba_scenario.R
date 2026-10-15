test_that("cumba_scenario returns a valid data frame", {
  data(tomatoFoggia)
  data(cumbaParameters)
  yr  <- max(lubridate::year(tomatoFoggia$weather$DATE))
  wx1 <- tomatoFoggia$weather[lubridate::year(tomatoFoggia$weather$DATE) == yr, ]

  invisible(utils::capture.output(
    out <- cumba_scenario(
      weather          = wx1,
      param            = cumbaParameters,
      estimateRad      = TRUE,
      estimateET0      = TRUE,
      transplantingDOY = 120,
      irrigationStrategy = list(
        vegetative   = list(wsLevel = 0.5, turnMin = 4),
        reproductive = list(wsLevel = 0.5, turnMin = 4),
        ripening     = list(wsLevel = 0.5, turnMin = 4)
      )
    )
  ))

  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(c("site", "year", "yield", "brix") %in% names(out)))

  last <- out[nrow(out), ]
  expect_gt(last$yield, 0)
  expect_true(last$brix > 0 && last$brix < 15)
  expect_false(anyNA(out$yield))
})

test_that("higher wsLevel gives higher yield and lower Brix (wetter soil)", {
  skip_on_cran()
  data(tomatoFoggia)
  data(cumbaParameters)
  wx <- tomatoFoggia$weather

  run_ws <- function(ws) {
    invisible(utils::capture.output(
      r <- cumba_scenario(
        weather          = wx,
        param            = cumbaParameters,
        transplantingDOY = 120,
        estimateRad      = TRUE,
        estimateET0      = TRUE,
        irrigationStrategy = list(
          vegetative   = list(wsLevel = ws, turnMin = 4),
          reproductive = list(wsLevel = ws, turnMin = 4),
          ripening     = list(wsLevel = ws, turnMin = 4)
        )
      )
    ))
    h <- dplyr::ungroup(dplyr::slice_tail(dplyr::group_by(r, site, year), n = 1))
    c(yield = mean(h$yield, na.rm = TRUE), brix = mean(h$brix, na.rm = TRUE))
  }

  full <- run_ws(0.8)
  defi <- run_ws(0.3)
  expect_gt(full[["yield"]], defi[["yield"]])
  expect_lt(full[["brix"]],  defi[["brix"]])
})
