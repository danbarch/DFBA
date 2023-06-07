#Mann-whitney (small) methods test

# Small method test data
E <- c(1.61, 2.02, 2.34, 2.89, 4.51, 4.72, 4.86, 6.21, 9.50, 33.39)
C <- c(1.11, 1.13, 1.32, 1.82, 1.99, 4.12, 6.28, 6.21, 8.24, 44.55)

set.seed(77)
library(vdiffr)

object <- dfba_mann_whitney(E,
                            C,
                            samples=10000)
test_that("Show method works",{
  expect_output(show(object))
})

test_that("Plot method works with prior",{
  expect_doppelganger(
    title = "mw_small_prior_plot",
    fig = plot(object)
  )
  })

test_that("Plot method works with no prior",{
  expect_doppelganger(
    title = "mw_small_no_prior_plot",
    fig = plot(object,
               plot.prior = FALSE)
  )
})
