# Mann-whitney (large) methods test

# Large method test data
E <- c(1.61, 2.02, 2.34, 2.89, 4.51, 4.72, 4.86, 6.21, 9.50, 33.39)
C <- c(1.11, 1.13, 1.32, 1.82, 1.99, 4.12, 6.28, 6.21, 8.24, 44.55)
Elarge<-rep(E, 3)
Clarge<-rep(C, 3)

set.seed(77)
library(vdiffr)

object <- dfba_mann_whitney(Elarge,
                            Clarge)
test_that("Show method works",{
  expect_output(show(object))
})

test_that("Plot method works with prior",{
  expect_doppelganger(
    title = "mw_large_prior_plot",
    fig = plot(object)
  )
})

test_that("Plot method works with no prior",{
  expect_doppelganger(
    title = "mw_large_no_prior_plot",
    fig = plot(object,
               plot.prior = FALSE)
  )
})
