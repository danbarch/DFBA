# McNemar methods test

set.seed(77)
library(vdiffr)

object <- dfba_mcnemar(n_01 = 17,
                       n_10 = 2)
test_that("Show method works",{
  expect_output(show(object))
})

test_that("Plot method works with prior",{
  expect_doppelganger(
    title = "mcnemar_prior_plot",
    fig = plot(object)
  )
})

test_that("Plot method works without prior",{
  expect_doppelganger(
    title = "mcnemar_noprior_plot",
    fig = plot(object,
               plot.prior = FALSE)
  )
})
