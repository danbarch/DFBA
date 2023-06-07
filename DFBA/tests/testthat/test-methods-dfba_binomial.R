# Test of binomial test methods

library(vdiffr)

set.seed(77)


object<-dfba_binomial(n1 = 16,
                     n2 = 2)


test_that("Show method works",{
  expect_output(show(object))
})

test_that("Plot method works with prior",{
  expect_doppelganger(
    title = "binomial_plot",
    fig = plot(object),
  )
})

test_that("Plot method works without prior",{
  expect_doppelganger(
    title = "binomial_noprior_plot",
    fig = plot(object,
               plot.prior = FALSE),
  )
})
