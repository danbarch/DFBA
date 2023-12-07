# Test of beta descriptive methods


library(vdiffr)

set.seed(77)


object<-dfba_beta_descriptive(a = 38,
                              b = 55)


test_that("Show method works",{
  expect_output(show(object))
})

test_that("Plot method works",{
  expect_doppelganger(
    title = "beta_descriptive_plot",
    fig = plot(object),
  )
})

