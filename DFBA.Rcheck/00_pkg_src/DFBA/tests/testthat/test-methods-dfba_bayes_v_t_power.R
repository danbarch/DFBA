# Bayes v t Power Methods Test

library(vdiffr)

set.seed(77)
object <- dfba_bayes_vs_t_power(n_min = 20,
                               model = "weibull",
                               delta = 0.8,
                               shape1 = 0.8,
                               shape2 = 0.8,
                               design = "paired",
                               samples = 300)

test_that("Show method works",{
  expect_output(show(object))
})

test_that("Plot method works",{
  expect_doppelganger(
    title = "bayes_vs_t_power_plot",
    fig = plot(object),
  )
})

