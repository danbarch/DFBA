# Power curve methods test

set.seed(77)
library(vdiffr)

object <- dfba_power_curve(n = 70,
                           model = "normal",
                           design = "paired",
                           samples = 150)
test_that("Show method works",{
  expect_output(show(object))
})

test_that("Plot method works",{
  expect_doppelganger(
    title = "power_curve_plot",
    fig = plot(object),
  )
})
