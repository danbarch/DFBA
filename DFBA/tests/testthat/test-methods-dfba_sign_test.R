# Sign test methods test

set.seed(77)
library(vdiffr)

measure_1 <- c(1.49, 0.64, 0.96, 2.34, 0.78, 1.29, 0.72, 1.52, 0.62, 1.67, 1.19, 0.86)
measure_2 <- c(0.53, 0.55, 0.58, 0.97, 0.60, 0.22, 0.05, 13.14, 0.63, 0.33, 0.91, 0.37)


set.seed(77)
object <- dfba_sign_test(Y1 = measure_1,
                         Y2 = measure_2)
test_that("Show method works",{
  expect_output(show(object))
})

test_that("Plot method works with prior",{
  expect_doppelganger(
    title = "sign_test_prior_plot",
    fig = plot(object)
  )
})

test_that("Plot method works with no prior",{
  expect_doppelganger(
    title = "sign_test_no_prior_plot",
    fig = plot(object,
               plot.prior = FALSE)
  )
})

