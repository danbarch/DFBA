# Small method tests

set.seed(77)
library(vdiffr)

# data for small method tests

Y1<-c(2.286,0.621,0.967,3.782,18.960,5.473,4.274,0.605)
Y2<-c(1.114,0.002,0.382,1.251,0.003,8.413,7.947,0.050)

object <- dfba_wilcoxon(Y1,
                        Y2,
                        samples=500)
test_that("Show method works",{
  expect_output(show(object))
})

test_that("Plot method works with prior",{
  expect_doppelganger(
    title = "wilcoxon_small_prior_plot",
    fig = plot(object)
  )
})

test_that("Plot method works with no prior",{
  expect_doppelganger(
    title = "wilcoxon_small_no_prior_plot",
    fig = plot(object,
               plot.prior = FALSE)
  )
})

