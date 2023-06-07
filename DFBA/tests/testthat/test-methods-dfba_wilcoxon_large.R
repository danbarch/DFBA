# Wilcoxon (large) methods test

set.seed(77)
library(vdiffr)

# data vectors for the large sample cases.

Y1<-c(2.286,0.621,0.967,3.782,18.960,5.473,4.274,0.605)
Y2<-c(1.114,0.002,0.382,1.251,0.003,8.413,7.947,0.050)
Y1L<-rep(Y1, 4)
Y2L<-rep(Y2, 4)

# Error tests


set.seed(77)
object <- dfba_wilcoxon(Y1L,
                        Y2L)
test_that("Show method works",{
  expect_output(show(object))
})

test_that("Plot method works with prior",{
  expect_doppelganger(
    title = "wilcoxon_large_prior_plot",
    fig = plot(object)
  )
})

test_that("Plot method works with no prior",{
  expect_doppelganger(
    title = "wilcoxon_large_no_prior_plot",
    fig = plot(object,
               plot.prior = FALSE)
  )
})

