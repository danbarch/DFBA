# Beta Contrast Methods Test

## Test data
n1_vec<-c(22, 15, 13, 21)
n2_vec<-c(18, 25, 27, 19)
ABcontrast<-c(.5, -.5, -.5, .5)

set.seed(77)
object <- dfba_beta_contrast(n1_vec = n1_vec,
                             n2_vec = n2_vec,
                             contrast_vec = ABcontrast)

test_that("Show method works",{
  expect_output(show(object))
})

library(vdiffr)
test_that("Plot method works",{
  expect_doppelganger(
    title = "beta_contrast_plot",
    fig = plot(object),
  )
})
