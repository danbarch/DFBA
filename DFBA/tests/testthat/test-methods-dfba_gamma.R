# Test of gamma methods

library(vdiffr)
set.seed(77)

## test data

gamma_test_matrix <- matrix(c(38, 4, 5, 0, 6, 40, 1, 2, 4, 8, 20, 30),
                            ncol = 4,
                            byrow = TRUE)


object<-dfba_gamma(gamma_test_matrix)


test_that("Show method works",{
  expect_output(show(object))
})

test_that("Plot method works with prior",{
  expect_doppelganger(
    title = "gamma_plot",
    fig = plot(object),
  )
})

test_that("Plot method works with no prior",{
  expect_doppelganger(
    title = "gamma_plot_no_prior",
    fig = plot(object,
               plot.prior = FALSE),
  )
})

# Phi_star

p <- seq(.05,.95,.05)
ypred <- 17.332-(50.261*p) + (48.308*p^2)

#  # Note the coefficients in the ypred equation were found first
#  # via a polynomial regression
yobs <- c(19.805, 10.105, 9.396, 8.219, 6.110, 4.543, 5.864, 4.861, 6.136,
          5.789,  5.443, 5.548, 4.746, 6.484, 6.185, 6.202, 9.804, 9.332,
          14.408)

objectstar <- dfba_bivariate_concordance(x = yobs,
                                         y = ypred,
                                         fitting.parameters = 3)

test_that("Show method works for phi_star",{
  expect_output(show(objectstar))
})

test_that("Plot method works for phi_star",{
  expect_doppelganger(
    title = "bivariate_concordance_star_plot",
    fig = plot(objectstar),
  )
})

test_that("Plot method works for phi_star with no prior",{
  expect_doppelganger(
    title = "bivariate_concordance_star_plot_no_prior",
    fig = plot(objectstar,
               plot.prior = FALSE),
  )
})
