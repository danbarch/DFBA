# Test of bivariate concordance methods

# Phi

library(vdiffr)

set.seed(77)

mt1 <- c(45, 40, 48, 42, 45, 44, 40, 37, 27, 45, 51, 44, 44, 29, 27, 48, 33, 39, 47, 54, 32)
mt2 <- c(37, 41, 50, 46, 31, 39, 37, 42, 27, 48, 49, 52, 27, 36, 29, 44, 44, 43, 34, 48, 33)

object<-dfba_bivariate_concordance(x = mt1,
                                   y = mt2)


test_that("Show method works for phi",{
  expect_output(show(object))
})

test_that("Plot method works for phi",{
  expect_doppelganger(
    title = "bc_p",
    fig = plot(object),
  )
})

test_that("Plot method works for phi with no prior",{
  expect_doppelganger(
    title = "bc_no_p",
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
    title = "bc_star",
    fig = plot(objectstar),
  )
})

test_that("Plot method works for phi_star with no prior",{
  expect_doppelganger(
    title = "bc_no_p_star",
    fig = plot(objectstar,
               plot.prior = FALSE),
  )
})
