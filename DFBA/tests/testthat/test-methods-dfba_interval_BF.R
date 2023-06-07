# Beta Interval BF Methods Test
## Tests the output for beta_bayes_factor when method = "interval"

set.seed(77)
object <- dfba_beta_bayes_factor(a = 17,
                                 b = 5,
                                 method = "interval",
                                 H0 = c(0, .5))

test_that("Show method works",{
  expect_output(show(object))
})
