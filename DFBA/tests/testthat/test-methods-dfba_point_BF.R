# Beta Point BF Methods Test
## Tests the output for beta_bayes_factor when method = "point"
set.seed(77)
object <- dfba_beta_bayes_factor(a_post = 17,
                                 b_post = 5,
                                 method = "point",
                                 H0 = .5)

test_that("Show method works",{
  expect_output(show(object))
})

