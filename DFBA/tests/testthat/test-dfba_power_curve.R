# Tests for power curve

# Error tests

test_that("Missing a0 parameter produces stop error",{
  expect_error(dfba_power_curve(a0 = NA,
                                n = 70,
                                model = "normal",
                                design = "paired",
                                samples = 150),
               "Both a0 and b0 must be positive and finite")
})

test_that("Missing b0 parameter produces stop error",{
  expect_error(dfba_power_curve(b0 = NA,
                                n = 70,
                                model = "normal",
                                design = "paired",
                                samples = 150),
               "Both a0 and b0 must be positive and finite")
})

test_that("Negative delta_step produces stop error",{
  expect_error(dfba_power_curve(n = 70,
                                model = "normal",
                                design = "paired",
                                samples = 150,
                                delta_step = -2),
               "The function requires a nonnegative value for delta.")
})

test_that("Negative block_max produces stop error",{
  expect_error(dfba_power_curve(n = 70,
                                model = "normal",
                                design = "paired",
                                samples = 150,
                                block_max = -2),
               "block_max must be nonnegative")
})

test_that("too small n",{
  expect_error(dfba_power_curve(n = 7,
                                model = "normal",
                                design = "paired",
                                samples = 150),
               "The function requires n to be an integer that is 20 or larger")
})

test_that("model is not in the list",{
  mlist<-c("normal",
           "weibull",
           "cauchy",
           "lognormal",
           "chisquare",
           "logistic",
           "exponential",
           "gumbel",
           "pareto")
  modelstop <- paste0("The set of distributions for model are:"," ","\n",
                      "\t\t", paste0(mlist, collapse = "\n\t\t"), "\n",
                      "The stipulated model is not on the list")
  expect_error(dfba_power_curve(n = 70,
                                model = "zoolander",
                                design = "paired",
                                samples = 150),
               modelstop)
})


test_that("design is not in the list",{
  designlist<-c("paired",
                "independent")
  designstop <- paste0("The set of distributions for design are:"," ","\n",
                       "\t\t", paste0(designlist, collapse = "\n\t\t"), "\n",
                       "The stipulated design is not on the list")
  expect_error(dfba_power_curve(n = 70,
                                model = "normal",
                                design = "pairedddd",
                                samples = 150),
               designstop)
})

test_that("Effect_crit outside of [0, 1] produces stop error",{
  expect_error(dfba_power_curve(n = 70,
                                model = "normal",
                                design = "paired",
                                samples = 150,
                                effect_crit = 42),
               "The effect_crit value must be a nonzero number less than 1.")
})
# Function tests
APowercurve <- dfba_power_curve(n = 70,
                                  model = "normal",
                                  design = "paired",
                                  samples = 150)

    bayes70c <- APowercurve$outputdf$Bayes_power[15]

    t70c <- APowercurve$outputdf$t_power[15]

    test_that("Spot check on Bayesian estimates",{
      expect_lte(abs(bayes70c - 0.99034), 0.07)
    })

    test_that("Spot check on frequentist power estimates",{
      expect_lte(abs(t70c - 0.99249), 0.07)
    })

