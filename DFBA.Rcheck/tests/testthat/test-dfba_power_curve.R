  APowercurve <- dfba_power_curve(n = 70,
                                  model = "normal",
                                  design = "paired",
                                  samples = 150)

    bayes70c = APowercurve$outputdf$Bayes_power[15]

    t70c = APowercurve$outputdf$t_power[15]

    test_that("Spot check on Bayesian estimates",{
      expect_lte(abs(bayes70c - 0.99034), 0.07)
    })

    test_that("Spot check on frequentist power estimates",{
      expect_lte(abs(t70c - 0.99249), 0.07)
    })

