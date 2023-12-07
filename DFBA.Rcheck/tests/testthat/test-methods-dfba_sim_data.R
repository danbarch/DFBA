# Sim data methods test


library(vdiffr)

set.seed(77)

object_independent <- dfba_sim_data(n = 450,
                                    model = "pareto",
                                    design = "independent",
                                    delta = 0.4,
                                    block_max = 100)

object_paired <- dfba_sim_data(n = 450,
                                    model = "pareto",
                                    design = "paired",
                                    delta = 0.4,
                                    block_max = 100)
test_that("Show method works",{
  expect_output(show(object_independent))
})

test_that("Plot method works for independent groups",{
  expect_doppelganger(
    title = "sim_data_independent_plot",
    fig = plot(object_independent),
  )
})

test_that("Plot method works for independent groups",{
  expect_doppelganger(
    title = "sim_data_paired_plot",
    fig = plot(object_paired),
  )
})
