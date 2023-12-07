# Median test methods test

# Test data

group1 <- c(12.90, 10.84, 22.67, 10.64, 10.67, 10.79, 13.55, 10.95, 12.19,
            12.76, 10.89, 11.02, 14.27, 13.98, 11.52, 13.49, 11.22, 15.07,
            15.74, 19.00)

group2 <- c(4.63, 58.64, 5.07, 4.66, 4.13, 3.92, 3.39, 3.57, 3.56, 3.39)

set.seed(77)
object <- dfba_median_test(E = group1,
                           C = group2)
test_that("Show method works",{
  expect_output(show(object))
})
