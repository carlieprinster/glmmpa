# Tests for power_blme() ------------------------------------------------
library(tidyr)

#test for binomial distribution
test_that("power_blme for binomial", {
  skip_on_cran()
  skip_if_not_installed("blme")
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("dplyr")

  #binomial dataset
  trt <- as.factor(c(rep(0, 4), rep(1, 4)))
  n <- rep(65, 8)
  pi <- c(rep(0.15, 4), rep(0.25, 4))
  location <- as.factor(rep(1:4, 2))
  expected_y <- (n * pi)
  ex_binomial <- cbind.data.frame(trt, n, pi, location, expected_y)

  #Expand Data
  expanded <- uncount(ex_binomial, weights=n)

  result <- suppressWarnings(suppressMessages(
    power_blme(Formula = cbind((expected_y), n - (expected_y)) ~ 1 + trt + (1 | location) +
                    (1 | location:trt), Varcomp = c(0.02,0.05), Resid_var = NULL, Data = ex_binomial, Expanded_Data = expanded,
                  Family = "binomial", Random_Effects = c("location", "location:trt"), Fixed_Effects = c("trt")  ,Alpha = 0.05, tol = 1e-6,
                  Optimizer = "nloptwrap", OptCtrl = list(maxeval = 2e5), NAGQ0initStep = 0)
  ))


  expect_true(result[1, 1] >= 0.3 && result[1, 1] <= 0.4)
})


#test for poisson distribution
test_that("power_blme for poisson", {
  skip_on_cran()
  skip_if_not_installed("blme")
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("dplyr")

  #poisson dataset
  trt <- as.factor(c(rep(1, 12), rep(2, 12)))
  rate <- as.factor(rep(c(rep(1, 4), rep(2, 4), rep(3, 4)), 2))
  exp_count <- c(rep(10, 4), rep(9, 4), rep(8, 4), rep(9, 4), rep(6, 4), rep(3, 4))
  block <- as.factor(rep(1:4, 6))
  ex_poisson <- cbind.data.frame(trt, rate, exp_count, block)

  result <- suppressWarnings(suppressMessages(
    power_blme(Formula = exp_count ~ rate + trt*rate + (1 | block) + (1 | block:trt), Varcomp = c(0.25,0.15),
                     Resid_var = NULL, Data = ex_poisson, Family = "poisson",
                     Random_Effects = c("block", "block:trt"), Fixed_Effects = c("rate"), Alpha = 0.05,
                     tol = 1e-6, Optimizer = "nloptwrap", OptCtrl = list(maxeval = 2e5), NAGQ0initStep = 0)
  ))



  expect_true(result[1, 1] >= 0.72 && result[1, 1] <= 0.73)
})


