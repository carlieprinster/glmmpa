# Tests for power_glmmTMB() ------------------------------------------------
library(tidyr)

library(lme4)


#test for binomial distribution
test_that("power_glmmTMB for binomial", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
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

  start = list(theta = log(c(sqrt(0.02), sqrt(0.05))))
  map = list(theta = factor(c(NA, NA)))

  ex_binomialmodel <- suppressWarnings(suppressMessages(
    glmmTMB::glmmTMB(pi ~ 1 + trt + (1 | location) + (1 | location:trt),
                     data = expanded,
                     family = binomial(),
                     REML = FALSE,
                     start = start,
                     map = map)
  ))

  result <- suppressWarnings(suppressMessages(
    power_glmmTMB(ex_binomialmodel, "trt", 0.05, expanded)
  ))


  expect_true(result[1, 1] >= 0.3 && result[1, 1] <= 0.4)
})


#test for poisson distribution
test_that("power_glmmTMB for poisson", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
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


  start <- list(theta = log(c(sqrt(0.25), sqrt(0.15))))
  map <- list(theta = factor(c(NA, NA)))

  ex_poissonmodel <- suppressWarnings(suppressMessages(
    glmmTMB::glmmTMB(exp_count ~ rate + trt*rate  + (1|block) + (1|block:trt),
                     data = ex_poisson,
                     family = poisson(),
                     start = start,
                     map = map)
  ))

  result <- suppressWarnings(suppressMessages(
    power_glmmTMB(ex_poissonmodel, "rate", 0.05, ex_poisson)
  ))


  expect_true(result[1, 1] >= 0.72 && result[1, 1] <= 0.73)
})
