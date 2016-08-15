context("Test generate_block_noise")

test_that("generate_block_noise works properly with character inputs", {
  #library(plyr)

  blockmatrix = cbind(c(1,1,1,1,2,2,2,2), c('a', 'a', 'b', 'b', 'c', 'c', 'd', 'd'), 1:8)
  levs = alply(blockmatrix, 2, unique)
  noiselevels = c(10, 1)
  set.seed(1)
  b1noise = rnorm(2, 0, noiselevels[1])
  b2noise = rnorm(4, 0, noiselevels[2])
  expected = rep(b1noise, each = 4) + rep(b2noise, each = 2)
  set.seed(1)
  output = generate_block_noise(blockmatrix, levs, noiselevels)
  expect_equivalent(expected, output)
})


test_that("generate_block_noise works properly with numeric inputs", {
  #library(plyr)

  blockmatrix = cbind(rep(1:2, each = 4), rep(1:4, each = 2), 1:8)
  levs = alply(blockmatrix, 2, unique)
  noiselevels = c(10, 1)
  set.seed(1)
  b1noise = rnorm(2, 0, noiselevels[1])
  b2noise = rnorm(4, 0, noiselevels[2])
  expected = rep(b1noise, each = 4) + rep(b2noise, each = 2)
  set.seed(1)
  output = generate_block_noise(blockmatrix, levs, noiselevels)
  expect_equivalent(expected, output)
})

test_that("generate_block_noise works non-consecutive levels", {
  #library(plyr)
  blockmatrix = cbind(rep(c(10, 20), each = 4), rep(c('shoe', 'hat', 'cape', 'post'), each = 2), 1:8)
  levs = alply(blockmatrix, 2, unique)
  noiselevels = c(10, 1)
  set.seed(1)
  b1noise = rnorm(2, 0, noiselevels[1])
  b2noise = rnorm(4, 0, noiselevels[2])
  expected = rep(b1noise, each = 4) + rep(b2noise, each = 2)
  set.seed(1)
  output = generate_block_noise(blockmatrix, levs, noiselevels)
  expect_equivalent(expected, output)
})


test_that("generate_block_noise works with non-standard level structures", {
  #library(plyr)
  blockmatrix = cbind(rep(c(10, 20, 30, 40), each = 2), rep(c('shoe', 'hat', 'cape', 'post'), each = 2), 1:8)
  levs = alply(blockmatrix, 2, unique)
  noiselevels = c(10, 1)
  set.seed(1)
  b1noise = rnorm(length(levs[[1]]), 0, noiselevels[1])
  b2noise = rnorm(length(levs[[2]]), 0, noiselevels[2])
  expected = rep(b1noise, each = 2) + rep(b2noise, each = 2)
  set.seed(1)
  output = generate_block_noise(blockmatrix, levs, noiselevels)
  expect_equivalent(expected, output)
})
