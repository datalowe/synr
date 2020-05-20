context("Response times setting and calculating")
library(synr)

test_that("Grapheme response times are actually added", {
  a <- synr::Grapheme$new(symbol='a')
  a$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  a$set_times(c(1.2, 8, 3.5, 6))
  expect_equal(length(a$response_times), 4)
})

test_that("Grapheme get_mean_response_time calculates correct mean", {
  a <- synr::Grapheme$new(symbol='a')
  a$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  a$set_times(c(1.2, 8, 3.5, 6))
  expect_equal(a$get_mean_response_time(), 4.675)
})

test_that("Grapheme get_mean_response_time calculates correct mean, even if
          the grapheme doesn't have any response colors set", {
  a <- synr::Grapheme$new(symbol='a')
  a$set_times(c(1.2, 8, 3.5, 6))
  expect_equal(a$get_mean_response_time(), 4.675)
})

test_that("Grapheme get_mean_response_time returns NA when no valid times are set", {
  b <- synr::Grapheme$new(symbol='b')
  b$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  num_na_vector <- as.numeric(c(NA, NA, NA, NA))
  b$set_times(num_na_vector)
  expect_true(is.na(b$get_mean_response_time()) )
})

test_that("Grapheme get_mean_response_time returns NA when no times at all are set", {
  b <- synr::Grapheme$new(symbol='b')
  b$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  expect_error(b$get_mean_response_time())
})

test_that("Participant get_mean_response_time calculates correct mean for one grapheme", {
  a <- synr::Grapheme$new(symbol='a')
  a$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  a$set_times(c(1.2, 8, 3.5, 6))
  p <- Participant$new()
  p$add_grapheme(a)
  expect_equal(p$get_mean_response_time(), 4.675)
})

test_that("Participant get_mean_response_time calculates correct mean for three graphemes", {
  a <- synr::Grapheme$new(symbol='a')
  a$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  a$set_times(c(1.2, 8, 3.5, 6))

  b <- synr::Grapheme$new(symbol='b')
  b$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  b$set_times(c(40, 1, 7, 3.2))

  g <- synr::Grapheme$new(symbol='g')
  g$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g$set_times(c(20, 8, 10, 14))

  p <- Participant$new()
  p$add_grapheme(a)
  p$add_grapheme(b)
  p$add_grapheme(g)

  expect_lt(p$get_mean_response_time()-10.15833, 0.01)
})

test_that("Participant get_mean_response_time raises error when no graphemes have been added", {
  p <- Participant$new()
  expect_error(p$get_mean_response_time())
})
