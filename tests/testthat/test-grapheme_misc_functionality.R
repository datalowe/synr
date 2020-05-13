context("Grapheme misc. functionality")
library(synr)

test_that("Symbol is set and represented correctly ", {
  g1 <- Grapheme$new(symbol="a")
  expect_equal(g1$symbol, "a")
  g2 <- Grapheme$new()
  g2$set_symbol('b')
  expect_equal(g2$symbol, "b")
})

test_that("Abbreviated symbol representation is correct", {
  g1 <- Grapheme$new(symbol="a")
  expect_equal(g1$get_abbreviated_symbol(), "a")
  g2 <- Grapheme$new()
  g2$set_symbol('Monday')
  expect_equal(g2$get_abbreviated_symbol(), "Mon")
})
