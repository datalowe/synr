context("Color setting and description")
library(synr)

test_that("Only non NA colors are counted as such", {
  a <- synr::Grapheme$new(symbol='a')
  a$set_colors(c("#101010", NA, NA, "#DDBBAA", NA), "Luv")
  expect_equal(a$get_num_non_na_colors(), 2)
  b <- synr::Grapheme$new(symbol='b')
  b$set_colors(c(NA), "Luv")
  expect_equal(b$get_num_non_na_colors(), 0)
  foo <- synr::Grapheme$new(symbol='foo')
  foo$set_colors(c("#880033"), "Luv")
  expect_equal(foo$get_num_non_na_colors(), 1)
})

test_that("has_only_non_na() returns TRUE when this is the case", {
  foo <- synr::Grapheme$new(symbol='foo')
  foo$set_colors(c("#880033"), "Luv")
  expect_true(foo$has_only_non_na_colors())
})

test_that("Color space is correctly stored", {
  a <- synr::Grapheme$new(symbol='a')
  a$set_colors(c("#101010", NA, NA, "#DDBBAA", NA), "Luv")
  expect_equal(a$color_space, "Luv")
})

test_that("Proportion of color responses that are
          blue is 1 when all registered responses are blue", {
  a <- synr::Grapheme$new(symbol='a')
  a$set_colors(c("#0000DD", "#0000DE", "#0000FF", "#0000FF", "#3333DD"), "Luv")
  expect_equal(a$get_prop_color(color_label="blue"), 1)
})

test_that("Proportion of color responses that are
          blue is 0 when no registered responses are blue", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#000000", "#AA0000", "#00BB00", "#000077", "#AADD11"), "Luv")
            expect_equal(a$get_prop_color(color_label="blue"), 0)
          })

test_that("Proportion of color responses that are
          blue is 0.5 when half of registered responses are blue,
          using r/g/b specifications", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000"), "Luv")
            expect_equal(a$get_prop_color(r=c(0.0, 0.3), g=c(0.0, 0.3), b=c(0.7, 1)),
                                          0.5)
          })

test_that("Conversion to Luv and back to hex code using get_hex_colors() works", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000"), "Luv")
            reconverted_colors <- a$get_hex_colors()
            expect_equal(reconverted_colors[1], "#0000DD")
            expect_equal(reconverted_colors[2], "#0000FF")
            expect_equal(reconverted_colors[3], "#00FF00")
            expect_equal(reconverted_colors[4], "#CC0000")
          })
