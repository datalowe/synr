context("Functions related to checking if color is in color range")
library(synr)


test_that("is_in_color_range using black color_label", {
  test_color_black <- c(0.03, 0.03, 0.05)
  expect_equal(is_in_color_range(test_color_black,
                                 color_label="black", color_space="sRGB"),
               TRUE)
  test_color_white <- c(0.93, 0.93, 0.95)
  expect_equal(is_in_color_range(test_color_white,
                                 color_label="black", color_space="sRGB"),
               FALSE)
})

test_that("is_in_color_range using r/g/b arguments for black", {
  test_color_black <- c(0.03, 0.03, 0.05)
  expect_equal(is_in_color_range(test_color_black,
                                 r=c(0.0, 0.3),
                                 g=c(0.0, 0.3),
                                 b=c(0.0, 0.3),
                                 color_space="sRGB"),
               TRUE)
  test_color_white <- c(0.93, 0.93, 0.95)
  expect_equal(is_in_color_range(test_color_white,
                                 r=c(0.0, 0.3),
                                 g=c(0.0, 0.3),
                                 b=c(0.0, 0.3),
                                 color_space="sRGB"),
               FALSE)
})

test_that("is_in_color_range using white color_label", {
  test_color_black <- c(0.03, 0.03, 0.05)
  expect_equal(is_in_color_range(test_color_black,
                                 color_label="white", color_space="sRGB"),
               FALSE)
  test_color_white <- c(0.93, 0.93, 0.95)
  expect_equal(is_in_color_range(test_color_white,
                                 color_label="white", color_space="sRGB"),
               TRUE)
})


test_that("is_in_color_range using blue color_label", {
  test_color_blue <- c(0.03, 0.03, 0.98)
  expect_equal(is_in_color_range(test_color_blue,
                                 color_label="blue", color_space="sRGB"),
               TRUE)
  test_color_white <- c(0.93, 0.93, 0.95)
  expect_equal(is_in_color_range(test_color_white,
                                 color_label="blue", color_space="sRGB"),
               FALSE)
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


test_that("Participant.get_prop_color(): proportion of color responses that are
          blue is 0.5 when half of registered responses are blue,
          using r/g/b specifications", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000"), "Luv")

            p <- Participant$new(id='1')
            p$add_grapheme(a)
            expect_equal(p$get_prop_color(r=c(0.0, 0.3), g=c(0.0, 0.3), b=c(0.7, 1)),
                         0.5)
          })


test_that("Participant.get_prop_color(): proportion of color responses that are
          blue is 0.5 when half of registered responses are blue,
          using r/g/b specifications, even if there's a non-valid response", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000", NA), "Luv")

            p <- Participant$new(id='1')
            p$add_grapheme(a)
            expect_equal(p$get_prop_color(r=c(0.0, 0.3), g=c(0.0, 0.3), b=c(0.7, 1)),
                         0.5)
          })


test_that("Participant.get_prop_color(): proportion of color responses that are
          blue is 0.5 when half of registered responses are blue,
          using r/g/b specifications, even if there's a grapheme with all non-valid
          responses", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000", NA), "Luv")
            b <- synr::Grapheme$new(symbol='b')
            b$set_colors(c(NA, NA, NA, NA, NA), "Luv")

            p <- Participant$new(id='1')
            p$add_graphemes(list(a, b))
            expect_equal(p$get_prop_color(r=c(0.0, 0.3), g=c(0.0, 0.3), b=c(0.7, 1)),
                         0.5)
          })


test_that("Participant.get_prop_color(): returns NA if there are no valid response colors", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c(NA, NA, NA, NA, NA), "Luv")
            b <- synr::Grapheme$new(symbol='b')
            b$set_colors(c(NA, NA, NA, NA, NA), "Luv")

            p <- Participant$new(id='1')
            p$add_graphemes(list(a, b))
            expect_true(is.na(p$get_prop_color(r=c(0.0, 0.3), g=c(0.0, 0.3), b=c(0.7, 1))))
          })

